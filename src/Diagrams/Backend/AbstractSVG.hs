{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NondecreasingIndentation   #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
  -- UndecidableInstances needed for ghc < 707

----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.AbstractSVG
-- Copyright   :  (c) 2011-2015 diagrams-svg team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--

-- TODO: Docs...

module Diagrams.Backend.AbstractSVG
  ( AbSVG(..) -- rendering token
  , Render(..)
  , B
  -- , Environment
    -- for rendering options specific to SVG
  , Options(..), sizeSpec, svgDefinitions, idPrefix, svgAttributes
  , SVGFloat
  , SvgRenderM
  , SvgRenderState

  ) where

#if __GLASGOW_HASKELL__ < 710
import           Data.Foldable            as F (foldMap)
#endif
import qualified Debug.Trace as Dbg
import qualified Data.Text                as T
import           Data.Tree

-- from base
-- import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Typeable

-- from containers
import           Data.HashMap.Strict      (fromListWith)

-- from lens
import           Control.Lens             hiding (transform, ( # ))

-- from diagrams-core
import           Diagrams.Core.Compile
import           Diagrams.Core.Types      (Annotation (..))

-- from diagrams-lib
import           Diagrams.Prelude         hiding (Attribute, size, view, local)
import           Diagrams.TwoD.Adjust     (adjustDia2D)
import           Diagrams.TwoD.Attributes (splitTextureFills)
import           Diagrams.TwoD.Path       (Clip (Clip))
import           Diagrams.TwoD.Text

-- from svg-builder
import           Graphics.Svg.Attributes
import           Graphics.Svg.Path (toText)

-- from this package
import           Graphics.Rendering.AbstractSVG
import           Graphics.Svg.Abstract.Elements (Attributes
                                                ,Element(..)
                                                ,Tag(A,Defs,G))


-----------------------
-- Graphics.Svg.Element
-----------------------
a_, g_ :: [(AttrTag, [T.Text])] -> Element
a_ = El A . fromListWith (<>)
g_ = El G . fromListWith (<>)

-- | @AbSVG@ is simply a token used to identify this rendering backend
--   (to aid type inference).
data AbSVG = AbSVG
  deriving (Show, Typeable)

type B = AbSVG

type instance V AbSVG = V2
type instance N AbSVG = Double

instance Show (RNode b v n a) where
  show (RPrim _) = "RPrim"
  show (RAnnot _) = "RAnnot"
  show REmpty = "REmpty"
  show (RStyle s) = "RStyle " ++ show s

-- data Environment n = Environment
--   { _style :: Style V2 n
--   , __pre :: T.Text
--   }

-- makeLenses ''Environment

data SvgRenderState n = SvgRenderState
  { _clipPathId :: Int
  , _fillGradId :: Int
  , _lineGradId :: Int
  , _style      :: Style V2 n
  , __pre       :: T.Text
  }

makeLenses ''SvgRenderState

-- initialEnvironment :: SVGFloat n => T.Text -> Environment n
-- initialEnvironment = Environment (mempty # recommendFillColor transparent)
-- -- Fill gradients ids are even, line gradient ids are odd.
initialSvgRenderState :: (Typeable n, Floating n) => T.Text -> SvgRenderState n
initialSvgRenderState = SvgRenderState 0 0 1 (mempty # recommendFillColor transparent)

-- | Monad to keep track of environment and state when rendering an SVG.
type SvgRenderM n = -- ReaderT (Environment n) (State SvgRenderState)
                    State (SvgRenderState n)

runRenderM :: SVGFloat n => T.Text -> SvgRenderM n a -> a
runRenderM o = flip evalState $ initialSvgRenderState o
               -- $ runReaderT  s (initialEnvironment o)

-- Handle clip attributes.
--
renderSvgWithClipping :: forall n. SVGFloat n
                      => T.Text
                      -> Tree Element       -- ^ Input SVG
                      -> Style V2 n         -- ^ Styles
                      -> SvgRenderM n DefTree -- ^ Resulting svg

renderSvgWithClipping prefix svg s =
  case op Clip <$> getAttr s of
    Nothing    -> return $ DefTree [] [svg]
    Just paths -> renderClips paths
  where
    renderClips :: [Path V2 n] -> SvgRenderM n DefTree
    renderClips []     = return $ DefTree [] [svg]
    renderClips (p:ps) = do
      clipPathId += 1
      ident <- use clipPathId
      renderClip p prefix ident <$> renderClips ps

-- | Create a new texture defs svg element using the style and the current
--   id number, then increment the gradient id number.
fillTextureDefs :: SVGFloat n => Style v n
                              -> SvgRenderM n DefTree
fillTextureDefs s = do
  ident <- use fillGradId
  fillGradId += 2 -- always even
  return $ renderFillTextureDefs ident s

lineTextureDefs :: SVGFloat n => Style v n
                              -> SvgRenderM n DefTree
lineTextureDefs s = do
  ident <- use lineGradId
  lineGradId += 2 -- always odd
  return $ renderLineTextureDefs ident s

instance SVGFloat n => Backend AbSVG V2 n where
  newtype Render  AbSVG V2 n = R { unR :: SvgRenderM n (Tree Element)}
  type    Result  AbSVG V2 n = Tree Element
  data    Options AbSVG V2 n = SVGOptions
    { _size            :: SizeSpec V2 n   -- ^ The requested size.
    , _svgDefinitions  :: Forest Element
                          -- ^ Custom definitions that will be added to the @defs@
                          --   section of the output.
    , _idPrefix        :: T.Text
    , _svgAttributes   :: Attributes
                          -- ^ Attributes to apply to the entire svg element.
    }

  renderRTree :: AbSVG -> Options AbSVG V2 n -> RTree AbSVG V2 n Annotation -> Result AbSVG V2 n
  renderRTree _ opts rt = Dbg.traceShow rt $ runRenderM (opts ^.idPrefix) svgOutput
    where
      svgOutput :: SvgRenderM n (Tree Element)
      svgOutput = do
        let V2 w h = specToSize 100 (opts^.sizeSpec)

        svg <- rtree (splitTextureFills rt)

        return $ svgHeader w h (opts^.svgDefinitions)
                               (opts^.svgAttributes)
                               svg

  adjustDia c opts d = ( sz, t <> reflectionY, d' ) where
    (sz, t, d') = adjustDia2D sizeSpec c opts (d # reflectY)

-- We'd like to traverse the tree, generating svg elements from RNodes. But prim
-- nodes render to trees, not Elements, which means that rendering an RPrim node
-- changes the shape of the tree. So instead we leave a bud for each primitive
-- while traversing, and then grow the buds into subtrees. This is safe to do
-- because diagrams promises that RPrim nodes have no children, and so we can
-- substitute in whatever children render gives us.

-- Meanwhile, RStyle nodes don't create SVG; instead they alter the rendering
-- environment for child nodes. So they go to the left, too; as do REmpty nodes,
-- which are empty

rtree :: forall n. Typeable n => RTree AbSVG V2 n Annotation -> SvgRenderM n [Tree Element]
rtree = fmap budbreak' . traverse rnode
  where
    rnode :: RNode AbSVG V2 n Annotation -> SvgRenderM n (Either (Maybe (Tree Element)) Element)
    rnode (RPrim p) = Left . Just <$> unR (render AbSVG p)
    rnode (RStyle sty) = (style %= (<> sty)) >> return (Left Nothing)
    rnode (RAnnot (OpacityGroup o)) = return . Right $ g_ [(Opacity_,[toText o])]
    rnode (RAnnot (Href uri)) = return . Right $ a_ [(XlinkHref_,[T.pack uri])]
    rnode (REmpty) = return $ Left Nothing

    budbreak :: a -> Forest (Either (Maybe (Tree a)) a) -> Forest a
    budbreak e (Node (Right v)       ts : tss) = Node v (budbreak e ts) : budbreak e tss
    budbreak e (Node (Left (Just t)) [] : tss) = t : budbreak e tss
    budbreak _ (Node (Left (Just _)) _  : _)   = error "diagrams-svgdom: RPrim nodes shouldn't have children"
    budbreak e (Node (Left Nothing)  [] : tss) = budbreak e tss
    budbreak e (Node (Left Nothing)  ts : tss) = Node e (budbreak e ts) : budbreak e tss
    budbreak _ []                              = []

    budbreak' = budbreak (El G mempty) . pure

    -- enstyle :: (MonadReader r m, Applicative f) =>
    --            Tree (Either (r -> r) (f Element)) -> m (Tree (f Element))
    -- enstyle (Node (Right v) ts) = (Node v) <$> cattrees ts
    -- enstyle (Node (Left f) ts)  = local f $ Node (pure $ El G mempty) <$> (cattrees ts)

    -- cattrees :: MonadReader r m => [Tree (Either (r -> r) a)] -> m [Tree a]
    -- cattrees ((Node (Right v) ts):tss) = do
    --   ts'  <- cattrees ts
    --   tss' <- cattrees tss
    --   return $ (Node v ts') : tss'
    -- cattrees ((Node (Left f) ts):tss)  = do
    --   ts'  <- local f $ cattrees ts
    --   tss' <- cattrees tss
    --   return $ ts' ++ tss'
    -- cattrees [] = return []

-- rtree' :: forall n. Typeable n => RTree AbSVG V2 n Annotation -> Render AbSVG V2 n
-- rtree' (Node (RPrim p) []) = render AbSVG p
-- rtree' (Node (RStyle sty) ts) = R . local (style %~ (<> sty)) $ Node (El G mempty) <$> traverse (unR . rtree') ts

-- rtree (Node (RPrim p) _)  = error "diagrams-svgdom: invalid RPrim node"

-- | Lens onto the size of the svg options.
sizeSpec :: Lens' (Options AbSVG V2 n) (SizeSpec V2 n)
sizeSpec f opts = f (_size opts) <&> \s -> opts { _size = s }

-- | Lens onto the svg definitions of the svg options.
svgDefinitions :: Lens' (Options AbSVG V2 n) (Forest Element)
svgDefinitions f opts =
  f (_svgDefinitions opts) <&> \ds -> opts { _svgDefinitions = ds }

-- | Lens onto the idPrefix of the svg options. This is the prefix given
--   to clipping paths to distinguish them from other svg files in the
--   same web page.
idPrefix :: Lens' (Options AbSVG V2 n) T.Text
idPrefix f opts = f (_idPrefix opts) <&> \i -> opts { _idPrefix = i }

-- | Lens onto the svgAttributes field of the svg options. This field
--   is provided to supply SVG attributes to the entire diagram.
svgAttributes :: Lens' (Options AbSVG V2 n) Attributes
svgAttributes f opts =
  f (_svgAttributes opts) <&> \ds -> opts { _svgAttributes = ds }


-- paths ---------------------------------------------------------------

attributedRender :: SVGFloat n => Tree Element -> SvgRenderM n (Tree Element)
attributedRender svg = do
  SvgRenderState _idClip idFill idLine sty preT <- get
  -- Environment sty preT <- ask
  clippedSvg   <- renderSvgWithClipping preT svg sty
  lineGradDefs <- lineTextureDefs sty
  fillGradDefs <- fillTextureDefs sty
  let DefTree ds cs = lineGradDefs <> fillGradDefs <> clippedSvg
      defs = Node (El Defs mempty) ds
  return $ Node (g_ (renderStyles idFill idLine sty)) (defs:cs)

entree :: Maybe Element -> Tree Element
entree (Just e) = Node e []
entree Nothing  = Node (El G mempty) [] -- Meaningless empty grouping

instance SVGFloat n => Renderable (Path V2 n) AbSVG where
  render _ = R . attributedRender . entree . renderPath

instance SVGFloat n => Renderable (Text n) AbSVG where
  render _ = R . attributedRender . (\e -> Node e []) . renderText

instance SVGFloat n => Renderable (DImage n Embedded) AbSVG where
  render _ = R . return . flip Node [] . renderDImageEmb


-- instance Hashable n => Hashable (Options AbSVG V2 n) where
--   hashWithSalt s  (SVGOptions sz defs ia sa gd) =
--     s  `hashWithSalt`
--     sz `hashWithSalt`
--     ds `hashWithSalt`
--     ia `hashWithSalt`
--     sa `hashWithSalt`
--       where ds = fmap renderBS defs
