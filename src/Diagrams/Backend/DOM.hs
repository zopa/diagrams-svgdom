{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
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

----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.DOM
-- Copyright   :  (c) 2017 Ben Doyle
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  benjamin.peter.doyle@gmail.com
--

-- TODO: Docs...

module Diagrams.Backend.DomSVG
  ( DomSvg(..) -- rendering token
  , B
    -- for rendering options specific to SVG
  , Options(..) -- , sizeSpec, svgDefinitions, idPrefix, svgAttributes
  , SVGFloat

  ) where

import qualified Data.Text                as T
import           Data.Tree                (Tree)
import qualified Data.Tree                as T

-- from base
import           Control.Monad.Reader
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
import           Graphics.Svg             (tag2text)

-- from diagrams-svg package
import           Diagrams.Backend.AbstractSVG hiding (B)
-- import           Graphics.Rendering.AbstractSVG
import           Graphics.Svg.Abstract.Elements (Attributes,Tag)

import qualified Graphics.Svg.Abstract.Elements as E
-- from ghcjs-dom
import           GHCJS.DOM
import           GHCJS.DOM.Document hiding (error)
import           GHCJS.DOM.Element (setAttribute)
import           GHCJS.DOM.Node
import           GHCJS.DOM.Types hiding (Text)

-- | @DomSvg@ is simply a token used to identify this rendering backend
--   (to aid type inference).
data DomSvg = DomSvg
  deriving (Show, Typeable)

type B = DomSvg

type instance V DomSvg = V2
type instance N DomSvg = Double

-- data SvgE = forall a. ( IsSVGElement a
--                       , IsSVGGraphicsElement a
--                       , IsElement a
--                       , IsNode a
--                       , IsEventTarget a
--                       , IsGObject a
--                       ) => SvgE a
-- -- type SvgTree = Tree SvgE

type SRT n m = ReaderT (Environment n) (StateT SvgRenderState m)

primconv :: (Renderable p AbSVG, Renderable p DomSvg) => Prim AbSVG V2 n -> Prim DomSvg V2 n
primconv (Prim p) = Prim p

instance (Transformable r, Renderable r AbSVG, V r ~ V2) => Renderable r DomSvg where
  render _ r = DR $ do
    let R sr = render AbSVG r
    t <- lift $ transrend sr
    createTree t

transrend :: Monad m => SvgRenderM n a -> SRT n m a
transrend c = ReaderT $ \e -> StateT $ \s ->
  return . runIdentity $ runStateT (runReaderT c e) s


instance SVGFloat n => Backend DomSvg V2 n where
  newtype Render  DomSvg V2 n = DR (ReaderT Document (SRT n JSM) Node)
  type    Result  DomSvg V2 n = ReaderT Document JSM DocumentFragment
  newtype Options DomSvg V2 n = DomOptions (Options AbSVG V2 n)
    -- { _size            :: SizeSpec V2 n   -- ^ The requested size.
    -- , _svgDefinitions  :: Forest Element
    --                       -- ^ Custom definitions that will be added to the @defs@
    --                       --   section of the output.
    -- , _idPrefix        :: T.Text
    -- , _svgAttributes   :: Attributes
    --                       -- ^ Attributes to apply to the entire svg element
    -- }

  renderRTree :: DomSvg -> Options DomSvg V2 n -> RTree DomSvg V2 n Annotation -> Result DomSvg V2 n
  renderRTree _ (DomOptions opts) rt = do
    d <- ask
    df <- newDocumentFragment d
    let t = renderRTree AbSVG opts rt
    appendChildUnsafe df $ Just t

  -- adjustDia c opts d =
  --   ( sz, t <> reflectionY, d' ) where
  --   (sz, t, d') = adjustDia2D sizeSpec c opts (d # reflectY)

-- (b -> a -> m b) -> t a -> m b
domSvg :: (MonadJSM m, IsDocument d) => E.Element -> ReaderT d m Node
domSvg (E.El t attrs) = do
    d <- ask
    e <- lift . createElementUnsafe d . Just $ tagString t
    itraverse_ (setAttribute' e) attrs
    case t of
      E.Text t -> do
        tn <- createTextNode d t
        lift (appendChildUnsafe e tn)
      _ -> return . toNode $ e
  where
    tagString :: Tag -> JSString
    tagString E.A              = "a"
    tagString E.ClipPath       = "clipPath"
    tagString E.Defs           = "defs"
    tagString E.G              = "g"
    tagString E.Image          = "image"
    tagString E.LinearGradient = "linearGradient"
    tagString E.RadialGradient = "radialGradient"
    tagString E.Path           = "path"
    tagString E.Stop           = "stop"
    tagString (E.Text _)       = "text"
    tagString _                = error "Not yet implemented"

    setAttribute' e a vs = setAttribute e (tag2text a) (T.intercalate " " vs)

createTree :: (MonadJSM m, IsDocument d) => Tree E.Element -> ReaderT d m Node
createTree (T.Node e []) = domSvg e
createTree (T.Node e ts) = do
  el  <- domSvg e
  ts' <- traverse createTree ts
  _ <- traverse (appendChildUnchecked el . Just) ts'
  return el

-- instance SVGFloat n => Renderable (Path V2 n) DomSvg where
--   render _ = R . attributedRender . entree . renderPath

-- instance SVGFloat n => Renderable (Text n) DomSvg where
--   render _ = R . attributedRender . (\e -> Node e []) . renderText

-- instance SVGFloat n => Renderable (DImage n Embedded) DomSvg where
--   render _ = R . return . flip Node [] . renderDImageEmb

