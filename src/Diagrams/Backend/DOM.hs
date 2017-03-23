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
{-# LANGUAGE OverloadedLists            #-}
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

module Diagrams.Backend.DOM
  ( renderDom
  , createTree
  , domSvg
  ) where

-- import           Control.Arrow
import           Data.Map                 (Map)
import qualified Data.Text                as T
import           Data.Tree                (Tree)
import qualified Data.Tree                as T

-- from base
import           Control.Monad.Reader

-- from lens
import           Control.Lens             hiding (transform, ( # ))

-- from diagrams-core
import           Diagrams.Core.Compile

-- from diagrams-lib
import           Diagrams.Prelude         hiding (Attribute, size, view, local)

-- from this package
import           Graphics.Svg.Attributes (tag2text)
import           Diagrams.Backend.AbstractSVG hiding (B)
import           Graphics.Svg.Abstract.Elements (Tag)
import qualified Graphics.Svg.Abstract.Elements as E

-- from ghcjs-dom
import           GHCJS.DOM.Document hiding (error)
import           GHCJS.DOM.Element (setAttribute)
import           GHCJS.DOM.Node
import           GHCJS.DOM.Types as DOM hiding (Text)

domSvg :: (MonadJSM m, IsDocument d) => E.Element -> ReaderT d m DOM.Element
domSvg (E.El t attrs) = do
    d <- ask
    e <- lift $ createElem d t
    itraverse_ (setAttribute' e) attrs
    itraverse_ (setAttribute e) attrs'
    case t of
      E.Text txt -> do
        tn <- createTextNode d txt
        lift (appendChildUnsafe e tn)
        return e
      _ -> return e
  where
    ns :: JSString
    ns = "http://www.w3.org/2000/svg"

    createElem d = createElementNSUnsafe d (Just ns) . Just . tagString

    attrs' :: Map JSString JSString
    attrs' = case t of
      E.Svg11 -> [("xmlns", "http://www.w3.org/2000/svg"), ("version","1.1")]
      _ -> []

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
    tagString E.Svg11          = "svg"
    tagString (E.Text _)       = "text"
    tagString e                = error $ "Not yet implemented" ++ (show e)

    setAttribute' e a vs = setAttribute e (tag2text a) (T.intercalate " " vs)

renderDom :: (MonadJSM m, IsDocument d, SVGFloat n, Monoid a, Semigroup a)
          => Options AbSVG V2 n -> QDiagram AbSVG V2 n a -> ReaderT d m DOM.Element
renderDom o = fmap (T.rootLabel) . createTree . renderDia AbSVG o

createTree :: (MonadJSM m, IsDocument d) => Tree E.Element -> ReaderT d m (Tree DOM.Element)
createTree (T.Node e ts) = do
  e'  <- domSvg e
  ts' <- traverse createTree ts
  _ <- traverse (appendChildUnchecked e' . Just . T.rootLabel) ts'
  return $ T.Node e' ts'
