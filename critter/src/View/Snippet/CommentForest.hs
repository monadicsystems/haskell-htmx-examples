{-# LANGUAGE OverloadedStrings #-}

module View.Snippet.CommentForest where

import Lucid
import Lucid.HTMX
import Model

import Data.Graph

newtype CommentForest = CommentForest (Forest Comment)

instance ToHtml CommentForest where
    toHtml (CommentForest commentForest) = do
        h1_ [] "This is a comment forest"
