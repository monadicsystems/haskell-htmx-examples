{-# LANGUAGE OverloadedStrings #-}

module View.Snippet.ArticleDetail where

import Lucid
import Lucid.HTMX
import Model
import View.Snippet.CommentForest


data ArticleDetail = ArticleDetail Article CommentForest

instance ToHtml ArticleDetail where
    toHtml (ArticleDetail article commentForest) = do
        h1_ [] "This is an article"
        toHtml commentForest
