{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module View.Snippet.ArticleList where

import Lucid
import Lucid.HTMX
import Model
import View.Snippet.CommentForest
import Data.Time
import Data.Text

data ArticleList = ArticleList [ArticleListItem]
-- data ArticleList = ArticleList (Rows Article)
-- data ArticleList = ArticleList (Grid Article)

instance ToHtml ArticleList where
  toHtml (ArticleList articles) = do
    div_ [class_ "m-2"] $ do
      h1_ [] "This is an articleList"

data ArticleListItem = ArticleListItem
  { articleListItemAuthor :: Text
  , articleListItemTitle :: Text
  , articleListItemCreatedAt :: UTCTime
  , articleListItemTopics :: [ArticleListItemTopic]
  } deriving (Eq, Show)

data ArticleListItemTopic = ArticleListItemTopic
  { articleListItemTopicColor :: Text
  , articleListItemTopicName :: Text
  } deriving (Eq, Show)

instance ToHtml ArticleListItemTopic where
  toHtml (ArticleListItemTopic color name) = do
    undefined

instance ToHtml ArticleListItem where
  toHtml (ArticleListItem author title createdAt topics) = do
    undefined
