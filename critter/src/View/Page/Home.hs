{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module View.Page.Home
  ( Home (..),
  )
where

import Data.String.QQ
import Lucid
import Lucid.HTMX
import Lucid.Supplemental hiding (svg_)
import Model
import View.Snippet.Feednav
import View.Snippet.Navbar
import View.Snippet.ArticleList

data Home = Home ArticleList

instance ToHtml Home where
  toHtml (Home articleList) = do
      toHtml Navbar
      div_ [class_ "flex flex-col lg:flex-row h-screen"] $ do
        div_ [class_ "flex flex-row lg:hidden m-2"] $ do
          aside_ [class_ "bg-blue-500 w-full mr-2 rounded-md"] $ h1_ [] "Search and tags go here"
          a_
            [ hxBoost_ "true"
            , href_ "/new_article"
            , type_ "button"
            , class_ "inline-flex items-center px-4 py-2 text-sm border border-transparent shadow-sm font-medium rounded-md text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 block sm:hidden"
            ] $ do
            "Add Post"
            svg_ [ xmlns_ "http://www.w3.org/2000/svg", class_ "h-10 w-10 -mr-1", fill_ "none", ariaHidden_ "true", viewBox_ "0 0 24 24", stroke_ "currentColor" ] $ path_ [ strokeLinecap_ "round", strokeLinejoin_ "round", strokeWidth_ "2", d_ "M12 9v3m0 0v3m0-3h3m-3 0H9m12 0a9 9 0 11-18 0 9 9 0 0118 0z" ]
        div_ [class_ "grow"] $ do
          toHtml Feednav
          toHtml articleList
        aside_ [class_ "bg-blue-500 h-1/3 w-1/4 m-2 rounded-md hidden lg:block"] $ do
          h1_ [] "Search and tags go here"
