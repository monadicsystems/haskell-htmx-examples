{-# LANGUAGE OverloadedStrings #-}

module View.Page.ArticleForm where

import Data.Text
import Lucid
import Lucid.Alpine
import Lucid.HTMX
import Lucid.Supplemental hiding (svg_)
import View.Snippet.Navbar

data ArticleForm = ArticleForm

instance ToHtml ArticleForm where
  toHtml ArticleForm = do
    toHtml Navbar
    div_ [class_ "flex items-center justify-center my-32 mx-4 md:mx-10 lg:mx-64"] $
      form_ [class_ "w-full", action_ "#"] $ do
        div_ $ do
          div_ [class_ "flex items-center", ariaOrientation_ "horizontal", role_ "tablist"] $ do
            button_ [id_ "tabs-1-tab-1", class_ "text-gray-500 hover:text-gray-900 bg-white hover:bg-gray-100 px-3 py-1.5 border border-transparent text-sm font-medium rounded-md", ariaControls_ "tabs-1-panel-1", role_ "tab", type_ "button"] $ "Write"
            button_ [id_ "tabs-1-tab-2", class_ "text-gray-500 hover:text-gray-900 bg-white hover:bg-gray-100 ml-2 px-3 py-1.5 border border-transparent text-sm font-medium rounded-md", ariaControls_ "tabs-1-panel-2", role_ "tab", type_ "button"] $ "Preview"
            div_ [class_ "ml-auto flex items-center space-x-5"] $ do
              div_ [class_ "flex items-center"] $
                button_ [type_ "button", class_ "-m-2.5 w-10 h-10 rounded-full inline-flex items-center justify-center text-gray-400 hover:text-gray-500"] $ do
                  span_ [class_ "sr-only"] "Insert link"
                  svg_ [class_ "h-5 w-5", xmlns_ "http://www.w3.org/2000/svg", viewBox_ "0 0 20 20", fill_ "currentColor", ariaHidden_ "true"] $ path_ [fillRule_ "evenodd", d_ "M12.586 4.586a2 2 0 112.828 2.828l-3 3a2 2 0 01-2.828 0 1 1 0 00-1.414 1.414 4 4 0 005.656 0l3-3a4 4 0 00-5.656-5.656l-1.5 1.5a1 1 0 101.414 1.414l1.5-1.5zm-5 5a2 2 0 012.828 0 1 1 0 101.414-1.414 4 4 0 00-5.656 0l-3 3a4 4 0 105.656 5.656l1.5-1.5a1 1 0 10-1.414-1.414l-1.5 1.5a2 2 0 11-2.828-2.828l3-3z", clipRule_ "evenodd"]
              div_ [class_ "flex items-center"] $
                button_ [type_ "button", class_ "-m-2.5 w-10 h-10 rounded-full inline-flex items-center justify-center text-gray-400 hover:text-gray-500"] $ do
                  span_ [class_ "sr-only"] "Insert code"
                  svg_ [class_ "h-5 w-5", xmlns_ "http://www.w3.org/2000/svg", viewBox_ "0 0 20 20", fill_ "currentColor", ariaHidden_ "true"] $ path_ [fillRule_ "evenodd", d_ "M12.316 3.051a1 1 0 01.633 1.265l-4 12a1 1 0 11-1.898-.632l4-12a1 1 0 011.265-.633zM5.707 6.293a1 1 0 010 1.414L3.414 10l2.293 2.293a1 1 0 11-1.414 1.414l-3-3a1 1 0 010-1.414l3-3a1 1 0 011.414 0zm8.586 0a1 1 0 011.414 0l3 3a1 1 0 010 1.414l-3 3a1 1 0 11-1.414-1.414L16.586 10l-2.293-2.293a1 1 0 010-1.414z", clipRule_ "evenodd"]
              div_ [class_ "flex items-center"] $
                button_ [type_ "button", class_ "-m-2.5 w-10 h-10 rounded-full inline-flex items-center justify-center text-gray-400 hover:text-gray-500"] $ do
                  span_ [class_ "sr-only"] "Mention someone"
                  svg_ [class_ "h-5 w-5", xmlns_ "http://www.w3.org/2000/svg", viewBox_ "0 0 20 20", fill_ "currentColor", ariaHidden_ "true"] $ path_ [fillRule_ "evenodd", d_ "M14.243 5.757a6 6 0 10-.986 9.284 1 1 0 111.087 1.678A8 8 0 1118 10a3 3 0 01-4.8 2.401A4 4 0 1114 10a1 1 0 102 0c0-1.537-.586-3.07-1.757-4.243zM12 10a2 2 0 10-4 0 2 2 0 004 0z", clipRule_ "evenodd"]
          div_ [class_ "mt-2"] $ do
            div_ [id_ "tabs-1-panel-1", class_ "p-0.5 -m-0.5 rounded-lg", ariaLabelledby_ "tabs-1-tab-1", role_ "tabpanel", tabindex_ "0"] $ do
              label_ [for_ "comment", class_ "sr-only"] "Comment"
              div_ $ textarea_ [rows_ "5", name_ "comment", id_ "comment", class_ "shadow-sm focus:ring-indigo-500 focus:border-indigo-500 block w-full sm:text-sm border-gray-300 rounded-md", placeholder_ "Add your comment..."] ""
            div_ [id_ "tabs-1-panel-2", class_ "p-0.5 -m-0.5 rounded-lg", ariaLabelledby_ "tabs-1-tab-2", role_ "tabpanel", tabindex_ "0"] $ div_ [class_ "border-b"] $ div_ [class_ "mx-px mt-px px-3 pt-2 pb-12 text-sm leading-5 text-gray-800"] "Preview content will render here."
        div_ [class_ "mt-2 flex justify-end"] $ button_ [type_ "submit", class_ "inline-flex items-center px-4 py-2 border border-transparent text-sm font-medium rounded-md shadow-sm text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"] "Post"
