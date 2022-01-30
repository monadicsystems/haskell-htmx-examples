{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}


module View.Snippet.Feednav where

import Data.Text
import Lucid
import Lucid.Supplemental hiding (svg_)
import Lucid.HTMX
import Lucid.Alpine


data Feednav = Feednav

instance ToHtml Feednav where
    toHtml _ = do
        toHtmlRaw @Text "<!--\n  This example requires Tailwind CSS v2.0+ \n  \n  This example requires some changes to your config:\n  \n  ```\n  // tailwind.config.js\n  module.exports = {\n    // ...\n    plugins: [\n      // ...\n      require('@tailwindcss/forms'),\n    ],\n  }\n  ```\n -->"
        div_ [ class_ "relative pb-5 border-b border-gray-200 sm:pb-0 m-2" ] $ do
            div_ [ class_ "sm:flex sm:items-center sm:justify-between" ] $ do
                div_ [ class_ "hidden sm:flex sm:mt-4 sm:mt-0 sm:absolute sm:top-0 sm:right-0" ] $ do
                    a_
                        [ hxBoost_ "true"
                        , href_ "/new_article"
                        , type_ "button"
                        , class_ "inline-flex items-center px-6 py-3 border border-transparent shadow-sm text-base font-medium rounded-md text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                        ] $ do
                        "Add Post"
                        svg_ [ xmlns_ "http://www.w3.org/2000/svg", class_ "h-5 w-5 ml-3 -mr-1", fill_ "none", ariaHidden_ "true", viewBox_ "0 0 24 24", stroke_ "currentColor" ] $ path_ [ strokeLinecap_ "round", strokeLinejoin_ "round", strokeWidth_ "2", d_ "M12 9v3m0 0v3m0-3h3m-3 0H9m12 0a9 9 0 11-18 0 9 9 0 0118 0z" ]
            div_ [ class_ "mt-4" ] $ do
                toHtmlRaw @Text "<!-- Dropdown menu on small screens -->"
                div_ [ class_ "sm:hidden" ] $ do
                    label_ [ for_ "current-tab", class_ "sr-only" ] "Select a tab"
                    select_ [ id_ "current-tab", name_ "current-tab", class_ "block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md" ] $ do
                        option_ "Popular"
                        option_ "Trending"
                        option_ [ selected_ "" ] "New"
                toHtmlRaw @Text "<!-- Tabs at small breakpoint and up -->"
                div_ [ class_ "hidden sm:block" ] $ nav_ [ class_ "flex space-x-8" ] $ do
                    toHtmlRaw @Text "<!-- Current: \"border-indigo-500 text-indigo-600\", Default: \"border-transparent text-gray-500 hover:text-gray-700 hover:border-gray-300\" -->"
                    a_ [ href_ "#", class_ "border-transparent text-gray-500 hover:text-gray-700 hover:border-gray-300 whitespace-nowrap pb-4 px-1 border-b-2 font-medium text-sm" ] "Popular"
                    a_ [ href_ "#", class_ "border-transparent text-gray-500 hover:text-gray-700 hover:border-gray-300 whitespace-nowrap pb-4 px-1 border-b-2 font-medium text-sm" ] "Trending"
                    a_ [ href_ "#", class_ "border-indigo-500 text-indigo-600 whitespace-nowrap pb-4 px-1 border-b-2 font-medium text-sm", ariaCurrent_ "page" ] "New"
