{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module View.Snippet.Navbar where

import Lucid
import Lucid.Alpine
import Lucid.HTMX
import Lucid.Supplemental hiding (svg_)
import Data.Text

data Navbar = Navbar

instance ToHtml Navbar where
    toHtml _ = do
        toHtmlRaw @Text "<!-- This example requires Tailwind CSS v2.0+ -->"
        header_ [ class_ "bg-indigo-600" ] $ nav_ [ class_ "max-w-7xl mx-auto px-4 px-6 sm:px-8", ariaLabel_ "Top" ] $ do
            div_ [ class_ "w-full py-6 flex items-center justify-between border-b border-indigo-500 sm:border-none" ] $ do
                div_ [ class_ "flex items-center", hxBoost_ "true" ] $ do
                    a_ [ href_ "/" ] $ do
                        span_ [ class_ "sr-only" ] "Workflow"
                        img_ [ class_ "h-10 w-auto", src_ "https://tailwindui.com/img/logos/workflow-mark.svg?color=white", alt_ "" ]
                    div_ [ class_ "hidden ml-10 space-x-8 sm:block" ] $ do
                        a_ [ href_ "#", class_ "text-base font-medium text-white hover:text-indigo-50" ] "Feed"
                        a_ [ href_ "#", class_ "text-base font-medium text-white hover:text-indigo-50" ] "Chatrooms"
                        a_ [ href_ "#", class_ "text-base font-medium text-white hover:text-indigo-50" ] "Help"
                div_ [ class_ "ml-10 space-x-4", hxBoost_ "true" ] $ do
                    a_ [ class_ "inline-block bg-indigo-500 py-2 px-4 border border-transparent rounded-md text-base font-medium text-white hover:bg-opacity-75" 
                       , href_ "/signInForm"
                       ]
                       "Sign In"
                    a_ [ class_ "inline-block bg-white py-2 px-4 border border-transparent rounded-md text-base font-medium text-indigo-600 hover:bg-indigo-50"
                       , href_ "/signUpForm"
                       ]
                       "Sign Up"
            div_ [ class_ "py-4 flex flex-wrap justify-center space-x-6 sm:hidden" ] $ do
                a_ [ href_ "#", class_ "text-base font-medium text-white hover:text-indigo-50" ] "Feed"
                a_ [ href_ "#", class_ "text-base font-medium text-white hover:text-indigo-50" ] "Chatrooms"
                a_ [ href_ "#", class_ "text-base font-medium text-white hover:text-indigo-50" ] "Help"
