{-# LANGUAGE OverloadedStrings #-}

module View.Page.About where

import Lucid
import Lucid.HTMX

data About = About

instance ToHtml About where
    toHtml About = do
        h1_ [class_ "text-2xl font-bold"] "About"
