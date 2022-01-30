{-# LANGUAGE OverloadedStrings #-}

module View.Page.Profile where

import Lucid
import Lucid.HTMX

import Model

data Profile = Profile User

instance ToHtml Profile where
    toHtml (Profile user) = do
        h1_ [] "This is the users profile"
