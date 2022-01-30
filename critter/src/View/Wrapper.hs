{-# LANGUAGE OverloadedStrings #-}

module View.Wrapper where

import Lucid

newtype Wrapper a = Wrapper a

instance ToHtml a => ToHtml (Wrapper a) where
  toHtml (Wrapper content) = do
    doctype_

    html_ [lang_ "en"] ""

    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]

      title_ "Critter"

      script_ [src_ "https://cdn.tailwindcss.com?plugins=forms,typography"] ("" :: Html ())
      script_ [src_ "https://unpkg.com/htmx.org@1.6.1"] ("" :: Html ())
      script_ [src_ "https://unpkg.com/htmx.org/dist/ext/json-enc.js"] ("" :: Html ())

      script_ [src_ "https://unpkg.com/alpinejs@3.x.x/dist/cdn.min.js", defer_ ""] ("" :: Html ())

    body_ [] $ do
      toHtml content
