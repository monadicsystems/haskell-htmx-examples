{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}


module API (CritterAPI) where

import Lucid

import View.Wrapper
import View.Page.Home
import View.Page.SignUpForm
import View.Page.SignInForm
import View.Page.ArticleForm

import Servant
import Servant.Auth
import Servant.HTML.Lucid
import Servant.HTMX

type CritterAPI = 

type Protected = "new_article" :> HXRequest :> Get '[HTML] (Either (Wrapper ArticleForm) ArticleForm)

type Unprotected =
    Get '[HTML] (Wrapper Home)
    :<|> "signUpForm" :> HXRequest :> Get '[HTML] (Either (Wrapper SignUpForm) SignUpForm)
    :<|> "signInForm" :> HXRequest :> Get '[HTML] (Either (Wrapper SignInForm) SignInForm)
    :<|> "signUp" :> ReqBody '[JSON] SignUpForm :> Post '[HTML] (Either ) -- 
    :<|> "signIn" :> ReqBody '[JSON] SignInForm :> Post '[HTML] (Either ) --

instance (ToHtml l, ToHtml r) => ToHtml (Either l r) where
    toHtml either = case either of
        Left l -> toHtml l
        Right r -> toHtml r
    toHtmlRaw either = case either of
        Left l -> toHtmlRaw l
        Right r -> toHtmlRaw r
