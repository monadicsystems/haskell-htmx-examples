{-# LANGUAGE OverloadedStrings #-}

module Handler (critterServer) where

import Data.Text
import Servant.Auth.Server
import Servant.Server
import Servant
import View.Page.Home
import View.Page.SignUpForm
import View.Page.SignInForm
import View.Snippet.ArticleList
import View.Page.ArticleForm
import View.Wrapper
import API

critterServer :: Server CritterAPI
critterServer = homeHandler
              :<|> signUpFormHandler
              :<|> signInFormHandler
              :<|> articleFormHandler

homeHandler :: Handler (Wrapper Home)
homeHandler = pure $ Wrapper $ Home (ArticleList [])

signUpFormHandler :: Maybe Text -> Handler (Either (Wrapper SignUpForm) SignUpForm)
signUpFormHandler mb = pure $ case mb of
    Just "true" -> Right SignUpForm
    _ -> Left $ Wrapper SignUpForm

signInFormHandler :: Maybe Text -> Handler (Either (Wrapper SignInForm) SignInForm)
signInFormHandler mb = pure $ case mb of
    Just "true" -> Right SignInForm
    _ -> Left $ Wrapper SignInForm

articleFormHandler :: Maybe Text -> Handler (Either (Wrapper ArticleForm) ArticleForm)
articleFormHandler mb = pure $ case mb of
    Just "true" -> Right ArticleForm
    _ -> Left $ Wrapper ArticleForm
