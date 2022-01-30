module Main where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON)
import Data.Int
import Data.Profunctor
import Data.Proxy
import Data.Text
import Data.Vector (Vector)
import GHC.Generics
import Hasql.TH
import Hasql.Session (Session, QueryError)
import Hasql.Statement (Statement(..))
import Lucid
import Lucid.Base (makeAttribute)
import Lucid.HTMX
import Lucid.HTMX.Servant
import Network.Wai.Handler.Warp
import Prelude
import Servant.API
import Servant.HTML.Lucid
import Servant.Server

import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.ByteString.Char8 as ByteStringChar8
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Hasql.Session as Session
import qualified Hasql.Connection as Connection


{- COMMON START -}

showT :: Show a => a -> Text
showT = Text.pack . show

showB :: Show a => a -> ByteStringLazy.ByteString
showB = ByteStringLazy.fromStrict . ByteStringChar8.pack . show

readT :: Read a => Text -> a
readT = read . Text.unpack

noHtml :: Html ()
noHtml = ""

-- forall a. Text -> Html a -> Html a
baseTemplate :: Monad m => Text -> HtmlT m a -> HtmlT m a
baseTemplate title innerHtml = do
    doctype_

    html_ [lang_ "en"] ""

    head_ $ do
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]

        title_ $ toHtml title

        link_ [href_ "https://unpkg.com/tailwindcss@^2/dist/tailwind.min.css", rel_ "stylesheet"]
        script_ [src_ "https://unpkg.com/htmx.org@1.5.0"] noHtml
        script_ [src_ "https://unpkg.com/htmx.org/dist/ext/json-enc.js"] noHtml

    body_ innerHtml

{- COMMON STUFF END -}

{- DATA MODEL START -}

newtype ID a = ID { unID :: Int32 }
    deriving newtype (Eq, FromJSON, Show, FromHttpApiData, ToHttpApiData)

newtype Email = Email { unEmail :: Text }
    deriving newtype (FromJSON, Eq, Show, ToHtml)

newtype Name = Name { unName :: Text }
    deriving newtype (FromJSON, Eq, Show, ToHtml)

data Status = Active | Inactive
    deriving (Eq, Show, Read, Generic, FromJSON)

data Contact = Contact
    { contactID :: ID Contact
    , contactName :: Name
    , contactEmail :: Email
    , contactStatus :: Status
    }
    deriving (Eq, Show)

type ContactTable = [Contact]

newtype ContactForm = ContactForm (Maybe Contact)

data ContactFormData = ContactFormData
    { contactFormDataName :: Name
    , contactFormDataEmail :: Email
    , contactFormDataStatus :: Status
    }
    deriving (Eq, Generic, Show, FromJSON)

{- DATA MODEL END -}

{- SQL STATEMENTS START -}

dropContactsTable :: Session ()
dropContactsTable = Session.sql [uncheckedSql| drop table if exists contacts |]

createContactsTable :: Session ()
createContactsTable = Session.sql
    [uncheckedSql|
        create table if not exists contacts (
            id serial primary key,
            name varchar (50) unique not null,
            email varchar (255) unique not null,
            status varchar (10) not null,
            created_at timestamp with time zone default current_timestamp
        )
    |]

-- cfd -> ContactFormData

cfdToTuple :: ContactFormData -> (Text, Text, Text)
cfdToTuple (ContactFormData (Name n) (Email e) status) = (n, e, showT status)

tupleToContact :: (Int32, Text, Text, Text) -> Contact
tupleToContact (cID, n, e, status) = Contact
    { contactID = ID cID
    , contactName = Name n
    , contactEmail = Email e
    , contactStatus = readT status
    }

insertCfdStatement :: Statement ContactFormData Contact
insertCfdStatement =
    dimap
        cfdToTuple
        tupleToContact
        [singletonStatement|
            insert into contacts (name, email, status)
            values ($1 :: text, $2 :: text, $3 :: text)
            returning id :: int4, name :: text, email :: text, status :: text
        |]

insertCfdsStatement :: Statement [ContactFormData] ()
insertCfdsStatement =
    dimap
        cfdsUnzip
        id
        [resultlessStatement|
            insert into contacts (name, email, status)
            select * from unnest ($1 :: text[], $2 :: text[], $3 :: text[])
        |]
    where
        cfdsUnzip
            :: [ContactFormData]
            -> (Vector Text, Vector Text, Vector Text)
        cfdsUnzip = Vector.unzip3 . fmap cfdToTuple . Vector.fromList

selectContactStatement :: Statement (ID Contact) Contact
selectContactStatement =
    dimap
        unID
        tupleToContact
        [singletonStatement|
            select id :: int4, name :: text, email :: text, status :: text
            from contacts
            where id = $1 :: int4
        |]

selectContactTableStatement :: Statement () ContactTable
selectContactTableStatement =
    dimap
        id
        (Vector.toList . fmap tupleToContact)
        [vectorStatement|
            select id :: int4, name :: text, email :: text, status :: text
            from contacts
            order by "created_at"
        |]

deleteContactStatement :: Statement (ID Contact) ()
deleteContactStatement =
    dimap
        unID
        id
        [resultlessStatement| 
            delete from contacts where id = $1 :: int4
        |]

updateContactStatement :: Statement (ID Contact, ContactFormData) Contact
updateContactStatement =
    dimap
        cfdWithIDToTuple
        tupleToContact
        [singletonStatement|
            update contacts
            set name = $2 :: Text,
                email = $3 :: Text,
                status = $4 :: Text
            where id = $1 :: int4
            returning id :: int4, name :: text, email :: text, status :: text
        |]
    where
        cfdWithIDToTuple
            :: (ID Contact, ContactFormData)
            -> (Int32, Text, Text, Text)
        cfdWithIDToTuple (ID cID, ContactFormData (Name n) (Email e) status) =
            (cID, n, e, showT status)

{- SQL STATEMENTS END -}

{- SQL FUNCTIONS START -}

insertCfd :: Connection.Connection -> ContactFormData -> IO (Either QueryError Contact)
insertCfd conn cfd = Session.run (Session.statement cfd insertCfdStatement) conn

insertCfds :: Connection.Connection -> [ContactFormData] -> IO (Either QueryError ())
insertCfds conn cfds = Session.run (Session.statement cfds insertCfdsStatement) conn

selectContact :: Connection.Connection -> ID Contact -> IO (Either QueryError Contact)
selectContact conn cID = Session.run (Session.statement cID selectContactStatement) conn

selectContactTable :: Connection.Connection -> IO (Either QueryError ContactTable)
selectContactTable conn = Session.run (Session.statement () selectContactTableStatement) conn

deleteContact :: Connection.Connection -> ID Contact -> IO (Either QueryError ())
deleteContact conn cID = Session.run (Session.statement cID deleteContactStatement) conn

updateContact :: Connection.Connection -> (ID Contact, ContactFormData) -> IO (Either QueryError Contact)
updateContact conn cfdWithID = Session.run (Session.statement cfdWithID updateContactStatement) conn

{- SQL FUNCTIONS START -}

{- API DEFINITION START -}

type GetContactTable = Get '[HTML] ContactTable

type GetContactForm = "edit"
    :> Capture "contact-id" (ID Contact)
    :> Get '[HTML] ContactForm

type GetContact = Capture "contact-id" (ID Contact) :> Get '[HTML] Contact

type PostContact = ReqBody '[JSON] ContactFormData :> Post '[HTML] Contact

type PatchContact = "edit"
    :> Capture "contact-id" (ID Contact)
    :> ReqBody '[JSON] ContactFormData
    :> Patch '[HTML] Contact

type DeleteContact = Capture "contact-id" (ID Contact) :> Delete '[HTML] NoContent

type API = GetContactTable
    :<|> GetContactForm
    :<|> GetContact
    :<|> PostContact
    :<|> PatchContact
    :<|> DeleteContact

{- API DEFINITION END -}

{- API PROXIES START -}

getContactTableProxy :: Proxy GetContactTable
getContactTableProxy = Proxy

getContactProxy :: Proxy GetContact
getContactProxy = Proxy

deleteContactProxy :: Proxy DeleteContact
deleteContactProxy = Proxy

postContactProxy :: Proxy PostContact
postContactProxy = Proxy

patchContactProxy :: Proxy PatchContact
patchContactProxy = Proxy

getContactFormProxy :: Proxy GetContactForm
getContactFormProxy = Proxy

apiProxy :: Proxy API
apiProxy = Proxy

{- API PROXIES END -}

{- HANDLERS START -}

getContactTableHandler :: Connection.Connection -> Handler ContactTable
getContactTableHandler conn = do
    res <- liftIO $ selectContactTable conn
    case res of
        Left queryErr      -> throwError $ err404 { errBody = showB queryErr }
        Right contactTable -> pure contactTable

getContactHandler :: Connection.Connection -> ID Contact -> Handler Contact
getContactHandler conn cID = do
    res <- liftIO $ selectContact conn cID
    case res of
        Left queryErr -> throwError $ err404 { errBody = showB queryErr }
        Right contact -> pure contact

postContactHandler :: Connection.Connection -> ContactFormData -> Handler Contact
postContactHandler conn cfd = do
    res <- liftIO $ insertCfd conn cfd
    case res of
        Left queryErr -> throwError $ err404 { errBody = showB queryErr }
        Right contact -> pure contact

deleteContactHandler :: Connection.Connection -> ID Contact -> Handler NoContent
deleteContactHandler conn cID = do
    res <- liftIO $ deleteContact conn cID
    case res of
        Left queryErr -> throwError $ err404 { errBody = showB queryErr }
        Right _       -> pure NoContent

patchContactHandler :: Connection.Connection -> ID Contact -> ContactFormData -> Handler Contact
patchContactHandler conn cID cfd = do
    res <- liftIO $ updateContact conn (cID, cfd)
    case res of
        Left queryErr -> throwError $ err404 { errBody = showB queryErr }
        Right contact -> pure contact

getContactFormHandler :: Connection.Connection -> ID Contact -> Handler ContactForm
getContactFormHandler conn cID = do
    res <- liftIO $ selectContact conn cID
    case res of
        Left queryErr -> throwError $ err404 { errBody = showB queryErr }
        Right contact -> pure $ ContactForm $ Just contact

server :: Connection.Connection -> Server API
server conn = getContactTableHandler conn
    :<|> getContactFormHandler conn
    :<|> getContactHandler conn
    :<|> postContactHandler conn
    :<|> patchContactHandler conn
    :<|> deleteContactHandler conn

{- HANDLERS END -}

{- SAFE LINKS START -}

getContactLink :: ID Contact -> Link
getContactLink = safeLink apiProxy getContactProxy

deleteContactLink :: ID Contact -> Link
deleteContactLink = safeLink apiProxy deleteContactProxy

postContactLink :: Link
postContactLink = safeLink apiProxy postContactProxy

patchContactLink :: ID Contact -> Link
patchContactLink = safeLink apiProxy patchContactProxy

getContactFormLink :: ID Contact -> Link
getContactFormLink = safeLink apiProxy getContactFormProxy

{- SAFE LINKS END -}

{- HTML START -}

noAttr_ :: Attribute
noAttr_ = makeAttribute "" ""

tableCellCss_ :: Text -> Attribute
tableCellCss_ custom = class_ $
    "border-4 items-center justify-center px-4 py-2 text-semibold text-lg text-center " <> custom

tableHeaderCss_ :: Text -> Attribute
tableHeaderCss_ = tableCellCss_

buttonCss_ :: Text -> Attribute
buttonCss_ custom = class_ $
    "px-4 py-2 text-lg text-white rounded-md " <> custom

instance ToHtml (ID a) where
    toHtml = toHtml . showT
    toHtmlRaw = toHtml

instance ToHtml Status where
    toHtml = \case
        Active -> "Active"
        Inactive -> "Inactive"
    toHtmlRaw = toHtml

instance ToHtml Contact where
    toHtml (Contact cID name email status) = do
        let rowId = "contact-row-" <> showT cID

        tr_ [id_ rowId] $ do
            td_ [tableCellCss_ ""] $ toHtml cID
            td_ [tableCellCss_ ""] $ toHtml name
            td_ [tableCellCss_ ""] $ toHtml email
            td_ [tableCellCss_ ""] $ toHtml status
            td_ [tableCellCss_ ""] $ do
                span_ [class_ "flex flex-row justify-center align-middle"] $ do
                    button_
                        [ buttonCss_ "mr-2 bg-purple-400"
                        , hxGetSafe_ $ getContactFormLink cID
                        , hxTarget_ $ "#" <> rowId
                        , hxSwap_ "outerHTML"
                        ]
                        "Edit"
                    button_
                        [ buttonCss_ "bg-red-400"
                        , hxDeleteSafe_ $ deleteContactLink cID
                        , hxConfirm_ "Are you sure?"
                        , hxTarget_ $ "#" <> rowId
                        , hxSwap_ "outerHTML"
                        ]
                        "Delete"
    toHtmlRaw = toHtml

instance ToHtml ContactForm where
    toHtml (ContactForm maybeContact) = case maybeContact of
        Nothing -> do
            tr_ [id_ "add-contact-row"] $ do
                td_ [tableCellCss_ ""] ""
                td_ [tableCellCss_ ""] $ input_
                    [ class_ "rounded-md px-2 border-2 add-contact-form-input"
                    , type_ "text"
                    , name_ "contactFormDataName"
                    ]
                td_ [tableCellCss_ ""] $ input_
                    [ class_ "rounded-md px-2 border-2 add-contact-form-input"
                    , type_ "text"
                    , name_ "contactFormDataEmail"
                    ]
                td_ [tableCellCss_ ""] $
                    form_ $ do
                        span_ [class_ "flex flex-col justify-center align-middle"] $ do
                            label_ [] $ do
                                "Active"
                                input_
                                    [ type_ "radio"
                                    , name_ "contactFormDataStatus"
                                    , value_ $ showT Active
                                    , class_ "ml-2 add-contact-form-input"
                                    ]
                            label_ [] $ do
                                "Inactive"
                                input_
                                    [ type_ "radio"
                                    , name_ "contactFormDataStatus"
                                    , value_ $ showT Inactive
                                    , class_ "ml-2 add-contact-form-input"
                                    ]
                td_ [tableCellCss_ ""] $
                    button_
                        [ buttonCss_ "w-full bg-green-400"
                        , hxExt_ "json-enc"
                        , hxPostSafe_ postContactLink
                        , hxTarget_ "#add-contact-row"
                        , hxSwap_ "beforebegin"
                        , hxInclude_ ".add-contact-form-input"
                        ]
                        "Add"
        Just (Contact cID (Name name) (Email email) status) -> do
            let editRowId = "edit-contact-row-" <> showT cID
                inputClass = "edit-contact-form-" <> showT cID <> "-input"

            tr_ [id_ editRowId] $ do
                td_ [tableCellCss_ ""] $ toHtml cID
                td_ [tableCellCss_ ""] $
                    input_
                        [ class_ $ "rounded-md px-2 border-2 " <> inputClass
                        , type_ "text"
                        , name_ "contactFormDataName"
                        , value_ name
                        ]
                td_ [tableCellCss_ ""] $
                    input_
                        [ class_ $ "rounded-md px-2 border-2 " <> inputClass
                        , type_ "text"
                        , name_ "contactFormDataEmail"
                        , value_ email
                        ]
                td_ [tableCellCss_ ""] $
                    form_ $ do
                        span_ [class_ "flex flex-col justify-center align-middle"] $ do
                            label_ [] $ do
                                "Active"
                                input_
                                    [ type_ "radio"
                                    , name_ "contactFormDataStatus"
                                    , value_ . Text.pack . show $ Active
                                    , class_ $ "ml-2 " <> inputClass
                                    , if status == Active then checked_ else noAttr_
                                    ]
                            label_ [] $ do
                                "Inactive"
                                input_
                                    [ type_ "radio"
                                    , name_ "contactFormDataStatus"
                                    , value_ . Text.pack . show $ Inactive
                                    , class_ $ "ml-2 " <> inputClass
                                    , if status == Inactive then checked_ else noAttr_
                                    ]
                td_ [tableCellCss_ ""] $
                    span_ [class_ "flex flex-row justify-center align-middle"] $ do
                        button_
                            [ buttonCss_ "mr-2 bg-green-500"
                            , hxExt_ "json-enc"
                            , hxPatchSafe_ $ patchContactLink cID
                            , hxTarget_ $ "#" <> editRowId
                            , hxSwap_ "outerHTML"
                            , hxInclude_ $ "." <> inputClass
                            ]
                            "Save"
                        button_
                            [ buttonCss_ "bg-red-400"
                            , hxGetSafe_ $ getContactLink cID
                            , hxTarget_ $ "#" <> editRowId
                            , hxSwap_ "outerHTML"
                            ]
                            "Cancel"
    toHtmlRaw = toHtml

instance ToHtml ContactTable where
    toHtml contactTable = baseTemplate "Contact Table" $ do
        script_ "document.body.addEventListener('htmx:beforeSwap',function(e){'add-contact-row'===e.detail.target.id&&Array.from(document.getElementsByClassName('add-contact-form-input')).map(e=>{e.value&&(e.value=e.defaultValue),e.checked&&(e.checked=e.defaultChecked)})});"
        div_ [class_ "flex items-center justify-center h-screen"] $
            table_ [class_ "table-auto rounded-lg"] $ do
                thead_ [] $
                    tr_ [] $ do
                        th_ [tableHeaderCss_ ""] "ID"
                        th_ [tableHeaderCss_ ""] "Name"
                        th_ [tableHeaderCss_ ""] "Email"
                        th_ [tableHeaderCss_ ""] "Status"
                        th_ [tableHeaderCss_ ""] "Action(s)"
                tbody_ $ do
                    Prelude.mapM_ toHtml contactTable
                    toHtml $ ContactForm Nothing
    toHtmlRaw = toHtml

{- HTML END -}

main :: IO ()
main = do
    let dbConnSettings = Connection.settings "localhost" 5432 "postgres" "dummy" "ex1"
        initialCfds =
            [ ContactFormData (Name "Alice Jones") (Email "alice@gmail.com") Active
            , ContactFormData (Name "Bob Hart") (Email "bhart@gmail.com") Inactive
            , ContactFormData (Name "Corey Smith") (Email "coreysm@grubco.com") Active
            ]

    connResult <- Connection.acquire dbConnSettings
    case connResult of
        Left err -> print err
        Right conn -> do
            _ <- Session.run dropContactsTable conn
            _ <- Session.run createContactsTable conn
            _ <- insertCfds conn initialCfds
            let port = 8080
                application = serve @API Proxy $ server conn

            print $ "Serving application on port: " <> show port
            run port application
