{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}

module Database where

import Model
import Data.Int
import Data.Profunctor
import Data.Text
import Data.Time
import Hasql.TH
import Hasql.Session (Session)
import Hasql.Statement (Statement(..))
import qualified Hasql.Session as Session
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Connection as Connection


-- BASE Tables --

createUserTable :: Session ()
createUserTable = Session.sql
    [uncheckedSql|
        create table if not exists user_ (
            pk_user serial primary key,
            username text unique not null,
            pic_link text null,
            bio text null,
            hash text not null,
            created_at timestampz not null default now()
        );
    |]

insertUser :: Statement UserForm User
insertUser =
    dimap
        userFormToTuple
        tupleToUser
        [singletonStatement|
            insert into user_ (username, pic_link, bio, hash)
            values ($1 :: text, $2 :: text?, $3 :: text?, crypt($4 :: text, gen_salt('bf')))
            returning pk_user :: int4, username :: text, pic_link :: text?, bio :: text?, created_at :: timestamptz
        |]
    where
        userFormToTuple :: UserForm -> (Text, Maybe Text, Maybe Text, Text)
        userFormToTuple UserForm{..} = (userFormUsername, userFormPicLink, userFormBio, userFormPassword)

        tupleToUser :: (Int32, Text, Maybe Text, Maybe Text, UTCTime) -> User
        tupleToUser (id, username, pic_link, bio, createdAt) = User
            { userBio = bio
            , userCreatedAt = createdAt
            , userID = ID id
            , userPicLink = pic_link
            , userUsername = username
            }

createArticleTable :: Session ()
createArticleTable = Session.sql
    [uncheckedSql|
        create table if not exists article (
            pk_article serial primary key,
            fk_user serial null references user on delete cascade,
            content text not null,
            created_at timestampz not null default now()
        );
    |]

insertArticle :: Statement ArticleForm Article
insertArticle =
    dimap
        articleFormToTuple
        tupleToArticle
        [singletonStatement|
            insert into article (fk_user, content)
            values ($1 :: int4?, $2 :: text)
            returning pk_article :: int4, fk_user :: int4?, content :: text, created_at :: timestamptz
        |]
    where
        articleFormToTuple :: ArticleForm -> (Maybe Int32, Text)
        articleFormToTuple ArticleForm{..} = (unID <$> articleFormAuthor, articleFormContent)

        tupleToArticle :: (Int32, Maybe Int32, Text, UTCTime) -> Article
        tupleToArticle (articleID, userID, content, createdAt) = Article
            { articleAuthor = ID <$> userID
            , articleContent = content
            , articleCreatedAt = createdAt
            , articleID = ID articleID
            }

createCommentTable :: Session ()
createCommentTable = Session.sql
    [uncheckedSql|
        create table if not exists comment (
            pk_comment serial primary key,
            fk_user serial null references user on delete cascade,
            fk_article serial null references article on delete cascade,
            fk_comment serial null references comment on delete cascade,
            content text not null,
            created_at timestampz not null default now(),
            constraint check ((fk_article is null and fk_comment is not null) or (fk_article is not null and fk_comment is null))
        );
    |]

insertComment :: Statement CommentForm Comment
insertComment =
    dimap
        commentFormToTuple
        tupleToComment
        [singletonStatement|
            insert into comment (fk_user, fk_article, fk_comment, content)
            values ($1 :: int4?, $2 :: int4?, $3 :: int4?, $4 :: text)
            returning pk_comment :: int4, fk_user :: int4?, fk_article :: int4?, fk_comment :: int4?, content :: text, created_at :: timestamptz
        |]
    where
        commentFormToTuple :: CommentForm -> (Maybe Int32, Maybe Int32, Maybe Int32, Text)
        commentFormToTuple CommentForm{..} = (unID <$> commentFormAuthor, unID <$> commentFormParentArticle, unID <$> commentFormParentComment, commentFormContent)

        tupleToComment :: (Int32, Maybe Int32, Maybe Int32, Maybe Int32, Text, UTCTime) -> Comment
        tupleToComment (commentID, authorID, parentArticleID, parentCommentID, content, createdAt) = Comment
            { commentAuthor = ID <$> authorID
            , commentContent = content
            , commentCreatedAt = createdAt
            , commentID = ID commentID
            , commentParentArticle = ID <$> parentArticleID
            , commentParentComment = ID <$> parentCommentID
            }

createTopicTable :: Session ()
createTopicTable = Session.sql
    [uncheckedSql|
        create table if not exists topic (
            pk_topic serial primary key,
            name text unique not null,
            color text not null check (color ~* '^#[a-f0-9]{2}[a-f0-9]{2}[a-f0-9]{2}$'),
            created_at timestampz not null default now()
        );
    |]

insertTopic :: Statement TopicForm Topic
insertTopic =
    dimap
        topicFormToTuple
        tupleToTopic
        [singletonStatement|
            insert into article (name, color)
            values ($1 :: text, $2 :: text)
            returning pk_topic :: int4, name :: text, color :: text, created_at :: timestamptz
        |]
    where
        topicFormToTuple :: TopicForm -> (Text, Text)
        topicFormToTuple TopicForm{..} = (topicFormName, topicFormColor)

        tupleToTopic :: (Int32, Text, Text, UTCTime) -> Topic
        tupleToTopic (topicID, name, color, createdAt) = Topic
            { topicColor = color
            , topicCreatedAt = createdAt
            , topicID = ID topicID
            , topicName = name
            }

-- JOIN Tables --

createUserFollowTable :: Session ()
createUserFollowTable = Session.sql
    [uncheckedSql|
        create table if not exists user_follow (
            fk_user_follower serial not null references user on delete cascade,
            fk_user_followee serial not null references user on delete cascade,
            created_at timestampz not null default now(),
            primary key (fk_user_follower, fk_user_followee)
        );
    |]

insertUserFollow :: Statement UserFollowForm UserFollow 
insertUserFollow =
    dimap
        userFollowFormToTuple
        tupleToUserFollow
        [singletonStatement|
            insert into user_follow (fk_user_follower, fk_user_followee)
            values ($1 :: int4, $2 :: int4)
            returning fk_user_follower :: int4, fk_user_followee :: int4, created_at :: timestamptz
        |]
    where
        userFollowFormToTuple :: UserFollowForm -> (Int32, Int32)
        userFollowFormToTuple (follower :-> followee) = (unID follower, unID followee)

        tupleToUserFollow :: (Int32, Int32, UTCTime) -> UserFollow
        tupleToUserFollow (follower, followee, createdAt) = UserFollow
            { userFollowCreatedAt = createdAt
            , userFollowFollowee = ID followee
            , userFollowFollower = ID follower
            }

createBlockUserTable :: Session ()
createBlockUserTable = Session.sql
    [uncheckedSql|
        create table if not exists block_user (
            id serial primary key,
            pk_user serial references user on delete cascade,
            fk_user serial references user on delete cascade
        );
    |]

createArticleRatingTable :: Session ()
createArticleRatingTable = Session.sql
    [uncheckedSql|
        create table if not exists article_rating (
            id serial primary key,
            fk_article serial null references article on delete cascade,
            fk_user serial null references user on delete cascade,
            rating bool not null
        );
    |]

createCommentRatingTable :: Session ()
createCommentRatingTable = Session.sql
    [uncheckedSql|
        create table if not exists comment_rating (
            id serial primary key,
            user_id serial null references user on delete cascade,
            comment_id serial null references article on delete cascade,
            rating bool not null
        );
    |]

createFollowTopicTable :: Session ()
createFollowTopicTable = Session.sql
    [uncheckedSql|
        create table if not exists follow_topic (
            id serial primary key,
            user_id serial references user on delete cascade,
            followed_user_id serial references tag on delete cascade
        );
    |]

-- INSERT Statements --

-- insertBook :: Statement Book (WithID Book)
-- insertBook =
--     dimap
--         contactFormToTuple
--         tupleToContact
--         [singletonStatement|
--             insert into contacts (name, email, status)
--             values ($1 :: text, $2 :: text, $3 :: text)
--             returning id :: int4, name :: text, email :: text, status :: text
--         |]
--     where
--         contactFormToTuple :: ContactForm -> (Text, Text, Text)
--         contactFormToTuple ContactForm{..} = (unName contactFormName, unEmail contactFormEmail, Text.pack . show $ contactFormStatus)

--         tupleToContact :: (Int32, Text, Text, Text) -> Contact
--         tupleToContact (id, name, email, status) = Contact
--             { contactID = ID id
--             , contactName = Name name
--             , contactEmail = Email email
--             , contactStatus = read . Text.unpack $ status
--             }

-- insertBooks :: Statement [Book] ()
-- insertBooks =
--     dimap
--         contactFormsUnzip
--         id
--         [resultlessStatement|
--             insert into contacts (name, email, status)
--             select * from unnest ($1 :: text[], $2 :: text[], $3 :: text[])
--         |]
--     where
--         contactFormsUnzip :: [ContactForm] -> (Vector Text, Vector Text, Vector Text)
--         contactFormsUnzip =
--             Vector.unzip3
--             . fmap
--                 (\ContactForm{..} ->
--                     (unName contactFormName, unEmail contactFormEmail, Text.pack . show $ contactFormStatus)
--                 )
--             . Vector.fromList

-- insertAuthors :: Statement [Author] ()
-- insertAuthors =
--     dimap
--         contactFormsUnzip
--         id
--         [resultlessStatement|
--             insert into contacts (name, email, status)
--             select * from unnest ($1 :: text[], $2 :: text[], $3 :: text[])
--         |]
--     where
--         contactFormsUnzip :: [ContactForm] -> (Vector Text, Vector Text, Vector Text)
--         contactFormsUnzip =
--             Vector.unzip3
--             . fmap
--                 (\ContactForm{..} ->
--                     (unName contactFormName, unEmail contactFormEmail, Text.pack . show $ contactFormStatus)
--                 )
--             . Vector.fromList

-- selectBook :: Statement (ID Book) (HasID Book)
-- selectBook =
--     dimap
--         unID
--         tupleToContact
--         [singletonStatement|
--             select id :: int4, name :: text, email :: text, status :: text
--             from contacts
--             where id = $1 :: int4
--         |]
--     where
--         tupleToContact :: (Int32, Text, Text, Text) -> Contact
--         tupleToContact (id, name, email, status) = Contact
--             { contactID = ID id
--             , contactName = Name name
--             , contactEmail = Email email
--             , contactStatus = read . Text.unpack $ status
--             }

-- selectAuthor :: Statement (ID Author) (HasID Author)
-- selectAuthor =
--     dimap
--         unID
--         tupleToContact
--         [singletonStatement|
--             select id :: int4, name :: text, email :: text, status :: text
--             from contacts
--             where id = $1 :: int4
--         |]
--     where
--         tupleToContact :: (Int32, Text, Text, Text) -> Contact
--         tupleToContact (id, name, email, status) = Contact
--             { contactID = ID id
--             , contactName = Name name
--             , contactEmail = Email email
--             , contactStatus = read . Text.unpack $ status
--             }

-- selectBooks :: Statement () [Contact] -- Add sort arg
-- selectBooks =
--     dimap id (Vector.toList . fmap tupleToContact)
--         [vectorStatement|
--             select id :: int4, name :: text, email :: text, status :: text
--             from contacts
--             |]
--     where
--         tupleToContact :: (Int32, Text, Text, Text) -> Contact
--         tupleToContact (id, name, email, status) = Contact
--             { contactID = ID id
--             , contactName = Name name
--             , contactEmail = Email email
--             , contactStatus = read . Text.unpack $ status
--             }

-- selectAuthors :: Statement () [Contact] -- Add sort arg
-- selectAuthors =
--     dimap id (Vector.toList . fmap tupleToContact)
--         [vectorStatement|
--             select id :: int4, name :: text, email :: text, status :: text
--             from contacts
--             |]
--     where
--         tupleToContact :: (Int32, Text, Text, Text) -> Contact
--         tupleToContact (id, name, email, status) = Contact
--             { contactID = ID id
--             , contactName = Name name
--             , contactEmail = Email email
--             , contactStatus = read . Text.unpack $ status
--             }

-- deleteBook :: Statement (ID Book) ()
-- deleteBook =
--     dimap (\(ID contactID) -> contactID) id
--         [resultlessStatement| 
--             delete from contacts where id = $1 :: int4
--             |]

-- deleteAuthor :: Statement (ID Author) ()
-- deleteAuthor =
--     dimap (\(ID contactID) -> contactID) id
--         [resultlessStatement| 
--             delete from contacts where id = $1 :: int4
--             |]

-- updateContactStatement :: Statement (ID Contact, ContactForm) Contact
-- updateContactStatement =
--     dimap
--         contactFormWithIDToTuple
--         tupleToContact
--         [singletonStatement|
--             update contacts
--             set name = $2 :: Text,
--                 email = $3 :: Text,
--                 status = $4 :: Text
--             where id = $1 :: int4
--             returning id :: int4, name :: text, email :: text, status :: text
--         |]
--     where
--         contactFormWithIDToTuple :: (ID Contact, ContactForm) -> (Int32, Text, Text, Text)
--         contactFormWithIDToTuple (contactID, ContactForm{..}) =
--             (unID contactID, unName contactFormName, unEmail contactFormEmail, Text.pack . show $ contactFormStatus)

--         tupleToContact :: (Int32, Text, Text, Text) -> Contact
--         tupleToContact (id, name, email, status) = Contact
--             { contactID = ID id
--             , contactName = Name name
--             , contactEmail = Email email
--             , contactStatus = read . Text.unpack $ status
--             }

-- insertContactDB :: Connection.Connection -> ContactForm -> IO Contact
-- insertContactDB conn contactForm = do
--     Right res <- Session.run (Session.statement contactForm insertContactStatement) conn
--     pure res

-- insertContactsDB :: Connection.Connection -> [ContactForm] -> IO ()
-- insertContactsDB conn contacts = do
--     Right res <- Session.run (Session.statement contacts insertContactsStatement) conn
--     pure res

-- getContactFromDB :: Connection.Connection -> ID Contact -> IO Contact
-- getContactFromDB conn contactID = do
--     Right res <- Session.run (Session.statement contactID getContactStatement) conn
--     pure res

-- getContactsFromDB :: Connection.Connection -> IO [Contact]
-- getContactsFromDB conn = do
--     Right res <- Session.run (Session.statement () getContactsStatement) conn
--     pure res

-- deleteContactFromDB :: Connection.Connection -> ID Contact -> IO ()
-- deleteContactFromDB conn contactID = do
--     Right res <- Session.run (Session.statement contactID deleteContactStatement) conn
--     pure res

-- updateContactDB :: Connection.Connection -> (ID Contact, ContactForm) -> IO Contact
-- updateContactDB conn contactFormWithID = do
--     Right res <- Session.run (Session.statement contactFormWithID updateContactStatement) conn
--     pure res

-- DATABASE END --
