module Model where

import Data.Int
import Data.Text
import Data.Time
import Lucid (With)


newtype ID a = ID { unID :: Int32 } deriving (Eq, Show)

-- newtype Password = Password { unPassword :: Text } deriving (Eq, Show)

-- data WithMetaData a = WithMetaData
--     { withMetaDataCreatedAt :: UTCTime
--     , withMetaDataData :: a
--     , withMetaDataID :: ID a
--     , withMetaDataUpdatedAt :: Maybe UTCTime
--     } deriving (Eq, Show)

data User = User
    { userBio :: Maybe Text
    , userCreatedAt :: UTCTime
    , userID :: ID User
    , userPicLink :: Maybe Text
    , userUsername :: Text
    } deriving (Eq, Show)

data UserForm = UserForm
    { userFormBio :: Maybe Text
    , userFormPassword :: Text
    , userFormPicLink :: Maybe Text
    , userFormUsername :: Text
    } deriving (Eq, Show)

data UserFollow = UserFollow
    { userFollowCreatedAt :: UTCTime
    , userFollowFollowee :: ID User
    , userFollowFollower :: ID User
    } deriving (Eq, Show)

data UserFollowForm = (ID User) :-> (ID User) deriving (Eq, Show)

data UserBlock = UserBlock
    { userBlockBlockee :: ID User
    , userBlockBlocker :: ID User
    , userBlockCreatedAt :: UTCTime
    } deriving (Eq, Show)

data UserBlockForm = (ID User) :-| (ID User) deriving (Eq, Show)

data TopicFollow = TopicFollow
    { topicFollowCreatedAt :: UTCTime
    , topicFollowFollower :: ID User
    , topicFollowTopic :: ID Topic
    } deriving (Eq, Show)

data TopicFollowForm = (ID User) :~> (ID Topic) deriving (Eq, Show)

data Article = Article
    { articleAuthor :: Maybe (ID User)
    , articleContent :: Text
    , articleCreatedAt :: UTCTime
    , articleID :: ID Article
    , articleTitle :: Text
    } deriving (Eq, Show)

data ArticleForm = ArticleForm
    { articleFormAuthor :: Maybe (ID User)
    , articleFormContent :: Text
    , articleFormTitle :: Text
    } deriving (Eq, Show)

data Comment = Comment
    { commentAuthor :: Maybe (ID User)
    , commentContent :: Text
    , commentCreatedAt :: UTCTime
    , commentID :: ID Comment
    , commentParentArticle :: Maybe (ID Article)
    , commentParentComment :: Maybe (ID Comment)
    } deriving (Eq, Show)

-- data Comment a = Comment
--     { commentAuthor :: Maybe (ID User)
--     , commentContent :: Text
--     , commentCreatedAt :: UTCTime
--     , commentID :: ID (Comment a)
--     , commentParent :: ID a
--     } deriving (Eq, Show)

-- type ArticleComment = Comment Article
-- type ReplyComment a = Comment (Comment a)

-- data ArticleComment = ArticleComment
--     { articleCommentAuthor :: Maybe (ID User)
--     , articleCommentContent :: Text
--     , articleCommentCreatedAt :: UTCTime
--     , articleCommentID :: ID ArticleComment
--     , articleCommentParent :: ID Article
--     } deriving (Eq, Show)

-- data ReplyComment a = ReplyComment
--     { replyCommentAuthor :: Maybe (ID User)
--     , replyCommentContent :: Text
--     , replyCommentCreatedAt :: UTCTime
--     , replyCommentID :: ID (ReplyComment a)
--     , replyCommentParent :: ID (ReplyComment a)
--     } deriving (Eq, Show)

data CommentForm = CommentForm
    { commentFormAuthor :: Maybe (ID User)
    , commentFormContent :: Text
    , commentFormParentArticle :: Maybe (ID Article)
    , commentFormParentComment :: Maybe (ID Comment) -- Either???
    } deriving (Eq, Show)

data Topic = Topic
    { topicColor :: Text
    , topicCreatedAt :: UTCTime
    , topicID :: ID Topic
    , topicName :: Text
    } deriving (Eq, Show)

data TopicForm = TopicForm
    { topicFormColor :: Text
    , topicFormName :: Text
    } deriving (Eq, Show)
