

data MessageMeta = FromMessageMeta
  {
    id :: Int,
    is_bot :: Bool, --false
    first_name :: T.Text,
    last_name :: T.Text,
    language_code :: T.Text --en
  } 
  deriving (Generic)
data ChatTextMeta = ChatTextMeta
  {
    id_chat :: Int,
    first_name_chat :: T.Text,
    last_name_chat :: T.Text,
    type_chat :: T.Text --private 
  } deriving (Generic)

data EntityDetails = EntityDetails {
  offsetD :: Int,
  lengthD :: Int,
  typeD :: T.Text --bot_command
} deriving (Generic)

data MessageDetails  = MessageDetails {
    fromMeta :: MessageMeta,
    chatMeta :: ChatTextMeta,
    dateMessage ::  Int,
    actualMessage :: T.Text,
    allEntityDetails :: [EntityDetails]
} deriving (Generic)

data TMessageBody = TMessageBody
 {
    update_id :: Int,
    message :: MessageDetails
 } deriving (Generic)
