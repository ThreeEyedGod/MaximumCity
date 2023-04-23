{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module InterfaceAdapters.AgricultureRestService where
import Polysemy ( Sem, Member, Embed )
import Polysemy.Error ()
import Servant
    ( Proxy(..),
      type (:>),
      Post,
      JSON,
      type (:<|>)(..),
      ReqBody,
      PlainText,
      Summary,
      HasServer(ServerT) )
import           UseCases.AgricultureUseCase (getInfoTlgm, getInfoPlainText)
import           UseCases.WWI (TheWeatherThere, PlaceName, WWI)
import           InterfaceAdapters.Telegram.Telegram
import           InterfaceAdapters.Weather.WWITelegramPirate
import           InterfaceAdapters.Weather.WWIWebPirate
import qualified Data.Text as T

                                     
-- | Declaring the routes of the REST API for Agriculture weather 
type AgricultureAPI =
       Summary "retrieve weather of a place sent thru Telegram (TelegramMessage -> TheWeatherThere)"
                      :> ReqBody '[ JSON] TelegramMessage
                      :> Post    '[ PlainText] TheWeatherThere -- Post    /weather
  :<|> Summary "retrieve weather of a place sent not as a JSON (PlaceName -> TheWeatherThere)"
                      :> ReqBody '[ PlainText] PlaceName
                      :> Post    '[ PlainText] TheWeatherThere -- Post    /weather

-- | implements the AgricultureAPI
agricultureServer :: (Member (Embed IO) r, Member WWI r) => ServerT AgricultureAPI (Sem r)
agricultureServer =  getInfoTlgm :<|>  getInfoPlainText

-- | boilerplate needed to guide type inference
agricultureAPI :: Proxy AgricultureAPI
agricultureAPI = Proxy