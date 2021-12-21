{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module InterfaceAdapters.AgricultureRestService where
import           Polysemy
import           Polysemy.Error
import           Servant
import           qualified UseCases.AgricultureUseCase as UC (weatherTown, WeatherStatus, WeatherStatusError)
import           UseCases.WWI
import           InterfaceAdapters.Telegram.Telegram
import           InterfaceAdapters.Weather.WWITelegramPirate
                                     
-- | Declaring the routes of the REST API for Agriculture weather 
type AgricultureAPI =
       Summary "retrieve weather of a place sent thru Telegram (TelegramMessage -> TheWeatherThere)"
                      :> ReqBody '[ JSON] TelegramMessage
                      :> Post    '[ PlainText] UseCases.WWI.TheWeatherThere -- Post    /weather
  :<|> Summary "retrieve weather of a place sent not as a JSON (PlaceName -> TheWeatherThere)"
                      :> ReqBody '[ PlainText] UseCases.WWI.PlaceName
                      :> Post    '[ PlainText] UseCases.WWI.TheWeatherThere -- Post    /weather

-- | implements the AgricultureAPI
agricultureServer :: (Member (Embed IO) r, Member UC.WeatherStatus r, Member (Error UC.WeatherStatusError) r) => ServerT AgricultureAPI (Sem r)
agricultureServer =
        weatherTownTelegram -- POST   /TelegramMessage -> TheWeatherThere
  :<|>  UC.weatherTown     -- POST    /PlaceName -> TheWeatherThere

-- | boilerplate needed to guide type inference
agricultureAPI :: Proxy AgricultureAPI
agricultureAPI = Proxy
 
