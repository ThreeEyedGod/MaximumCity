module InterfaceAdapters.Config where

-- | global application configuration
data Config = Config {
  port        :: Int     -- ^ the port where the server is listening
, backend     :: Backend -- ^ selects the weather source
, frontend    :: Frontend  -- ^ the path to UI channel
, gateway     :: Gateway  -- ^ api calls through 
, verbose     :: Bool    -- ^ True enables logging
}

data Backend = PirateWeather | OpenWeather deriving (Show, Eq)
data Frontend = Telegram | Signal | Web | Whatsapp | CL deriving (Show, Eq)
data Gateway  = AWSAPIRest | Other deriving (Show, Eq)
