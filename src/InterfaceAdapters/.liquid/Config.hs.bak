{--
    TBD: Use Efects Library 
--}

module InterfaceAdapters.Config (
  Config (..)
, Backend (..)
, Frontend (..)
, GeoEnd (..)
, Gateway (..)
, loadConfig
) where

-- | global application configuration
data Config = Config {
  port        :: Int     -- ^ the port where the server is listening
, backend     :: Backend -- ^ selects the weather source
, frontend    :: Frontend  -- ^ the path to UI channel
, geoend      :: GeoEnd
, gateway     :: Gateway  -- ^ api calls through 
, verbose     :: Bool    -- ^ True enables logging
}

data Backend = PirateWeather | OpenWeather deriving (Show, Eq)
data Frontend = Telegram | Signal | Web | Whatsapp | CL deriving (Show, Eq)
data GeoEnd = PositionStack | OpenCage deriving (Show, Eq)
data Gateway  = AWSAPIRest | Other deriving (Show, Eq)

-- | load application config. This would load a config file or read commandline args or use AWS Paramter Store?
-- | port, verbose are unused
loadConfig :: IO Config
loadConfig = return Config {port = 8080, backend = PirateWeather, frontend = Telegram, geoend = OpenCage, gateway = AWSAPIRest,  verbose = True}

