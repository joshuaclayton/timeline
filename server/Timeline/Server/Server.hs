module Timeline.Server.Server
    ( runApp
    ) where

import           Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Encoding as E
import           Network.HTTP.Types.Status (unprocessableEntity422)
import qualified Network.Wai.Handler.Warp as W
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Timeline
import           Timeline.Server.Json ()
import qualified Web.Scotty.Trans as S

type AppAction = S.ActionT TL.Text AppConfig
type AppConfig = ReaderT Int IO

runApp :: Int -> IO ()
runApp port = S.scottyOptsT (S.Options 1 $ settings port) (`runReaderT` port) app'

settings :: Int -> W.Settings
settings port = W.setPort port W.defaultSettings

app' :: S.ScottyT TL.Text AppConfig ()
app' = do
    S.middleware logStdoutDev
    S.post "/" rootA

rootA :: AppAction ()
rootA = do
    S.addHeader "Access-Control-Allow-Origin" "*"
    S.addHeader "Access-Control-Allow-Methods" "GET, POST"

    either (render422 . TL.pack) (S.text . E.decodeUtf8 . A.encode) . parseGraphs . TL.toStrict . E.decodeUtf8 =<< S.body

render422 :: TL.Text -> AppAction ()
render422 t = do
    S.status unprocessableEntity422
    S.text $ E.decodeUtf8 $ A.encode $ A.object [ "error" A..= t ]
