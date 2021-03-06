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

    either displayError renderJSON . parsePostBody =<< S.body
  where
    parsePostBody = parseGraphs . TL.toStrict . E.decodeUtf8
    displayError = render422 . TL.pack

render422 :: TL.Text -> AppAction ()
render422 t = do
    S.status unprocessableEntity422
    renderJSON $ A.object [ "error" A..= t ]

renderJSON :: (Monad m, A.ToJSON a) => a -> S.ActionT TL.Text m ()
renderJSON = S.text . E.decodeUtf8 . A.encode
