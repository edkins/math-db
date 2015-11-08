module AppIO where

import System.Process
import XMLPipe
import qualified Data.Text as T
import Control.Monad

data CoqProcess =
  CoqProcess {
    send :: XMLSender,
    rcv :: XMLReceiver,
    process :: ProcessHandle
  }

type Processes = CoqProcess
data LogEntry = CoqSend | CoqReceive | ErrorLog deriving Show
type Log = [(LogEntry, T.Text)]

data AppIO a = AppIO (Processes -> IO (Either T.Text a, Log))

instance Monad AppIO where
  AppIO m >>= f = AppIO (\p -> do
    (x, log) <- m p
    case x of
      Left err -> return (Left err, log)
      Right xx -> do
        let AppIO mf = f xx
        (x', log') <- mf p
        return (x', log ++ log'))

  fail str = AppIO (\p -> return (Left $ T.pack str, [(ErrorLog, T.pack str)]))

instance Functor AppIO where
  fmap = liftM

instance Applicative AppIO where
  pure x = AppIO (\p -> return (Right x, []))
  (<*>) = ap

appIO :: IO a -> AppIO a
appIO m = AppIO (\p -> do
  x <- m
  return (Right x, []))

appLog :: LogEntry -> T.Text -> AppIO ()
appLog entry text = AppIO (\p -> return (Right (), [(entry,text)]))

getProcesses :: AppIO Processes
getProcesses = AppIO (\p -> return (Right p, []))

coqSend :: AppIO XMLSender
coqSend = do
  p <- getProcesses
  return (send p)

coqRcv :: AppIO XMLReceiver
coqRcv = do
  p <- getProcesses
  return (rcv p)

runAppIO :: Processes -> AppIO a -> IO (Either T.Text a, Log)
runAppIO p (AppIO m) = m p

