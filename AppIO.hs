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
data LogEntry = CoqSend | CoqReceive deriving Show
type Log = [(LogEntry, T.Text)]

data AppIO a = AppIO (Processes -> IO (a, Log))

instance Monad AppIO where
  AppIO m >>= f = AppIO (\p -> do
    (x, log) <- m p
    let AppIO mf = f x
    (x', log') <- mf p
    return (x', log ++ log'))

instance Functor AppIO where
  fmap = liftM

instance Applicative AppIO where
  pure x = AppIO (\p -> return (x, []))
  (<*>) = ap

appIO :: IO a -> AppIO a
appIO m = AppIO (\p -> do
  x <- m
  return (x, []))

appLog :: LogEntry -> T.Text -> AppIO ()
appLog entry text = AppIO (\p -> return ((), [(entry,text)]))

coqSend :: AppIO XMLSender
coqSend = AppIO (\p -> return (send p, []))

coqRcv :: AppIO XMLReceiver
coqRcv = AppIO (\p -> return (rcv p, []))

runAppIO :: Processes -> AppIO a -> IO (a, Log)
runAppIO p (AppIO m) = m p

