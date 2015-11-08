module Services where

import Coq
import AppIO

data Services = Services {coqService :: CoqProcess}

startServices :: IO Services
startServices = do
  c <- startCoq
  return Services {coqService = c}

stopServices :: Services -> IO ()
stopServices c = do
  stopCoq (coqService c)
