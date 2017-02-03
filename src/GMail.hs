{-# LANGUAGE OverloadedStrings #-}
module GMail
( send
) where

import Data.Maybe
import Data.Text as T (pack)
import Data.Text.Lazy as TL (pack)
import Network.Mail.Client.Gmail (sendGmail)
import Network.Mail.Mime (Address(Address))
import Secrets (gmailPass, gmailUser)

send :: IO ()
send = let gmailPass' = TL.pack gmailPass
           gmailUserT = T.pack gmailUser
           gmailUserTL = TL.pack gmailUser
       in sendGmail gmailUserTL gmailPass' (Address (Just "Kit") gmailUserT) [Address (Just "Kit") gmailUserT] [] [] "CONFIDENTIAL SUBJECT MATTER" "WHAT A NICE BODY" [] 10000000
