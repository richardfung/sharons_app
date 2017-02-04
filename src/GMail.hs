{-# LANGUAGE OverloadedStrings #-}
module GMail
( sendEmail
) where

import Data.Maybe
import Data.Text as T (pack)
import Data.Text.Lazy as TL (pack)
import Network.Mail.Client.Gmail (sendGmail)
import Network.Mail.Mime (Address(Address))
import Secrets (gmailPass, gmailUser)

sendEmail :: String -> IO ()
sendEmail body =
    let gmailPass' = TL.pack gmailPass
        gmailUserT = T.pack gmailUser
        gmailUserTL = TL.pack gmailUser
    in sendGmail gmailUserTL gmailPass' (Address (Just "Kit") gmailUserT) [Address (Just "Kit") gmailUserT] [] [] "Notification from Sharon's App!" (TL.pack body) [] 10000000
