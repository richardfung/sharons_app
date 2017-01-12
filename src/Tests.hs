

testGetAuctionFiles :: IO ()
testGetAuctionFiles = getAuctionFiles

testGetAuction :: IO ()
testGetAuction = do
    af <- getAuctionFiles
    print af
    a <- getAuction $ B.pack $ url $ head $ files af
    print a
