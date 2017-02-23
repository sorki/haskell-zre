import Data.Binary.Put

 -- zre hello message packing
genZre = BL.toStrict $ runPut $ do
  putWord16be zreSig
  putWord8 0x01 -- cmd
  putInt8 $ fromIntegral zreVer
  putWord16be 1337
  putByteStringLen "endpoint:1337"
  putByteStrings ["group1", "group2"]
  putInt8 7
  putByteStringLen "name"
  putMap (M.fromList [("abc", "val1"), ("qqq", "val2")])
