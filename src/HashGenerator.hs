module HashGenerator
  ( 
    HashFunction(..)
  ,HashGenerator(..)
  ) where

import Crypto.Hash
import Data.ByteString(ByteString)
import Data.ByteString.Char8(pack)

data HashFunction=MD5_F|SHA1_F|SHA256_F|SHA512_F


class HashGenerator a where
  calculate::a->ByteString->ByteString



instance HashGenerator HashFunction where
  calculate=calculateHash

instance Eq HashFunction where
  (==) MD5_F MD5_F=True
  (==) SHA1_F SHA1_F=True
  (==) SHA256_F SHA256_F=True
  (==) SHA512_F SHA512_F=True
  (==) _ _=False

md5Hash::ByteString->String
md5Hash input=show (hash input::Digest MD5)


sha1Hash::ByteString->String
sha1Hash input=show (hash input::Digest SHA1)

sha256Hash::ByteString->String
sha256Hash input=show (hash input::Digest SHA256)


sha512Hash::ByteString->String
sha512Hash input=show (hash input::Digest SHA512)



calculateHash::HashFunction->ByteString->ByteString
calculateHash hashF input
  |hashF==MD5_F=pack $ md5Hash input
  |hashF==SHA1_F=pack $ sha1Hash input
  |hashF==SHA256_F=pack $ sha256Hash input
  |hashF==SHA512_F=pack $ sha512Hash input
