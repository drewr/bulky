{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Map as Map
import Data.Aeson (Object, ToJSON, encode, decode, (.=),
                   object, toJSON)
import Data.Aeson.TH (deriveJSON, defaultOptions,
                      fieldLabelModifier, omitNothingFields)

type Payload = BS.ByteString

data Action = IndexAction ActionMeta Payload
            | CreateAction ActionMeta Payload
            | UpdateAction ActionMeta Payload
            | DeleteAction ActionMeta
            deriving (Show)

data ActionMeta = ActionMeta { _index :: Maybe Text.Text
                             , _type :: Maybe Text.Text
                             , _id :: Maybe Text.Text
                             }
                deriving (Show)

data ActionL = ActionL Text.Text ActionMeta

$(deriveJSON
  defaultOptions { fieldLabelModifier = id
                 , omitNothingFields = True } ''ActionMeta)

data BulkyOpts = BulkyOpts
                 { }

instance ToJSON ActionL where
  toJSON (ActionL k v) = object [k .= v]

class ToBulk a where
  toBulk :: a -> BS.ByteString

instance ToBulk Action where
  toBulk (IndexAction meta payload) =
    BS.concat [(encode $ ActionL "index" meta), BS.pack "\n", payload]
  toBulk (CreateAction meta payload) =
    BS.concat [(encode $ ActionL "create" meta), BS.pack "\n", payload]
  toBulk (UpdateAction meta payload) =
    BS.concat [(encode $ ActionL "update" meta), BS.pack "\n", payload]
  toBulk (DeleteAction meta) =
    encode $ ActionL "delete" meta

actionify :: [String] -> [Action]
actionify [] = []
actionify (x:xs) =
  case action of
    "create" -> let (payload:ys) = xs
                in CreateAction meta (BS.pack payload) : actionify ys
    "index" -> let (payload:ys) = xs
               in IndexAction meta (BS.pack payload) : actionify ys
    "update" -> let (payload:ys) = xs
                in UpdateAction meta (BS.pack payload) : actionify ys
    "delete" -> (DeleteAction meta) : (actionify xs)
  where (Just actionM) =
          decode (BS.pack x) :: Maybe (Map.Map Text.Text ActionMeta)
        ((action,meta):_) = Map.toList actionM

bulky :: String -> String
bulky input = unlines $ map (BS.unpack . toBulk) $ actionify $ lines input

main :: IO ()
main = interact bulky
