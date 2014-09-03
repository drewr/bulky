{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as M
import Data.Aeson (Value(..), ToJSON, encode, eitherDecode, decode, (.=), object, toJSON)
import Data.Aeson.TH (deriveJSON, defaultOptions,
                      fieldLabelModifier, omitNothingFields)

type Payload = BS.ByteString
type ActionName = Text.Text

data Action = IndexAction ActionMeta Payload
            | CreateAction ActionMeta Payload
            | UpdateAction ActionMeta Payload
            | DeleteAction ActionMeta
            | InvalidAction String
            deriving (Show)

data ActionMeta = ActionMeta (Map.Map Text.Text Value)
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

decodeAction :: String -> Maybe (ActionName, ActionMeta)
decodeAction x = 
  case thing of
    (Just actionM) -> let l = Map.toList actionM
                      in if length l > 0
                         then Just $ head l
                         else Nothing
    Nothing -> Nothing
  where
    thing = decode (BS.pack x) :: Maybe (Map.Map Text.Text ActionMeta)
  
actionify :: [String] -> [Action]
actionify [] = []
actionify (x:xs) =
  case action of
    (Just ("create", meta)) -> let (payload:ys) = xs
                               in CreateAction meta (BS.pack payload) : actionify ys
    (Just ("index", meta)) -> let (payload:ys) = xs
                              in IndexAction meta (BS.pack payload) : actionify ys
    (Just ("update", meta)) -> let (payload:ys) = xs
                               in UpdateAction meta (BS.pack payload) : actionify ys
    (Just ("delete", meta)) -> (DeleteAction meta) : (actionify xs)
    otherwise -> actionify xs
  where action = decodeAction x

bulky :: String -> String
bulky input = unlines
            . map (BS.unpack . toBulk)
            . actionify
            . filter ((> 0) . length)
            . lines
            $ input

main :: IO ()
main = interact bulky
