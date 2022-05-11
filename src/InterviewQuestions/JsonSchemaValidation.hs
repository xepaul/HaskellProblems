{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}

module InterviewQuestions.JsonSchemaValidation where

import Control.Monad (Monad ((>>=)), mapM_, unless)
import Data.Either (Either (..))
import Data.Either.Combinators (maybeToRight)
import Data.Eq (Eq((==)))
import Data.Foldable (traverse_)
import Data.Function (($))
import Data.Int (Int)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (Maybe (..))
import Data.Monoid ((<>))
import Data.String (String)
import GHC.Show (Show (show))

data JSONSchema
  = JsObject (Map String JSONSchema)
  | JsString
  | JsMayInt
  | JsList JSONSchema
  deriving (Show)

data JSONValue
  = JvObject (Map String JSONValue)
  | JvString String
  | JvMayInt (Maybe Int)
  | JvList [JSONValue]
  deriving (Show)

-- | validate a Json value matches the given schema
validate :: JSONSchema -> JSONValue -> Either String ()
validate JsString (JvString _) = Right ()
validate JsMayInt (JvMayInt _) = Right ()
validate (JsObject ms) (JvObject mv) =
  traverse_
    ( \(k, v) -> do
        -- ensure all properties exist and not extra properties
        let errorMsg = "Properties don't match schema props:" 
                    <> show (Map.keys ms) <> " value props:" 
                    <> show (Map.keys mv) <> " " 
        unless (Map.keys ms == Map.keys mv) $ Left errorMsg
        -- avoid using partial functions, key is already verified to be in the Map
        maybeToRight (errorMsg <> " Prop not found: " <> k) (Map.lookup k mv) 
          >>= validate v 
    )
    (Map.toList ms)
validate (JsList ms) (JvList ys) = mapM_ (validate ms) ys
validate s v = Left $ "schema value mismatch:" <> show s <> ";" <> show v

-- >>> exampleValue
-- >>>  validate exampleSchema exampleValue
-- >>>  validate exampleSchema exampleBad
-- >>>  validate exampleSchema exampleBad2
-- >>>  validate exampleSchema exampleBad3
-- JvObject (fromList [("addresses",JvList [JvObject (fromList [("address",JvString "g")]),JvObject (fromList [("address",JvString "g")])]),("age",JvMayInt Nothing),("name",JvString "g")])
-- Right ()
-- Left "schema value mismatch:JsMayInt;JvString \"s\""
-- Left "Properties don't match schema props:[\"addresses\",\"age\",\"name\"] value props:[\"addresses\",\"age2\",\"name\"] "
-- Left "Properties don't match schema props:[\"address\"] value props:[\"addressg\"] "

exampleSchema :: JSONSchema
exampleSchema = JsObject (Map.fromList [("name", JsString), 
                                   ("age", JsMayInt), 
                                   ("addresses", JsList exampleListofAddresses)
                                   ])

exampleValue :: JSONValue
exampleValue =
  JvObject
    ( Map.fromList
        [ ("name", JvString "g"),
          ("age", JvMayInt Nothing),
          ("addresses", JvList [exampleAddress, exampleAddress])
        ]
    )

exampleListofAddresses :: JSONSchema
exampleListofAddresses = JsObject (Map.fromList [("address", JsString)])

exampleAddress :: JSONValue
exampleAddress =
  JvObject
    ( Map.fromList
        [("address", JvString "g")]
    )

exampleAddressBadAddress :: JSONValue
exampleAddressBadAddress =
  JvObject
    ( Map.fromList
        [("addressg", JvString "g")]
    )

exampleBad :: JSONValue
exampleBad = -- age is the wrong type
  JvObject
    ( Map.fromList
        [ ("name", JvString "g"),
          ("age", JvString "s"),
          ("addresses", JvList [exampleAddress, exampleAddress])
        ]
    )

exampleBad2 :: JSONValue
exampleBad2 =  -- age property missing and age2 property shouldn't exist 
  JvObject
    ( Map.fromList
        [ ("name", JvString "g"),
          ("age2", JvMayInt Nothing), 
          ("addresses", JvList [])
        ]
    )

exampleBad3 :: JSONValue
exampleBad3 = -- list of addresses are all the same type
  JvObject
    ( Map.fromList
        [ ("name", JvString "g"),
          ("age", JvMayInt Nothing),
          ("addresses", JvList [exampleAddress, exampleAddressBadAddress])
        ]
    )