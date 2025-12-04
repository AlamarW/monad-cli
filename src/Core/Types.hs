{-# LANGUAGE DeriveGeneric #-}

module Core.Types
  ( Value(..)
  , Record
  , ErrorInfo(..)
  , Pipeline
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Streaming (Stream, Of)

-- | Value represents the different types of data that can flow through the pipeline
-- This enables type-safe pattern matching while maintaining flexibility
data Value
  = VText Text      -- ^ Text/string values
  | VInt Int        -- ^ Integer values
  | VPath FilePath  -- ^ File paths
  | VBool Bool      -- ^ Boolean values
  deriving (Show, Eq, Generic)

-- | Record is a flexible key-value store using Text keys and typed Values
-- Commands can add fields incrementally without breaking downstream consumers
type Record = Map Text Value

-- | ErrorInfo captures error context for explicit error handling
-- Includes what command failed, why it failed, and the input that caused the failure
data ErrorInfo = ErrorInfo
  { errorCommand :: Text        -- ^ Name of the command that produced the error
  , errorMessage :: Text        -- ^ Human-readable error message
  , errorInput   :: Maybe Record -- ^ The input record that caused the error (if available)
  } deriving (Show, Eq, Generic)

-- | Pipeline is a stream of Either ErrorInfo Record
-- - Left ErrorInfo represents an error (short-circuits the pipeline)
-- - Right Record represents successful data flowing through
-- The Stream type enables lazy, memory-efficient processing of large datasets
type Pipeline = Stream (Of (Either ErrorInfo Record)) IO ()
