{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Api.BackOffice.Types
  ( module Core.Domain
  )
where

-- Re-export QuizSummary from Core.Domain for backwards compatibility
import           Core.Domain (QuizSummary (..))
