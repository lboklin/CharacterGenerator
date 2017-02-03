-- | TODO: Convert Traits data type into Trait,
--      with the fields as value constructors.

module Character.Traits
( Traits(..)
) where

data Traits = Traits
    { fearlessness       :: !Int
    , communication      :: !Int
    , determination      :: !Int
    , confidence         :: !Int
    , reactionQuickness  :: !Int
    , fineMotorSkills    :: !Int
    , criticalThinking   :: !Int
    , logicalReasoning   :: !Int
    , patternRecognition :: !Int
    , attention          :: !Int
    , mentalEndurance    :: !Int
    , selfControl        :: !Int
    , emotionalStability :: !Int
    } deriving (Show, Read)

