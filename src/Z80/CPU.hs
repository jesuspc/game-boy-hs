{-# LANGUAGE TemplateHaskell #-}

module Z80.CPU where

import           Control.Lens

type Register8 = Int
type Register16 = Int
type RegisterT = Register8
type Clock = Int
type B8 = Int -- 8bits

data Flag = Zero
          | Operation
          | HalfCarry
          | Carry
          deriving (Show)

data RegisterState = RegisterState
     { _a  :: Register8,
       _b  :: Register8,
       _c  :: Register8,
       _d  :: Register8,
       _e  :: Register8,
       _h  :: Register8,
       _l  :: Register8,
       _f  :: Register8,
       _pc :: Register16,
       _sp :: Register16
     } deriving (Show)
makeLenses ''RegisterState

data ClockState = ClockState
                  {
                    _cc :: Clock
                  } deriving (Show)
makeLenses ''ClockState

data FullState = FullState
                 {
                   _reg   :: RegisterState,
                   _clock :: ClockState
                 } deriving (Show)
makeLenses ''FullState
