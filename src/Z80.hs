{-# LANGUAGE TemplateHaskell #-}

module Z80 where

import           Control.Lens
import           Data.Bits

type Register8 = Int
type Register16 = Int
type RegisterT = Register8
type Clock = Int

data Flag = Zero
          | Operation
          | HalfCarry
          | Carry
          deriving (Show)

data IState = IState
     { _a  :: Register8,
       _b  :: Register8,
       _c  :: Register8,
       _d  :: Register8,
       _e  :: Register8,
       _h  :: Register8,
       _l  :: Register8,
       _f  :: Register8,
       _pc :: Register16,
       _sp :: Register16,
       _m  :: RegisterT,
       _t  :: RegisterT
     } deriving (Show)
makeLenses ''IState

data ClockState = ClockState
                  {
                    _cm :: Clock,
                    _ct :: Clock
                  } deriving (Show)
makeLenses ''ClockState

data FullState = FullState
                 {
                   _regs   :: IState,
                   _clocks :: ClockState
                 } deriving (Show)
makeLenses ''FullState

flagToInt :: Flag -> Int
flagToInt Zero      = 0x80
flagToInt Operation = 0x40
flagToInt HalfCarry = 0x20
flagToInt Carry     = 0x10

reset :: FullState
reset = FullState istate cstate
  where
    istate = IState
      { _a = 0
      , _b = 0
      , _c = 0
      , _d = 0
      , _e = 0
      , _h = 0
      , _l = 0
      , _f = 0
      , _pc = 0
      , _sp = 0
      , _m = 0
      , _t = 0
      }
    cstate = ClockState { _cm = 0, _ct = 0 }

addrE :: IState -> IState
addrE s = s
          & a .~ s ^. e
          & f .~ 0
          & f %~ zeroCheck
          & f %~ carryCheck
          & m .~ 1
          & t .~ 4
  where
    zeroCheck x
      | s ^. a .&. 255 == 0 = x .|. flagToInt Zero
      | otherwise = x
    carryCheck x
      | s ^. a > 255 = x .|. flagToInt Carry
      | otherwise = x
