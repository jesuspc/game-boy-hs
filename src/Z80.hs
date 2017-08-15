module Z80 where

import           Z80.CPU
import           Z80.Instructions

import           Data.Bits

reset :: FullState
reset = FullState istate cstate
  where
    istate = RegisterState
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
      }
    cstate = ClockState { _cc = 0 }

exec :: FullState -> FullState
exec s = undefined

flagToInt :: Flag -> Int
flagToInt Zero      = 0x80
flagToInt Operation = 0x40
flagToInt HalfCarry = 0x20
flagToInt Carry     = 0x10
