{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Z80 where

import qualified MMU          as MMU
import           Z80.CPU

import           Control.Lens

import           Data.Bits
import           Data.Word
import           Internal

type Opcode = Word8
type CycleCount = Int
type ArgCount = Int

data FullState = FullState { _cpuS :: CpuState, _memS :: MMU.Memory }
makeLenses ''FullState

reset :: CpuState
reset = CpuState istate cstate
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

exec :: CpuState -> CpuState
exec s = undefined

inst :: Opcode -> (Word8, Word16) -> (Instruction, CycleCount, ArgCount)
inst 0x00 _ = (NOP, 4, 0)
inst 0x01 _ = (LDrrnn B C, 12, 2)
inst 0x03 _ = (INCrr B C, 8, 0)
inst _ _    = error "Unknown Opcode"

flagToInt :: Flag -> Int
flagToInt Zero      = 0x80
flagToInt Operation = 0x40
flagToInt HalfCarry = 0x20
flagToInt Carry     = 0x10

data Instruction = NOP
                 | LDrrnn Register8Type Register8Type
                 | LDrrar Register8Type Register8Type Register8Type
                 | INCrr Register8Type Register8Type
                 | LDrr B8 Register8Type Register8Type

runInstruction :: Instruction -> FullState -> FullState
runInstruction inst s = case inst of
  NOP -> s & cpuS . clock . cc +~ 4
  LDrrnn r1 r2 -> s & cpuS . reg . ls r2 %~ const (fst (MMU.rb (s ^. memS) pc'))
                    & cpuS . reg . ls r1 %~ const (fst (MMU.rb (s ^. memS) (pc' + 1)))
                    & cpuS . reg . pc +~ 2
                    & cpuS . clock . cc +~ 12
  LDrrar r1 r2 r3 -> undefined
  INCrr r1 r2 -> s & cpuS . reg . ls r2 %~ (\v -> (v + 1) .&. 0xFF)
                   & cpuS . reg %~ (\r -> if r ^. ls r2 == 0
                                   then r & ls r1 %~ (\v -> (v + 1) .&. 0xFF)
                                   else r)
                   & cpuS . clock . cc +~ 8
  _   -> s
  where
    pc' = view (cpuS . reg . pc) s

ls :: Register8Type -> Lens' RegisterState Register8
ls A = a
ls B = b
ls C = c
ls D = d
ls E = e
ls H = h
ls L = l
ls F = f
