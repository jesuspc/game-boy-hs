{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Z80 where

import qualified MMU
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

data Instruction = NOP
                 | LDrrnn Register8Type Register8Type
                 | LDrrar Register8Type Register8Type Register8Type
                 | INCrr Register8Type Register8Type
                 | INCr Register8Type
                 | DECr Register8Type
                 | LDrn Register8Type
                 deriving (Show)

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

inst :: Opcode -> Instruction
inst 0x00 = NOP
inst 0x01 = LDrrnn B C
inst 0x02 = LDrrar B C A
inst 0x03 = INCrr B C
inst 0x04 = INCr B
inst 0x05 = DECr B
inst 0x06 = LDrn B
inst _    = error "Unknown Opcode"

flagToInt :: Flag -> Int
flagToInt Zero      = 0x80
flagToInt Operation = 0x40
flagToInt HalfCarry = 0x20
flagToInt Carry     = 0x10

runInstruction :: Instruction -> FullState -> FullState
runInstruction inst s = case inst of
  NOP -> s & cpuS . clock . cc +~ 4
  LDrrnn r1 r2 -> s & cpuS . reg . ls r2 %~ const (fst (MMU.rb (s ^. memS) pc'))
                    & cpuS . reg . ls r1 %~ const (fst (MMU.rb (s ^. memS) (pc' + 1)))
                    & cpuS . reg . pc +~ 2
                    & cpuS . clock . cc +~ 12
  LDrrar r1 r2 r3 -> s & memS %~ (\ms -> MMU.wb ms (shift (s ^. cpuS . reg . ls r1) 8 + s ^. cpuS . reg . ls r2) (s ^. cpuS . reg . ls r3))
                       & cpuS . clock . cc +~ 8
  INCrr r1 r2 -> s & cpuS . reg . ls r2 %~ (\v -> (v + 1) .&. 0xFF)
                   & cpuS . reg %~ (\r -> if r ^. ls r2 == 0
                                          then r & ls r1 %~ (\v -> (v + 1) .&. 0xFF)
                                          else r)
                   & cpuS . clock . cc +~ 8
  INCr r -> s & cpuS . reg . ls r +~ 1
              & cpuS . reg . ls r %~ (.&. 0xFF)
              & cpuS . reg  %~ (\rg -> if rg ^. ls r /= 0
                                       then rg & ls F .~ 0
                                       else rg & ls F .~ 0x80)
              & cpuS . clock . cc +~ 4
  DECr r -> s & cpuS . reg . ls r -~ 1
              & cpuS . reg . ls r %~ (.&. 0xFF)
              & cpuS . reg  %~ (\rg -> if rg ^. ls r /= 0
                                       then rg & ls F .~ 0
                                       else rg & ls F .~ 0x80)
              & cpuS . clock . cc +~ 4
  LDrn r -> s & cpuS . reg . ls r %~ const (fst (MMU.rb ms' pc'))
              & cpuS . reg . pc +~ 1
              & cpuS . clock . cc +~ 8
  where
    pc' = view (cpuS . reg . pc) s
    ms' = view memS s

ls :: Register8Type -> Lens' RegisterState Register8
ls A = a
ls B = b
ls C = c
ls D = d
ls E = e
ls H = h
ls L = l
ls F = f
