{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RankNTypes #-}

module Z80.Instructions where

import           Control.Lens
import           Data.Bits
import           Internal
import           Z80.CPU

data Instruction = LDrrnn B8 Register8Type Register8Type
                 | LDrr B8 Register8Type Register8Type
                 | NOP
                 | INCrr Register8Type Register8Type

runInstruction :: Instruction -> FullState -> FullState
runInstruction inst s = case inst of
  NOP         -> s & clock . cc +~ 4
  INCrr r1 r2 -> s & reg . ls r2 %~ (\v -> (v + 1) .&. 0xFF)
                   & reg %~ (\r -> if r ^. ls r2 == 0
                                   then r & ls r1 %~ (\v -> (v + 1) .&. 0xFF)
                                   else r)
                   & clock . cc +~ 8
  _ -> s

ls :: Register8Type -> Lens' RegisterState Register8
ls A = a
ls B = b
ls C = c
ls D = d
ls E = e
ls H = h
ls L = l
ls F = f
