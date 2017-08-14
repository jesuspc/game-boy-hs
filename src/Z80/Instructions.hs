module Z80.Instructions where

import           Control.Lens
import           Z80.CPU

data Instruction = LDrrnn B8 Register8 Register8
                 | LDrr B8 Register8 Register8
                 | Nop

runInstruction :: Instruction -> FullState -> FullState
runInstruction Nop s            = s & clock . cc +~ 4
runInstruction (LDrr p r1 r2) s = s
