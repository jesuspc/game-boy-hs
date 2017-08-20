{-# LANGUAGE TemplateHaskell #-}

module MMU (rb) where--, rw, wb, ww) where

import           Control.Lens
import           Data.Bits
import           Internal

type Address = Int
type RomImage = String

data Memory = Memory {
  _rom     :: RomImage,
  _romOffs :: Int,
  _ramOffs :: Int,
  _wram    :: [Int],
  _eram    :: [Int]
  }
makeLenses ''Memory

rb :: Memory -> Address -> (B8, Memory)
rb m addr
  -- Bank 0
  | masked == 0x0000 = (fromEnum $ (m ^. rom) !! addr, m) -- Missing bios handling
  | masked `elem` [0x1000, 0x2000, 0x3000] = (fromEnum $ (m ^. rom) !! addr, m)
  -- Bank 1
  | masked `elem` [0x4000, 0x5000, 0x6000, 0x7000] = (fromEnum $ (m ^. rom) !! (m ^. romOffs + addr .&. 0x3FFF), m)
  -- VRAM
  | masked `elem` [0x8000, 0x9000] = undefined -- GPU
  -- External RAM
  | masked `elem` [0xA000, 0xB000] = ((m ^. eram) !! (m ^. ramOffs + addr .&. 0x1FFF), m)
  -- Work RAM and echo
  | masked `elem` [0xC000, 0xD000, 0xE000] = ((m ^. wram) !! (addr .&. 0x1FFF), m)
  -- Else
  | masked == 0xF000 = handleElse
  | otherwise = error "boom"
  where
    masked = addr .&. 0xF000
    handleElse
      -- Echo RAM
      | masked' `elem` [
          0x000, 0x100, 0x200, 0x300, 0x400, 0x500,
          0x600, 0x700, 0x800, 0x900, 0xA00, 0xB00,
          0xC00, 0xD00] = ((m ^. wram) !! addr .&. 0x1FFF, m)
      -- OAM
      | masked' == 0xE00 = undefined -- GPU
      -- Zeropage RAM, IO, interrupts
      | otherwise = undefined
      where
        masked' = addr .&. 0x0F00
