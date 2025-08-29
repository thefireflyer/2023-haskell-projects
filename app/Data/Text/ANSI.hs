module Data.Text.ANSI where

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

-- | ANSI text, control, and escape codes.
-- * https://gist.github.com/ConnerWill/d4b6c776b509add763e17f9f113fd25b
data ANSI
  = ANSIPure String
  | ANSIText [ANSI]
  | ANSIBell
  | ANSIBackSpace
  | ANSIHorzTab
  | ANSILineFeed
  | ANSIVerticalTab
  | ANSIFormFeed
  | ANSICarriageReturn
  | ANSIDelete
  | -- | Limited support.
    ANSIHideCursor
  | -- | Limited support.
    ANSIShowCursor
  | -- | Limited support.
    ANSISaveScreen
  | -- | Limited support.
    ANSIRestoreScreen
  | -- | Limited support.
    ANSIEnableAlt
  | -- | Limited support.
    ANSIDisableAlt
  | ANSIToHome
  | ANSIToPos Int Int
  | ANSIUp Int
  | ANSIDown Int
  | ANSIRight Int
  | ANSILeft Int
  | ANSIToNextLine Int
  | ANSIToPrevLine Int
  | ANSIToCol Int
  | -- | Limited support.
    ANSISavePos
  | -- | Limited support.
    ANSIRestorePos
  | ANSIResetStyles
  | ANSIBold
  | ANSIItalic
  | ANSIUnderline
  | ANSIBlink
  | ANSIReverse
  | ANSIStrikeThrough
  | ANSIForeground ANSIColor
  | ANSIBackground ANSIColor
  deriving (Show, Read)

type ANSIColor = Int

-------------------------------------------------------------------------------

strAnsi :: ANSI -> [Char]
strAnsi (ANSIPure s) = s
strAnsi (ANSIText xs) = concatMap strAnsi xs
strAnsi ANSIHideCursor = "\ESC[?25l"
strAnsi ANSIShowCursor = "\ESC[?25h"
strAnsi ANSISaveScreen = "\ESC[?47h"
strAnsi ANSIRestoreScreen = "\ESC[?47l"
strAnsi ANSIEnableAlt = "\ESC[?1049h"
strAnsi ANSIDisableAlt = "\ESC[?1049l"
strAnsi ANSISavePos = "\ESC 7"
strAnsi ANSIRestorePos = "\ESC 8"
strAnsi ANSIResetStyles = "\ESC[0m"
strAnsi ANSIBold = "\ESC[1m"
strAnsi ANSIItalic = "\ESC[3m"
strAnsi ANSIUnderline = "\ESC[4m"
strAnsi ANSIBlink = "\ESC[5m"
strAnsi ANSIReverse = "\ESC[7m"
strAnsi ANSIStrikeThrough = "\ESC[9m"
strAnsi (ANSIForeground x) = "\ESC[38;5;" <> show x <> "m"
strAnsi (ANSIBackground x) = "\ESC[48;5;" <> show x <> "m"
strAnsi x = error "not implemented for " <> show x

strAnsiPure :: ANSI -> String
strAnsiPure (ANSIPure s) = s
strAnsiPure (ANSIText xs) = concatMap strAnsi xs
strAnsiPure _ = ""

-------------------------------------------------------------------------------

ansiC :: ANSIColor -> String -> ANSI
ansiC x s = ANSIForeground x <> ANSIPure s <> ANSIResetStyles

ansiB :: ANSIColor -> String -> ANSI
ansiB x s = ANSIBackground x <> ANSIPure s <> ANSIResetStyles

ansiI :: String -> ANSI
ansiI s = ANSIItalic <> ANSIPure s <> ANSIResetStyles

-------------------------------------------------------------------------------

instance Semigroup ANSI where
  (<>) :: ANSI -> ANSI -> ANSI
  ANSIText as <> ANSIText bs = ANSIText $ as <> bs
  ANSIText as <> b = ANSIText $ as <> [b]
  a <> ANSIText bs = ANSIText $ a : bs
  a <> b = ANSIText [a,b]

instance Monoid ANSI where
  mempty :: ANSI
  mempty = ANSIText []

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------
