module Color

import Utils

||| The 8 standard colors.
public export
data Color =
    Black         |
    Red           |
    Green         |
    Yellow        |
    Blue          |
    Magenta       |
    Cyan          |
    White         |
    BrightBlack   |
    BrightRed     |
    BrightGreen   |
    BrightYellow  |
    BrightBlue    |
    BrightMagenta |
    BrightCyan    |
    BrightWhite

export
toFgStr : Color -> String
toFgStr Color.Black = "30"
toFgStr Color.Red = "31"
toFgStr Color.Green = "32"
toFgStr Color.Yellow = "33"
toFgStr Color.Blue = "34"
toFgStr Color.Magenta = "35"
toFgStr Color.Cyan = "36"
toFgStr Color.White = "37"
toFgStr Color.BrightBlack = "90"
toFgStr Color.BrightRed = "91"
toFgStr Color.BrightGreen = "92"
toFgStr Color.BrightYellow = "93"
toFgStr Color.BrightBlue = "94"
toFgStr Color.BrightMagenta = "95"
toFgStr Color.BrightCyan = "96"
toFgStr Color.BrightWhite = "97"

export
toBgStr : Color -> String
toBgStr Color.Black = "40"
toBgStr Color.Red = "41"
toBgStr Color.Green = "42"
toBgStr Color.Yellow = "43"
toBgStr Color.Blue = "44"
toBgStr Color.Magenta = "45"
toBgStr Color.Cyan = "46"
toBgStr Color.White = "47"
toBgStr Color.BrightBlack = "100"
toBgStr Color.BrightRed = "101"
toBgStr Color.BrightGreen = "102"
toBgStr Color.BrightYellow = "103"
toBgStr Color.BrightBlue = "104"
toBgStr Color.BrightMagenta = "105"
toBgStr Color.BrightCyan = "106"
toBgStr Color.BrightWhite = "107"

public export
FromStr Color () where
  parse str = case trim (toLower str) of
    "black" => Right Color.Black
    "red" => Right Color.Red
    "green" => Right Color.Green
    "yellow" => Right Color.Yellow
    "blue" => Right Color.Blue
    "magenta" => Right Color.Magenta
    "cyan" => Right Color.Cyan
    "white" => Right Color.White
    "bright black" => Right Color.BrightBlack
    "bright red" => Right Color.BrightRed
    "bright green" => Right Color.BrightGreen
    "bright yellow" => Right Color.BrightYellow
    "bright blue" => Right Color.BrightBlue
    "bright magenta" => Right Color.BrightMagenta
    "bright cyan" => Right Color.BrightCyan
    "bright white" => Right Color.BrightWhite
    _ => Left ()

public export
FromStr Color () => Cast String Color where
  cast str = case the (Either () Color) (parse str) of
    Right color => color
    Left _ => Color.White

public export
Eq Color where
  (==) Black         Black         = True
  (==) Red           Red           = True
  (==) Green         Green         = True
  (==) Yellow        Yellow        = True
  (==) Blue          Blue          = True
  (==) Magenta       Magenta       = True
  (==) Cyan          Cyan          = True
  (==) White         White         = True
  (==) BrightBlack   BrightBlack   = True
  (==) BrightRed     BrightRed     = True
  (==) BrightGreen   BrightGreen   = True
  (==) BrightYellow  BrightYellow  = True
  (==) BrightBlue    BrightBlue    = True
  (==) BrightMagenta BrightMagenta = True
  (==) BrightCyan    BrightCyan    = True
  (==) BrightWhite   BrightWhite   = True
  (==) _             _             = False

