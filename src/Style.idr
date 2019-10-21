module Style

import Data.Vect
import Data.Bits

and: machineTy Z -> machineTy Z -> machineTy Z
and x y = Bits.and' {n=Z} x y

b8: Bits8 -> Bits8
b8 b = b

CLEARV: Bits8
CLEARV = b8 0x00

BOLD: Bits8
BOLD= b8 0x01

UNDERLINE: Bits8
UNDERLINE = b8 0x02

REVERSED: Bits8
REVERSED = b8 0x04

ITALIC: Bits8
ITALIC = b8 0x06

BLINK: Bits8
BLINK = b8 0x10

HIDDEN: Bits8
HIDDEN = b8 0x20

DIMMED: Bits8
DIMMED = b8 0x40

STRIKETHROUGH: Bits8
STRIKETHROUGH = b8 0x60

-- #[derive(Clone, Copy, PartialEq, Eq, Debug)]
public export
data StyleAttribute =
    Clear         |
    Bold          |
    Dimmed        |
    Underline     |
    Reversed      |
    Italic        |
    Blink         |
    Hidden        |
    Strikethrough

public export
Show StyleAttribute where
  show Clear = ""
  show Bold = "1"
  show Dimmed = "2"
  show Italic = "3"
  show Underline = "4"
  show Blink = "5"
  show Reversed = "7"
  show Hidden = "8"
  show Strikethrough = "9"

public export
Cast StyleAttribute Bits8 where
  cast Clear = CLEARV
  cast Bold = BOLD
  cast Dimmed = DIMMED
  cast Italic = ITALIC
  cast Underline = UNDERLINE
  cast Blink = BLINK
  cast Reversed = REVERSED
  cast Hidden = HIDDEN
  cast Strikethrough = STRIKETHROUGH

STYLES: List (Bits8, StyleAttribute)
STYLES = [
  (BOLD, Bold),
  (DIMMED, Dimmed),
  (UNDERLINE, Underline),
  (REVERSED, Reversed),
  (ITALIC, Italic),
  (BLINK, Blink),
  (HIDDEN, Hidden),
  (STRIKETHROUGH, Strikethrough)
]

namespace StyleAttribute
  from_bits8: Bits8 -> Maybe (List StyleAttribute)
  from_bits8 b =
    if b == CLEARV then Nothing
    else
      let
        res = filter (\(mask, _) => (and b mask) /= (b8 0)) STYLES
        res' = map (\(_, value) => value) res
      in
        if isNil res' then Nothing
        else Just res'

public export
record Style where
  constructor MkStyle
  content: Bits8

export
CLEAR: Style
CLEAR = MkStyle CLEARV

-- impl Style {
--     pub fn to_str(self) -> String {
--         let styles = StyleAttribute::from_u8(self.0).unwrap_or(Vec::new());
--         styles
--             .iter()
--             .map(|s| s.to_str())
--             .collect::<Vec<&str>>()
--             .join(";")
--     }

--     pub fn new(from: StyleAttribute) -> Style {
--         Style(from.to_u8())
--     }

--     pub fn from_both(one: Style, two: StyleAttribute) -> Style {
--         Style(one.0 | two.to_u8())
--     }
-- }

-- #[cfg(test)]
-- mod tests {

--     mod u8_to_styles_invalid_is_none {
--         use super::super::CLEARV;
--         use super::super::{Style, StyleAttribute};

--         #[test]
--         fn empty_is_none() {
--             assert_eq!(None, StyleAttribute::from_u8(CLEARV))
--         }

--     }

--     mod u8_to_styles_isomorphism {
--         use super::super::StyleAttribute;
--         use super::super::{
--             BLINK, BOLD, DIMMED, HIDDEN, ITALIC, REVERSED, STRIKETHROUGH, UNDERLINE,
--         };

--         macro_rules! value_isomorph {
--             ($name:ident, $value:expr) => {
--                 #[test]
--                 fn $name() {
--                     let u = StyleAttribute::from_u8($value);
--                     assert!(
--                         u.is_some(),
--                         "{}: StyleAttribute::from_u8 -> None",
--                         stringify!($value)
--                     );
--                     let u = u.unwrap();
--                     assert!(
--                         u.len() == 1,
--                         "{}: StyleAttribute::from_u8 found {} styles (expected 1)",
--                         stringify!($value),
--                         u.len()
--                     );
--                     assert!(
--                         u[0].to_u8() == $value,
--                         "{}: to_u8() doesn't match its const value",
--                         stringify!($value)
--                     );
--                 }
--             };
--         }

--         value_isomorph!(bold, BOLD);
--         value_isomorph!(underline, UNDERLINE);
--         value_isomorph!(reversed, REVERSED);
--         value_isomorph!(italic, ITALIC);
--         value_isomorph!(blink, BLINK);
--         value_isomorph!(hidden, HIDDEN);
--         value_isomorph!(dimmed, DIMMED);
--         value_isomorph!(strikethrough, STRIKETHROUGH);
--     }

--     mod styles_combine_complex {
--         use super::super::StyleAttribute::*;
--         use super::super::{Style, StyleAttribute};
--         use super::super::{
--             BLINK, BOLD, DIMMED, HIDDEN, ITALIC, REVERSED, STRIKETHROUGH, UNDERLINE,
--         };

--         fn style_from_multiples(styles: &[StyleAttribute]) -> Style {
--             let mut res = Style::new(styles[0]);
--             for s in &styles[1..] {
--                 res = Style::from_both(res, *s)
--             }
--             res
--         }

--         macro_rules! test_aggreg {
--             ($styles:expr, $expect:expr) => {{
--                 let v = style_from_multiples($styles);
--                 let r = StyleAttribute::from_u8(v.0).expect("should find styles");
--                 assert_eq!(&$expect as &[StyleAttribute], &r[..])
--             }};
--         }

--         #[test]
--         fn aggreg1() {
--             let styles: &[StyleAttribute] = &[Bold, Bold, Bold];
--             test_aggreg!(styles, [Bold])
--         }

--         #[test]
--         fn aggreg2() {
--             let styles: &[StyleAttribute] = &[Italic, Italic, Bold, Bold];
--             test_aggreg!(styles, [Bold, Italic])
--         }

--         #[test]
--         fn aggreg3() {
--             let styles: &[StyleAttribute] = &[Bold, Italic, Bold];
--             test_aggreg!(styles, [Bold, Italic])
--         }

--         macro_rules! test_combine {
--             ($styles:expr) => {{
--                 let v = style_from_multiples($styles);
--                 let r = StyleAttribute::from_u8(v.0).expect("should find styles");
--                 assert_eq!($styles, &r[..])
--             }};
--         }

--         #[test]
--         fn two1() {
--             let s: &[StyleAttribute] = &[Bold, Underline];
--             test_combine!(s)
--         }

--         #[test]
--         fn two2() {
--             let s: &[StyleAttribute] = &[Underline, Italic];
--             test_combine!(s)
--         }

--         #[test]
--         fn two3() {
--             let s: &[StyleAttribute] = &[Bold, Italic];
--             test_combine!(s)
--         }

--         #[test]
--         fn three1() {
--             let s: &[StyleAttribute] = &[Bold, Underline, Italic];
--             test_combine!(s)
--         }

--         #[test]
--         fn three2() {
--             let s: &[StyleAttribute] = &[Dimmed, Underline, Italic];
--             test_combine!(s)
--         }

--         #[test]
--         fn four() {
--             let s: &[StyleAttribute] = &[Dimmed, Underline, Italic, Hidden];
--             test_combine!(s)
--         }

--         #[test]
--         fn five() {
--             let s: &[StyleAttribute] = &[Dimmed, Underline, Italic, Blink, Hidden];
--             test_combine!(s)
--         }

--         #[test]
--         fn six() {
--             let s: &[StyleAttribute] = &[Bold, Dimmed, Underline, Italic, Blink, Hidden];
--             test_combine!(s)
--         }

--         #[test]
--         fn all() {
--             let s: &[StyleAttribute] = &[
--                 Bold,
--                 Dimmed,
--                 Underline,
--                 Reversed,
--                 Italic,
--                 Blink,
--                 Hidden,
--                 Strikethrough,
--             ];
--             test_combine!(s)
--         }

--     }
-- }
