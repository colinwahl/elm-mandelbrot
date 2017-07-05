module Complex exposing (..)

type alias Complex =
  { re : Float
  , im : Float
  }

repeat : Int -> Complex -> Complex -> Complex
repeat n z c =
  case n of
    0 -> z
    n -> repeat (n-1) (add (mult z z) c) c

complex : Float -> Float -> Complex
complex a b = { re = a, im = b }

i : Complex
i = Complex 0 1

zero : Complex
zero = Complex 0 0

re : Complex -> Float
re c = c.re

im : Complex -> Float
im c = c.im

add : Complex -> Complex -> Complex
add c1 c2 =
  {re = (c1.re + c2.re), im = (c1.im + c2.im)}

arg : Complex -> Float
arg c =
  case (c.re, c.im) of
    (0,0) -> 0
    (x,y) -> atan2 y x


atan2 : comparable -> comparable -> Float
atan2 y x =
  if x > 0 then Basics.atan (y/x)
  else if x == 0 && y > 0 then pi / 2
  else if x < 0 && y > 0 then pi + Basics.atan (y/x)
  else if (x <= 0 && y < 0 ) then 0 - (atan2 (-y) x)
  else if (y == 0 && (x < 0)) then pi
  else if x == 0 && y == 0 then y
  else x+y

abs : Complex -> Float
abs c =
  (c.re^2 + c.im^2)^(0.5)

nln : Complex -> (Int -> Complex)
nln z =
  \k -> {re = logBase (Basics.e) (abs z), im = (arg z) + 2 * Basics.pi * (toFloat k)}

{-| The natrual log of a complex number.
-}

ln : Complex -> Complex
ln = flip nln 0

mult : Complex -> Complex -> Complex
mult c1 c2 =
  { re = (c1.re * c2.re) - (c1.im * c2.im), im = (c1.re * c2.im) + (c2.re * c1.im) }

exp : Complex -> Complex
exp c = mult (complex (Basics.e ^ (re c)) 0) (complex (Basics.cos (im c)) (Basics.sin (im c)))

pow : Complex -> Complex -> Complex
pow z w = if z == zero then zero else  exp (mult w (ln z))
