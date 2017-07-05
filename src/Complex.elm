module Complex exposing (..)

type alias Complex =
  { re : Float
  , im : Float
  }

complex : Float -> Float -> Complex
complex a b = { re = a, im = b }

i : Complex
i = Complex 0 1

re : Complex -> Float
re c = c.re

im : Complex -> Float
im c = c.im

mult : Complex -> Complex -> Complex
mult c1 c2 =
  { re = (c1.re * c2.re) - (c1.im * c2.im), im = (c1.re * c2.im) + (c2.re * c1.im) }

exp : Complex -> Complex
exp c = mult (complex (Basics.e ^ (re c)) 0) (complex (Basics.cos (im c)) (Basics.sin (im c)))
