module Model.Physics
    (
        Shape (..)
    ) where

type XCenter = Double
type YCenter = Double
type Width = Double
type Height = Double
type Angle = Double
type Radius = Double

data Shape = Rect XCenter YCenter Width Height Angle |
             Circle XCenter YCenter Radius

