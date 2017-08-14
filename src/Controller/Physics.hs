module Controller.Physics
    (
        collide
    ) where

import Model.Physics

import Misc

data V2D = V2D (Float,Float)
    deriving Show

collide :: Shape -> Shape -> Bool
collide (Circle x1 y1 r1) (Circle x2 y2 r2) =
    (x1 - x2)^2 + (y1 - y2)^2 <= r1^2 + r2^2

collide r@(Rect _ _ _ _ _) c@(Circle _ _ _) = collide c r
collide (Circle x1 y1 r) (Rect x2 y2 w h _) =
    (closestX - x1)^2 + (closestY - y1)^2 <= r^2

    where closestX = clamp x1 (x2 - w / 2) (x2 + w / 2)
          closestY = clamp y1 (y2 - h / 2) (y2 + h / 2)

collide r1@(Rect x1 y1 w1 h1 a1) r2@(Rect x2 y2 w2 h2 a2)
    | a1 == 0 && a2 == 0 = 
        x1 - w1 / 2 <= x2 + w2 / 2 &&
        x1 + w1 / 2 >= x2 - w2 / 2 &&
        y1 - h1 / 2 <= y2 + h2 / 2 &&
        y1 + h1 / 2 >= y2 - h2 / 2
    | otherwise = collidez

    where   -- Find corner's coords, treat as vector's
      -- UL: upper left, UR: upper right, BL: bottom left, BR: bottom right

      corner_UL1 = V2D (rtf $ x1 + hyp1*cos(aUL1), rtf $ y1 + hyp1*sin(aUL1))
      corner_UR1 = V2D (rtf $ x1 + hyp1*cos(aUR1), rtf $ y1 + hyp1*sin(aUR1))
      corner_BL1 = V2D (rtf $ x1 + hyp1*cos(aBL1), rtf $ y1 + hyp1*sin(aBL1))
      corner_BR1 = V2D (rtf $ x1 + hyp1*cos(aBR1), rtf $ y1 + hyp1*sin(aBR1))

      corner_UL2 = V2D (rtf $ x2 + hyp2*cos(aUL2), rtf $ y2 + hyp2*sin(aUL2))
      corner_UR2 = V2D (rtf $ x2 + hyp2*cos(aUR2), rtf $ y2 + hyp2*sin(aUR2))
      corner_BL2 = V2D (rtf $ x2 + hyp2*cos(aBL2), rtf $ y2 + hyp2*sin(aBL2))
      corner_BR2 = V2D (rtf $ x2 + hyp2*cos(aBR2), rtf $ y2 + hyp2*sin(aBR2))



      aUL1 = atan(-h1/w1) + pi + a1
      aUR1 = atan(h1/w1) + a1
      aBL1 = atan(h1/w1) + pi + a1
      aBR1 = atan(-h1/w1) + a1

      aUL2 = atan(-h2/w2) + pi + a2
      aUR2 = atan(h2/w2) + a2
      aBL2 = atan(h2/w2) + pi + a2
      aBR2 = atan(-h2/w2) + a2

      hyp1 = sqrt((w1/2)^2 + (h1/2)^2)
      hyp2 = sqrt((w2/2)^2 + (h2/2)^2)

      V2D (x11, y11) = corner_UL1
      V2D (x12, y12) = corner_UR1
      V2D (x13, y13) = corner_BL1
      V2D (x14, y14) = corner_BR1

      V2D (x21, y21) = corner_UL2
      V2D (x22, y22) = corner_UR2
      V2D (x23, y23) = corner_BL2
      V2D (x24, y24) = corner_BR2


      -- Find normals
      -- normal to top/bottom edge of rectangle 1
      normal_tb1 = V2D (y11 - y12, x12 - x11)
      normal_rl1 = V2D (y11 - y13, x13 - x11)  -- right/left edge
      normal_tb2 = V2D (y21 - y22, x22 - x21)
      normal_rl2 = V2D (y21 - y23, x23 - x21)


      -- "Project" corners onto normals
      -- "Project" because don't need to divide by magnitude of the vector because when comparing, are comparing vectors projected onto the same vector so we don't need to divide by the mag of that vec


      r1_corners = [corner_UL1, corner_UR1, corner_BL1, corner_BR1]
      r2_corners = [corner_UL2, corner_UR2, corner_BL2, corner_BR2]
      normals = [normal_tb1, normal_rl1, normal_tb2, normal_rl2]

      -- f1 = map scal_proj r1_corners
      f1 = map dot_prod r1_corners
      projList1 = map (\normal -> map ($ normal) f1) normals

      -- f2 = map scal_proj r2_corners
      f2 = map dot_prod r2_corners
      projList2 = map (\normal -> map ($ normal) f2) normals


      -- Check if "projections" overlap
      collidez = and $ zipWith overlap projList1 projList2 


-- Scalar projection of a onto b

dot_prod :: V2D -> V2D -> Float
dot_prod (V2D (x1,y1)) (V2D (x2,y2)) = x1*x2 + y1*y2


magnitude :: V2D -> Float
magnitude (V2D (q, r)) = sqrt(q^2 + r^2)


overlap :: [Float] -> [Float] -> Bool
overlap r1_scal_vecs r2_scal_vecs = overlap'
    where
        minr1 = minimum r1_scal_vecs
        maxr1 = maximum r1_scal_vecs

        minr2 = minimum r2_scal_vecs
        maxr2 = maximum r2_scal_vecs

        overlap' = (minr1 <= minr2 && maxr1 >= minr2) || (minr2 <= minr1 && maxr2 >= minr1)

        -- or $ [and [minr1 <= minr2, maxr1 >= minr2], and [minr2 <= minr1, maxr2 >= minr1]]

rtf = realToFrac

