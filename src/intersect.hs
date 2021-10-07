type Point = (Float, Float)

type Polygon = [Point]

type Line = (Point, Point)

{-
    m>0, a orientação é anti-horária
    m<0, a orientação é horária
    m=0, é colinear
-}

-- | The 'orientation' function
orientation :: Point -> Point -> Point -> Int
orientation (x_1, y_1) (x_2, y_2) (x_3, y_3)
  | m > 0 = 1
  | m < 0 = 2
  | otherwise = 0
  where
    m = vetorial ab bc
    ab = (x_2 - x_1, y_2 - y_1)
    bc = (x_3 - x_2, y_3 - y_2)
    vetorial (v_x, v_y) (w_x, w_y) = v_x * w_y - v_y * w_x

-- | The 'intersect' function
intersect :: Line -> Line -> Bool
intersect
  (p_1, q_1)
  (p_2, q_2)
    | orientation p_1 q_1 p_2 /= orientation p_1 q_1 q_2
        && orientation p_2 q_2 p_1 /= orientation p_2 q_2 q_1 =
      True
    | orientation p_1 q_1 p_2 == 0
        && orientation p_1 q_1 q_2 == 0
        && orientation p_2 q_2 p_1 == 0
        && orientation p_2 q_2 q_1 == 0 =
      True
    -- falta as condições de intersecção das projeções...
    | otherwise = False

intersectLinePoly :: Line -> Polygon -> Bool
intersectLinePoly line polygon = or [line `intersect` side | side <- zip polygon (tail polygon)]
