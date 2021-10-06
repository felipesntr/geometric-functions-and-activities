type Point = (Float, Float)

type Poly = [Point]

type Line = [Point]

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

-- (a, b, c) = a * x + b * y + c = 0

-- -- | The 'intersect' function
-- intersect :: Line -> Line -> Bool
-- intersect
--   [p_1, q_1]
--   [p_2, q_2]
--     | orientation p_1 q_1 p_2 /= orientation p_1 q_1 q_2
--         && orientation p_2 q_2 p_1 /= orientation p_2 q_2 q_1 =
--       True