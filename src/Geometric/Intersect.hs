module Geometric.Intersect where

type Point = (Double, Double)

type Polygon = [Point]

type Line = (Point, Point)


-- 1
{-
    m > 0, orientation is counterclockwise
    m < 0, orientation is clockwise
    m = 0, is collinear
-}

-- | The 'orientation' function checks the orientation of three points.
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

-- | The 'intersectsTwoLines' function checks whether the two lines intersect.
intersectsTwoLines :: Line -> Line -> Bool
intersectsTwoLines
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
    -- the intersection conditions of the projections are still missing..
    | otherwise = False

-- 2

-- | The 'intersectsLinePolygons' checks whether a line intersects a polygon.
intersectsLinePolygons :: Line -> Polygon -> Bool
intersectsLinePolygons line polygon =
  or [line `intersectsTwoLines` side | side <- zip polygon (tail polygon)]


-- 3

-- | The 'intersectsTwoPolygons' checks whether a polygon intersects another polygon.
intersectsTwoPolygons :: Polygon -> Polygon -> Bool
intersectsTwoPolygons polygon_1 polygon_2 =
  or [ intersectsLinePolygons sideOfPolygon1 polygon_2 | sideOfPolygon1 <- zip polygon_1 (tail polygon_1) ]


-- 7

-- | The 'pointOfIntersectionTwoLines' returns the intersection point of two lines, if it exists.
pointOfIntersectionTwoLines :: Line -> Line -> Point
pointOfIntersectionTwoLines ((x_1, y_1), (x_2, y_2)) ((x_3, y_3), (x_4, y_4))
  | intersectsTwoLines ((x_1, y_1), (x_2, y_2)) ((x_3, y_3), (x_4, y_4)) = (x, y)
  | otherwise = error "Error: there is no intersection between the segments."
  where 
        -- coefficients
        a_1 = (y_2 - y_1)/(x_2 - x_1)
        c_1 = (-a_1) * x_1 + y_1
        a_2 = (y_4-y_3)/(x_4-x_3)
        c_2 = a_2 * x_3 + y_3
        -- coordenates 
        x = (c_2-c_1)/(a_1-a_2)
        y = a_1 * x + c_1

