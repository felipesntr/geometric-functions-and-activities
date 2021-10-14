

{-
    Dupla:
        - Felipe Santos Rocha
            - Matricula : 202100104271

            
        - Ytallo 
-}


type Point = (Double, Double)

type Polygon = [Point]

type Line = (Point, Point)


-- 1
{-
    m > 0, orientation is counterclockwise
    m < 0, orientation is clockwise
    m = 0, is collinear
-}

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

intersectsLinePolygons :: Line -> Polygon -> Bool
intersectsLinePolygons line polygon =
  or [line `intersectsTwoLines` side | side <- zip polygon (tail polygon)]


-- 3

intersectsTwoPolygons :: Polygon -> Polygon -> Bool
intersectsTwoPolygons polygon_1 polygon_2 =
  or [ intersectsLinePolygons sideOfPolygon1 polygon_2 | sideOfPolygon1 <- zip polygon_1 (tail polygon_1) ]

-- 4

areaPolygon :: Polygon -> Double
areaPolygon polygon =
    abs (0.5 * sum [
        (x_1*y_2) - (x_2*y_1) | ((x_1, y_1), (x_2, y_2)) <- pairList])
    where
        polygon' = polygon ++ take 1 polygon
        pairList = zip polygon' (tail polygon')

-- 5

centroid :: Polygon -> Point
centroid polygon = (c_x, c_y)
    where
        c_x =  sum [  (x_1 + x_2) * (x_1*y_2 - x_2* y_1) | ((x_1, y_1), (x_2, y_2)) <- pairList ]
                / (6 * areaPolygon polygon)
        c_y =  sum [  (y_1 + y_2) * (x_1*y_2 - x_2*y_1) | ((x_1, y_1), (x_2, y_2)) <- pairList ]
                / (6 * areaPolygon polygon)
        polygon' = polygon ++ take 1 polygon
        pairList = zip polygon' (tail polygon')

-- 6 

{- COLAR A 6 AQUIII -}

-- 7

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

