module Geometric.Area where

type Point = (Float, Float)

type Polygon = [Point]

-- | The 'areaPolygon' function return the area of a polygon.
areaPolygon :: Polygon -> Float
areaPolygon polygon = 
    abs (0.5 * sum [ 
        (x_1*y_2) - (x_2*y_1) | ((x_1, y_1), (x_2, y_2)) <- pairList])
    where
        polygon' = polygon ++ take 1 polygon
        pairList = zip (polygon') (tail polygon')

