module Geometric.Centroid where
import Geometric.Area ( Polygon, Point, areaPolygon )

-- | The 'centroid' function return the centroid point of a polygon.
centroid :: Polygon -> Point
centroid polygon = (c_x, c_y)
    where
        c_x =  sum [  (x_1 + x_2) * (x_1*y_2 - x_2* y_1) | ((x_1, y_1), (x_2, y_2)) <- pairList ]
                / (6 * areaPolygon polygon)
        c_y =  sum [  (y_1 + y_2) * (x_1*y_2 - x_2*y_1) | ((x_1, y_1), (x_2, y_2)) <- pairList ]
                / (6 * areaPolygon polygon)
        polygon' = polygon ++ take 1 polygon
        pairList = zip polygon' (tail polygon')


        