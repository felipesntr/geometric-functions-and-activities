module Geometric.Reflection where
import Geometric.Intersect

type Vector = (Point, Point)

type Angle = Double

type DotProduct = Double

type Norm  = Double

-- |The "reflection" function returns the reflection vector if the vector intersects the line

reflection :: Vector -> Line -> Vector
reflection ((x_1,y_1),(x_2,y_2)) ((x_3,y_3),(x_4,y_4))
            | intersectsTwoLines ((x_1,y_1),(x_2,y_2)) ((x_3,y_3),(x_4,y_4)) = ((x_2,y_2),(b1, b2))
            | otherwise = error "do not intersect"
                where 
                    b1 = ((x_1-x_2)*cos alpha) + ((y_1-y_2)*((-1)*(sin alpha)))
                    b2 = ((x_1-x_2)*sin alpha) + ((y_1-y_2)*(cos alpha))
                    alpha = ((theta ((x_1,y_1),(x_2,y_2)) ((x_3,y_3),(x_4,y_4)))*2) + 180 

-- |The "theta" function returns an angle formed by a vector in contact with a line

theta :: Vector ->  Line -> Angle
theta v w = cos calculation
    where calculation = (dotProduct v w)/(norm v)*(norm w)

-- |The "norm" function calculates the norm of a vector

norm :: Vector -> Norm
norm ((x_1,y_1),(x_2,y_2)) = sqrt(((x_2-x_1)^2)+((y_2-y_1)^2))

-- |The "dotProduct" function calculates the dot product of two vectors

dotProduct :: Vector -> Line ->  DotProduct
dotProduct ((x_1, y_1),(x_2, y_2)) ((x_3, y_3),(x_4, y_4))  =
    ((x_2-x_1)*(x_4-x_3)) + ((y_2-y_1)*(y_4-y_3))