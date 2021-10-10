module Geometric.Reflection where
import Geometric.Intersect

type Vector = (Point, Point)

type Angle = Float

type DotProduct = Float

type Norm  = Float

-- |The "reflection" function returns the reflection vector if the vector intersects the line

reflection :: Vector -> Line -> Vector
reflection ((x_1,y_1),(x_2,y_2)) ((x_3,y_3),(x_4,y_4)) 
            | (intersectsTwoLines ((x_1,y_1),(x_2,y_2)) ((x_3,y_3),(x_4,y_4)) ) = ((x_2, y_2), headOfVector)
            | otherwise = error "do not intersect"
                        where 
                                headOfVector = reflexVectorPoint ((x_1,y_1), (x_2,y_2)) resulVec
                                resulVec = resultantVector resulNor unitaryLine
                                resulNor = resultantVectorNorm nor angle 
                                nor = norm ((x_1,y_1),(x_2,y_2))
                                angle = theta ((x_1,y_1),(x_2,y_2)) ((x_3,y_3),(x_4,y_4))
                                unitaryLine = unitary ((x_3,y_3),(x_4,y_4)) lineNor
                                lineNor = norm ((x_3,y_3),(x_4,y_4))
                                

-- |The "reflexVectorPoint" function returns the "head" point of the reflex vector

reflexVectorPoint :: Vector -> Vector -> Point
reflexVectorPoint ((x_1,y_1),(x_2,y_2)) ((x_3,y_3),(x_4,y_4)) = ((x_4-x_3)-(x_2-x_1),(y_4-y_3)-(y_2-y_1))

-- |The "resultantVector" function returns the vector resulting from the sum of the vector with its reflection vector

resultantVector :: Norm -> Vector -> Vector
resultantVector n ((x_1,y_1),(x_2,y_2)) = 
    ((x_1*n,y_1*n),(x_2*n,y_2*n))

-- |The "unitary" function returns the unit vector of a vector

unitary :: Line -> Norm -> Vector
unitary ((x_1,y_1),(x_2,y_2)) n = ((x_1/n,y_1/n),(x_2/n,y_2/n))

-- |The "resultantVectorNorm" function calculates the norm of the resultant vector

resultantVectorNorm :: Norm -> Angle -> Norm
resultantVectorNorm vNorm theta =
    (2)*(vNorm)*(sin alpha)
    where alpha = 90 - theta

-- |The "angle" function returns an angle formed by a vector in contact with a line

theta :: Vector ->  Line -> Angle
theta v w = acos calculation
    where calculation = (dotProduct v w)/(norm v)*(norm w)

-- |The "norm" function calculates the norm of a vector

norm :: Vector -> Norm
norm ((x_1,y_1),(x_2,y_2)) = sqrt(((x_2-x_1)^2)+((y_2-y_1)^2))

-- |The "dotProduct" function calculates the dot product of two vectors

dotProduct :: Vector -> Line ->  DotProduct
dotProduct ((x_1, y_1),(x_2, y_2)) ((x_3, y_3),(x_4, y_4))  =
    ((x_2-x_1)*(x_4-x_3)) + ((y_2-y_1)*(y_4-y_3))
