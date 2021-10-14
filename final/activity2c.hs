
reflectionAngle :: Angle -> Line -> Angle
reflectionAngle angle ((x_1, y_1), (x_2, y_2)) 
        | x_2 == x_1 = if x_2 < 0 then pi + angle else pi - angle
        | alpha == 0 = (-1)* angle
        | alpha > 0 && alpha < pi/2 = alpha + pi/2 - angle
        | otherwise =  (-1) * (alpha + pi/2 - angle)
        where 
          alpha = ( y_2 - y_1 )/( x_2 - x_1 )
          


velocityRot = 1.5 * pi

accelerationShip = 0.5

ship = [(0, 0), (1, 2), (2, 0)]

velocityLimitY = 0.15

velocityLimitX = 0.15

screenLimitY = 10

screenLimitX = 10

main = activityOf initial update view

initial = (centroid ship, (1, 1), pi / 2, 's', False)



update :: Event -> World -> World
update (KeyPress "Left") ((x, y), (v_x, v_y), angle, rot, accelerating) = ((x, y), (v_x, v_y), angle, 'a', accelerating)
update (KeyPress "Right") ((x, y), (v_x, v_y), angle, rot, accelerating) = ((x, y), (v_x, v_y), angle, 'h', accelerating)
update (KeyPress "Up") ((x, y), (v_x, v_y), angle, rot, accelerating) = ((x, y), (v_x, v_y), angle, rot, True)
update (KeyRelease "Up") ((x, y), (v_x, v_y), angle, rot, accelerating) = ((x, y), (v_x, v_y), angle, rot, False)
update (KeyRelease "Left") ((x, y), (v_x, v_y), angle, rot, accelerating) = ((x, y), (v_x, v_y), angle, 's', accelerating)
update (KeyRelease "Right") ((x, y), (v_x, v_y), angle, rot, accelerating) = ((x, y), (v_x, v_y), angle, 's', accelerating)
update (TimePassing t) ((x, y), (v_x, v_y), angle, rot, accelerating)
  | rot == 'a' = ((x, y), (v_x, v_y), angle + t * velocityRot, rot, accelerating)
  | rot == 'h' = ((x, y), (v_x, v_y), angle - t * velocityRot, rot, accelerating)
  | x >= screenLimitX = ((x + v_x * cos angle - 0.5, y + v_y * sin angle), (v_x + t * accelerationShip/2 - 1, v_y + t * accelerationShip/2 - 1), reflectionAngle angle ((10, -10), (10, 10)) , rot, accelerating)
  | y >= screenLimitY = ((x + v_x * cos angle, y + v_y * sin angle - 0.5), (v_x + t * accelerationShip/2, v_y + t * accelerationShip/2), reflectionAngle angle ((10, 10), (-10, 10)), rot, accelerating)
  | x <= (-1) * screenLimitX = ((x + v_x * cos angle + 0.5, y + v_y * sin angle), (v_x + t * accelerationShip/2, v_y + t * accelerationShip/2), (-1)*reflectionAngle angle ((-10, 10), (-10, -10)), rot, accelerating)
  | y <= (-1) * screenLimitY = ((x + v_x * cos angle, y + v_y * sin angle + 0.5), (v_x + t * accelerationShip/2, v_y + t * accelerationShip/2), reflectionAngle angle ((-9.9, -10), (10, -10)), rot, accelerating)
  | accelerating =
    if v_x <= velocityLimitX && v_y <= velocityLimitY
      then ((x + v_x * cos angle, y + v_y * sin angle), (v_x + t * accelerationShip/2, v_y + t * accelerationShip/2), angle, rot, accelerating)
      else ((x + v_x * cos angle, y + v_y * sin angle), (v_x, v_y), angle, rot, accelerating)
  | not accelerating && v_x >= 0 && v_y >= 0 = ((x + v_x * cos angle, y + v_y * sin angle), (v_x - t * accelerationShip/12, v_y - t * accelerationShip/12), angle, rot, accelerating)
  | otherwise = ((x, y), (v_x, v_y), angle, rot, accelerating)
update _ w = w

view :: World -> Picture
view ((x, y), (v_x, v_y), angle, rot, accelerating) =
  translated x y (colored red (rotated (angle - pi / 2) (uncurry translated (centroid ship) (solidPolygon ship))))

  
  
  
