-- 2.a
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
  | accelerating =
    ((x + v_x * cos angle, y + v_y * sin angle), (1, 1), angle, rot, accelerating)
    | otherwise = ((x, y), (0, 0), angle, rot, accelerating)
update _ w = w
view :: World -> Picture
view ((x, y), (v_x, v_y), angle, rot, accelerating) =
  translated x y (colored red (rotated (angle - pi / 2) (translated (fst (centroid ship)) (snd (centroid ship)) (solidPolygon ship))))

