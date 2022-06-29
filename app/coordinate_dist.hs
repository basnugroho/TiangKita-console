x1 = -7.327634106959024
y1 = 112.7785288916455
x2 = -7.327893510270373
y2 = 112.77854716094016

coordist :: (Double, Double) -> (Double, Double) -> Double
coordist (x1 , y1) (x2 , y2) = sqrt (x'*x' + y'*y')
    where
      x' = x1 - x2
      y' = y1 - y2