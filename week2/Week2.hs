data Thing = Shoe
            | Ship
            | SealingWax
            | Cabbage
            | King
   deriving Show

shoe :: Thing
shoe = Shoe

isSmall :: Thing -> Bool
isSmall Ship = False
isSmall King = False
isSmall _    = True

data FailableDouble = Failure
                    | OK Double
  deriving Show

ex01 = Failure
ex02 = OK 3.4

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d
