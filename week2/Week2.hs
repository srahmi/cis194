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

data Person = Person String Int Thing
  deriving Show

brent :: Person
brent = Person "Brent" 31 SealingWax

stan :: Person
stan = Person "Stan" 94 Cabbage

said :: Person
said = Person "Said" 31 SealingWax

getAge :: Person -> Int
getAge (Person _ a _) = a

baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++") is " ++ n

checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ ", you're my kind of preson!"
checkFav (Person n _ _)          = n ++ ", your favourit thing is lame."

printAge :: Person -> String
printAge (Person _ a@age _) = "Your age is " ++ show a

caseFunction :: String -> Int
caseFunction input = case input of
                        []      -> 3
                        ('H':s) -> length s
                        _       -> 7

failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
                     Failure -> 0
                     OK d    -> d
