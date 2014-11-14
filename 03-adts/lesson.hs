-- enumeration types

data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show

shoe :: Thing
shoe = Shoe

listO'Things :: [Thing]
listO'Things = [Shoe, SealingWax, Cabbage, King, Ship]

-- write functions on Things by pattern matching

isSmall :: Thing -> Bool
isSmall Shoe = True
isSmall Ship = False

-- Thing is an enumeration type, but enumerations are a special case
-- of Haskell's more general _algebraic data types_

data FailableDouble = Failure
                    | OK Double
  deriving Show

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d) = d

-- data constructors can have more than one argument

data Person = Person String Int Thing
  deriving Show

brent :: Person
brent = Person "Brent" 31 SealingWax

stan :: Person
stan = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a

-- A pattern of the form x@pat can be used to match a value against the pattern
-- pat, but also give the name x to the entire value being matched. For example:

baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

-- patterns can be nested

checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ " likes sealing wax"
checkFav (Person n _ _) = n ++ " likes something else"

-- fundamental construct for doing pattern matching is case expression

ex03 = case "Hello" of
        []      -> 3
        ('H':s) -> length s
        _       -> 7

-- recursive data types

data IntList = Empty | Cons Int IntList

intListProd :: IntList -> Int
intListProd Empty = 1
intListProd (Cons x l) = x * intListProd l
