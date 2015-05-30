-- Algebraic data types
-- http://www.seas.upenn.edu/~cis194/spring13/lectures/02-ADTs.html

data Thing = Shoe
            | Ship
            | SealingWax
            | Cabbage
            | King
  deriving Show

isSmall :: Thing -> Bool
isSmall Ship = False
isSmall King = False
isSmall _ = True

-- enumeration is just a special case of haskell's algebraic data types.
-- here is a data type that is more than just enumeration

-- an instance of this data type can be one of many different things, which it
-- seems we specify in the ADT declaration.
-- From wiki: "Each “type of thing” is associated with an identifier called a
-- constructor, which can be thought of as a kind of tag for that kind of data"
data FailableDouble = Failure
                    | OK Double
                    deriving Show

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d) = d

-- composes to: failureToZero (safeDiv x y)

-- Data constructors can have more than one argument
-- Store a person's name, age, favorite Thing
data Person = Person String Int Thing
  deriving Show

ludwig :: Person
ludwig = Person "Amy" 22 Cabbage

hannah :: Person
hannah = Person "Hannah" 29 Shoe

getAge :: Person -> Int
getAge (Person _ a _) = a

{-
An algebraic data type has one or more data constructors, and each
data constructor can have zero or more arguments.

data AlgDataType = Constr1 Type11 Type12
                 | Constr2 Type21
                 | Constr3 Type31 Type32 Type33
                 | Constr4

This specifies that a value of type AlgDataType can be constructed in one of
four ways: using Constr1, Constr2, Constr3, or Constr4.
-}

-- Recursive data types
-- Lists are recursive. A list is either empty or is an element followed by the
-- remaining list.

data IntList = Empty | Cons Int IntList


