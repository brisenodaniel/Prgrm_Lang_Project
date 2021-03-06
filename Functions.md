Here is a simple haskell function:

```haskell
doubleMe :: Int -> Int --function declaration
doubleMe x = x + x  --function definition
```

Haskell allows for conditional statements and other controls within function declarations:
```haskell
doubleSmallNums x = if x < 100 x * 2 else x --conditional in function declaration
```


The following code uses pattern matching. Patterns are checked from top to bottom, and it is almost like an "if else" condition. 
The code is much more clean that that, however, because of the syntax.
```haskell
factorial :: Int -> Int 
factorial 0 = 1
factorial x = x * factorial(x-1)
```


In the print statement, there is a lamdba function, which is an anonymous function. The '/' marks the beginning of the lamdba
function, and the "2" is the argument used. This is useful when we only need to use this function once, because it is very 
easy and quick to create:
```haskell
main = do
  putStrLn "The predecessor of 2 is: "
  print(/(x -> x-1) 2) --lambda function
 ```

Guards are very similar to "if else" statements. They are the "|" symbol. They, like pattern matching, make the code easier
to read and write. Every "if else then" statement can be replaced by a guard, and are generally preferred. The "where" clause 
is useful for scoping bindings over several guarded equations. It is especially efficient when the variables in the conditions
have long definitions, such as the "bmiTell" function further below.
```haskell
progressReport :: Double -> String
progressReport grade
  | grade <= belowAverage = "You are currently failing this course" 
  | grade <= average = "You are doing poorly in this course"
  | grade <= good = "You are around doing an average job in this course"
  | grade <= Excellent = "You are doing well in this course"
  | otherwise = "You are exceeding expectations in this course"
where Excellent = 90.0 --where clause
      good = 80.0
      average = 70.0
      belowAverage = 60.0

bmiTell :: Float -> Float -> String
bmiTell weight height
    | weight / height ^ 2 <= 18.5 = "You're underweight"
    | weight / height ^ 2 <= 25.0 = "You're at a normal body mass"
    | weight / height ^ 2 <= 30.0 = "You are overweight"
    | otherwise = "You are very overweight"

bmiTell :: Float -> Float -> String
bmiTell weight height
    | bmi <= 18.5 = "You are underweight"
    | bmi <= 25.0 = "You are at a normal body mass"
    | bmi <= 30.0 = "You are overweight"
    | otherwise   = "You are very overweight"
    where bmi = weight / height ^ 2


```
  
  You can use the where clause to pattern matching. You can also use the where clause to define functions. 'Where' bindings are syntactic constructs, 'let' bindings are expressions
  
The "where" clause, however, is not the end-all and be-all of declaration. 
```haskell
f :: State s a
f = State $ \x -> y
   where y = ... x ...
```
The function above won't work because x is not in scope, and where is referring to the pattern matching of f=. 
When "let" is used, we don't have a problem.
```haskell
f :: s -> (a,s)
f x =
   let y = ... x ...
   in  y
```


Source: http://learnyouahaskell.com/syntax-in-functions
