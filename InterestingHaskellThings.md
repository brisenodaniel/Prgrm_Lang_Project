# Haskell is Lazy
Values don't have to be computed if they are not going to be used. Infinite lists are a nice implementation of this feature. 

```haskell
ghci> take 10 (cycle [1,0])  
[1,0,1,0,1,0,1,0,1,0]  

ghci> take 20 [10,9..]
[10,9,8,7,6,5,4,3,2,1,0,-1,-2,-3,-4,-5,-6,-7,-8,-9]

ghci> take 50 ['a','b'..]
"abcdefghijklmnopqrstuvwxyz{|}~\DEL\128\129\130\131\132\133\134\135\136\137\138\139\140\141\142\143\144\145\146"

ghci> take 12 (cycle "WOW ")  
"WOW WOW WOW " 
```
Lazy evaluation has many advantages, its main drawback is that memory usage can become unpredictable.


# Haskell is statically typed
Haskell is unique in its typing compared to other statically typed languages, as it has complete type inference. Type inference is when you only have to declare a minimal amount of types by hand. This means that you very rarely declare the types of your functions, except as a means of code documentation. Haskell will look at how you use the variables and figure out from there what type the variable should be. All the types composed together by function application have to match up. If they don't, the program will be rejected by the compiler. 

# Haskell is pure

In Haskell, there are no statements or instructions, only expressions which cannot mutate variables (local or global) nor access state like time or random numbers.


The following function takes an integer and returns an integer. By the type it cannot do any side-effects whatsoever, it cannot mutate any of its arguments.

doubleMe :: Int -> Int
doubleMe x = x +x

The following string concatenation works:
"My name is " ++ "Colton!" 

The following string concatenation is a type error:
"Name: " ++ getLine
Because getLine has type IO String and not String. Name is of type String. You can't mix and match purity with impurity.


