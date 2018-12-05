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


# Haskell is strongly typed
Haskell is unique in its typing compared to other strongly typed languages, as types are automatically inferred. 
You should document the types you're going to be using, but you don't have to declare the types. In a sense, 
Haskell is similar to Python where the

This means that you very rarely have to declare the types of your functions, except as a means of code documentation. 

Haskell will look at how you use the variables and figure out from there what type the variable should be. Some would say Haskell 
is superior to languages like python in a sense, because even though they both assume the type, Haskell will catch errors 
at compile time instead of run time.



