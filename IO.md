

To start, a simple hello world:

```haskell
main = PutStrLn "Hello world"

main = do
  putStrLn "What is your dad's name?"
  dad <- getLine
  putStrLn "What is your mom's name?"
  mom <- getLine
  putStrLn "Your parents names are: " ++ mom ++ " and " ++ dad
```


The following code reads a string in and reverses it
```haskell
main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseW line
            main

reverseW :: String -> String
reverseW = unwords . map reverse . words
```
"words" works similar to split in other languages, as it makes a list of words.
Then mapping reverse reverses each item in the list, and finally "unwords" puts
the list into a single string. So in summary, the function takes a string as
input, and a reversed string is outputted using the functions above.
