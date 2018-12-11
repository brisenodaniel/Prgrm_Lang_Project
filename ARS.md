### Consider the schemas of rules

  ab -> ba
  
  ba -> ab
  
  aa ->
  
  b ->
  
### Reduce some example strings such as abba and bababa.
                   
abba: ```abba ->* aa -> Empty```

bababa: ```bababa ->* aaa -> a```

### Why is the ARS not terminating?


### How many equivalence classes does ⟷∗ have? Can you describe them in a nice way? What are the normal forms?

This has two equivalence classes. 

### Can you change the rules so that the ARS becomes terminating without changing its equivalence classes? Which measure function proves termination of your modified system?

We would just need to remove the first two rules.

### Write down a question or two about strings that can be answered using the ARS. Think about whether this amounts to giving a semantics to the ARS.

Reduce the string ```abbaabbaabba```

Reduce the string ```aaaaaaaaabba```
