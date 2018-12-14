### Consider the schemas of rules

  ab -> ba
  
  ba -> ab
  
  aa ->
  
  b ->
  
### Reduce some example strings such as abba and bababa.
                   
abba: ```abba ->* aa -> Empty```

bababa: ```bababa ->* aaa -> a```

### Why is the ARS not terminating?

It is not terminating because ```ab -> ba -> ab -> ba```
The existence of a measure function is impossible with these two rules.

### How many equivalence classes does ⟷∗ have? Can you describe them in a nice way? What are the normal forms?

This has two equivalence classes. Given an odd amount of a's, the normal form would reduce to a single 'a' character. Given an even amount, the string would reduce to an empty string. The number of b's does not matter, since the rule always reduces b's to an empty word, thus making the normal form dependent on the numbers of a's in the string.

### Can you change the rules so that the ARS becomes terminating without changing its equivalence classes? Which measure function proves termination of your modified system?

We would just need to remove the first two rules. The function A(L) -> N proves this.

### Write down a question or two about strings that can be answered using the ARS. Think about whether this amounts to giving a semantics to the ARS.

Reduce the string ```abbaabbaabba```

Reduce the string ```aaaaaaaaabba```