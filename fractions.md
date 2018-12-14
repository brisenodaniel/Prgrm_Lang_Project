
Fractions are sets of number pairs
1/3 = {(1,3),(2,6),(3,9)...}
2/6 = {(1,3),(2,6),(3,9)...}


   def
1/3 = [(1,3)]
Confluence - order in which you cancel doesn't matter.

a/b = a'/b'
a * b' = b * a'

(a,b) = (a',b') if (a,b) and (a',b') have the same normal forms
(n*a,n*b) = (a,b) = n * a * b = n * b * a

      confluence
          .
        */ \*
        *\ /*

  Order of cancellation doesn't matter
        (2*3*5, 2*3*7)
          /        \
     (3*5, 3*7)  (2*5,2*7)
          \        /
            (5,7)

If you have confluence and termination, then you have unique normal forms.
