### Exercise: Show that whatever the test <TEST> the program below terminates. Are there any assumptions you need do make the argument work?

```
while ub > lb + 1 do
begin r : = (ub + lb) div 2;
if <TEST> then ub := r else lb := r
end 
```

cases:

(ub + lb)/2 - lb < ub - lb

(ub + lb)/2 < ub



|---|---|

lb       lb       ub

|---|---|

lb       ub       ub


ub - lb

