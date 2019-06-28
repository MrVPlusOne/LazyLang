# Lazy-lang

A Haskell-style lazy language in 500 lines of Scala code

**TDDO: Write a blog about Lazy-lang**

### A quick look
Use `sbt repl` to start Lazy-lang's REPL (Read–eval–print loop).

Evaluate an expression:
```$xslt
λ> plus 1 2
(parsed) Right(((plus 1) 2))
(evaluated in 8 steps)
res0 = 3
```

Introduce a definition to the environment:
```
λ> fact x = if greater x 0 then times x (fact (plus x -1)) else 1
(parsed) Left((fact,(λ x. (((choose ((greater x) 0)) ((times x) (fact ((plus x) -1)))) 1))))
fact defined.

λ> fact 3
(parsed) Right((fact 3))
(evaluated in 129 steps)
res1 = 6
```

Lists are just nested pairs (2-tuples):
```$xslt
λ> showList [1,2,3,unit]
(parsed) Right((showList ((Pair 1) ((Pair 2) ((Pair 3) unit)))))
(evaluated in 292 steps)
res2 = '[1, [2, [3, unit]]]'
```

Get the 3rd element of a list:
```$xslt
λ> get 2 [1,2,3,4,unit]
(parsed) Right(((get 2) ((Pair 1) ((Pair 2) ((Pair 3) ((Pair 4) unit))))))
(evaluated in 134 steps)
res3 = 3
```

Define an infinite list of fibonacci numbers:
```$xslt
λ> showList (take 10 fibs) where fibs = [1, 1, zipWith plus fibs (snd fibs)]
(parsed) Right(((showList ((take 10) fibs)) where fibs = ((Pair 1) ((Pair 1) (((zipWith plus) fibs) (snd fibs))))))
(evaluated in 2564 steps)
res4 = '[1, [1, [2, [3, [5, [8, [13, [21, [34, [55, unit]]]]]]]]]]'
```

