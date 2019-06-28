# Lazy-lang

A Haskell-style lazy language in 500 lines of Scala code

(with *lambda calculus*, *monad transformers*, and *cats!*)

**TDDO: write a blog about Lazy-lang**

### A quick look
Use `sbt repl` to start Lazy-lang's REPL (Read–eval–print loop).

Evaluate an expression:
```$xslt
λ> plus 1 2
(expr parsed) ((plus 1) 2)
(evaluated in 8 steps)
res0 = 3
```

Recursively define factorial:
```
λ> fact x = if greater x 0 then times x (fact (plus x -1)) else 1
(binding parsed) fact := (λ x. (((choose ((greater x) 0)) ((times x) (fact ((plus x) -1)))) 1))
fact defined.

λ> fact 3
(expr parsed) (fact 3)
(evaluated in 129 steps)
res1 = 6
```

Lists are just nested pairs (2-tuples):
```$xslt
λ> showList [1,2,3,unit]
(expr parsed) (showList ((Pair 1) ((Pair 2) ((Pair 3) unit))))
(evaluated in 292 steps)
res2 = '[1, [2, [3, unit]]]'
```

Define an infinite list of fibonacci numbers (that runs in linear time):
```$xslt
λ> showList (take 10 fibs) where fibs = [1, 1, zipWith plus fibs (snd fibs)]
(expr parsed) ((showList ((take 10) fibs)) where fibs = ((Pair 1) ((Pair 1) (((zipWith plus) fibs) (snd fibs)))))
(evaluated in 2564 steps)
res3 = '[1, [1, [2, [3, [5, [8, [13, [21, [34, [55, unit]]]]]]]]]]'
```

Evaluation trace is printed when encountering run-time errors:
```$xslt
λ> showList [1, 2, plus 1 'a', unit]
(expr parsed) (showList ((Pair 1) ((Pair 2) ((Pair ((plus 1) 'a')) unit))))
(evaluated in 243 steps)
Reduction error: Function undefined on value.
((plus 1) 'a')     | ctx: plus -> <function: plus>
x     | ctx: x -> ((plus 1) 'a')
x     | ctx: x -> x
((f x) y)     | ctx: f -> (λ x. (λ y. x)), x -> ((plus 1) 'a'), y -> unit
(p left)     | ctx: p -> (λ f. ((f x) y)), left -> (λ x. (λ y. x))
(fst x)     | ctx: fst -> (λ p. (p left)), x -> (λ f. ((f x) y))
(showAtom (fst x))     | ctx: showAtom -> <function: showAtom>, fst -> (λ p. (p left)), x -> (λ f. ((f x) y))
head     | ctx: head -> (showAtom (fst x))
(strConcat head)     | ctx: strConcat -> <function: strConcat>, head -> (showAtom (fst x))
((strConcat head) ', ')     | ctx: strConcat -> <function: strConcat>, head -> (showAtom (fst x))
(strConcat ((strConcat head) ', '))     | ctx: strConcat -> <function: strConcat>, head -> (showAtom (fst x))
 ... (30 more hidden) ...
```
