# Scala Exercises realised by myself

This is a space to collect suggestions, doubts and/or problems that I'm facing when doing Scala Exercises

## Cats

- What are the most common Functors? List, Option and Future?
- Why `Foldable[List].foldK(List(None, Option("two"), Option("three")))` is `Some("two")`
  while `Foldable[List].foldK(List(List(1, 2), List(3, 4, 5)))` is `List(1,2,3,4,5)` 
  
