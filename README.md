# Metagen

Planning to provide type-safe and expressive eDSLs that generating code for Python and other programming languages in lack of static safety.

We're capable of doing this, because
1. natural numbers and heterogeneous lists that are inductive
2. concise notations for type level stuffs
3. Haskell's type system enables encoding variadic arguments, OOP classes and other necessary language constructs in the host language. The code typechecks.

```haskell
main = do
  let
      hlist = [hl|1, 2, 3, 5|]
      index = [nat|3]

      hlist' :: [hl|Int, Int, Int, Int|]
      hlist' = hlist

  print $ hlist' ! index
  putStrLn "FLam"
```

In the this package, a Y*-style transformer for extensible Quotation rewritting is provided, which considerably simplifies the use of Template Haskell. See `Metagen.TH` for implementation, and `Metagen.Templates` for the usage.
