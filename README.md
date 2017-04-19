# tinyparsec
A very basic implementation of a parser combinator library in purescript.

The implementation is based on this paper - [Monadic Parsing in Haskell](http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf)

### How to test it out

Have implemented it with purescript 0.11.2.

Assuming you have purescript compiler (psc), pulp and bower already installed - 

```
bower install
pulp psci
```

In the purescript repl - 

```
> import TinyParsec
> import JsonParser
> parse jsonValParser "{\"name\": \"John\", \"age\": 36}"
> import SimpleArithParser
> parse expr "23+43*3-4"
```

