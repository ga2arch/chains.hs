hs-chains
=========

Don't break the chain made in haskell 

# Installation 

cabal install --only-dependencies


# Run 

cabal run (add|rm|done|show)

add  <name>
rm   <name>
done <name>
show 

# Example

cabal run add Study

```
Study
1
---
```

cabal run done Study

```
Study
1
---
 V
 ```

