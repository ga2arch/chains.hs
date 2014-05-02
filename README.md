chains.hs
=========

Don't break the chain made in haskell 

# Installation 

cabal install --only-dependencies


# Run 

## Cli

cabal run (add | rm | done | show)

```
add  <name>
rm   <name>
done <name>
show 
```

## Web

cabal run web

# Example - Cli 

cabal run add Study

```
Study - 0 
---------
```

cabal run done Study

```
Study - 1
---------
V
```
cabal run done Study

```
Study - 2
---------
V V
```
