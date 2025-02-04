This program collects a few statistics from the files in the data folder.

It prints information about how often a module from the GHC API is used, and
how often identifiers are used. Hopefully the output is self-describing enough
to interpret it. See the example [output](./output.txt).

Run it with

```
runghc analyze-ghc-names.hs | less
```

The data is collected with `print-ghc-names`. e.g.

```
ghc --make SOURCE_FILES -fplugin=PrintGHCNames | grep -e "^print-ghc-names:" > data/newfile.txt
```
