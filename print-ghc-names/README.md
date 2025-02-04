This is a plugin that collects all the names used by a module that
come from the `ghc` unit or from `ghc-lib-parser`.

Build with

```
cabal build print-ghc-names
```

Run with

```
ghc -fplugin=PrintGHCNames SOURCES
```

The output of the compiler is augmented with several lines of the form

```
print-ghc-names:unit-where-name-is-used:Fully.Qualified.Name
```

One such line is printed for each found name. e.g.

```
print-ghc-names:hie-compat-0.3.1.2-inplace:GHC.Utils.Outputable.Outputable
print-ghc-names:hie-compat-0.3.1.2-inplace:GHC.Utils.Outputable.SDoc
print-ghc-names:hie-compat-0.3.1.2-inplace:GHC.Utils.Outputable.ppr
print-ghc-names:hls-plugin-api-2.9.0.1-inplace:GHC.Driver.DynFlags.DynFlags
print-ghc-names:hls-plugin-api-2.9.0.1-inplace:GHC.Driver.DynFlags.DynFlags
print-ghc-names:hls-plugin-api-2.9.0.1-inplace:GHC.Driver.DynFlags.DynFlags
print-ghc-names:ghcide-2.9.0.1-inplace:GHC.Core.CoreProgram
print-ghc-names:ghcide-2.9.0.1-inplace:GHC.Core.TyCo.Rep.FunTy
print-ghc-names:ghcide-2.9.0.1-inplace:GHC.Core.TyCo.Rep.Type
```
