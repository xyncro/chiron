# Release Notes

## 6.0.0 - 2015.12.13

Hello to the version 6 release! The big changes in 6 are about simplification in line with the underlying approach to optics taken by the [Aether][aether] library (supported in Chiron). Version 6 also sees the introduction of a dedicated new [Chiron Site][chiron] which will hold an expanding collection of guides, reference, etc.

### Operations

The old distinct sets of functions for lens/prism based operations within a Json expression, with different methods for lenses and prisms (previously called partial lenses), have been unified:

```fsharp
// Old
Json.Lens.get
Json.Lens.set
Json.Lens.map

Json.Prism.get
Json.Prism.set
Json.Prism.map

// (Even) Older
Json.Lens.get
Json.Lens.getPartial
...

// New
Json.Optic.get
Json.Optic.set
Json.Optic.map
```

### Miscellaneous

As well as the changes to optic based usage, there are some excellent fixes and improvements behind the scenes from [Marcus Griep][griep], who has hugely improved the testing, and discovered (and fixed!) some interesting edge case bugs.

[aether]: https://xyncro.tech/aether
[chiron]: https://xyncro.tech/chiron
[griep]: https://github.com/neoeinstein
