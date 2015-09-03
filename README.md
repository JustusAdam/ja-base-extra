# ja-base-extra [![Hackage Status](https://img.shields.io/hackage/v/ja-base-extra.svg)][hackage]

Or **j**ustus-**a**dam-base-extra, which is the expanded form.

This is a library of small helper functions that I use and that depend only on the `base` library. But of which I am certain they would never be added to the actual `base` library.

Most of these functions are simple to implement and do not warrant including a library, however I like to do so regardless, because it thins out the usual `MyProject.Util` module.

## Usage

Just add as dependency and `import Original.Module.JAExtra`. Currently available on [hackage][].

[hackage]: https://hackage.haskell.org/package/ja-base-extra

## Naming

This library is named after me for two reasons:

1. because it is an opinionated library of things that I thought were necessary and
2. because naming it and the included modules something generic is not cool, if it is not generic by design (like the `extra` library)

## Documentation

Haddock documentation is provided. If you are unhappy with the documentation [let me know](mailto:dev@justus.science) or [submit a pull request][pr].

[pr]: https://github.com/JustusAdam/ja-base-extra/compare

## Contributing

I am happy about any contribution. Just [submit a pull request][pr].

Be aware though that I might reject proposals because I judge them unfitting, it is after all an opinionated library.
