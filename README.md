# Purescript DataCite

[![License: MPL 2.0](https://img.shields.io/badge/License-MPL%202.0-brightgreen.svg)](https://opensource.org/licenses/MPL-2.0)

PureScript types for representing and reading DatCite (currently JSON only)
records.

## Usage


## Build code

> npm install

> npm run build

## Run Tests

Tests use static copies of data obtained from DataCite, so they
should periodically be checked against DataCite in case of a
Schema update.

## Publish

> TODO

## Generate Draft Haskell datatypes

As I'm unaware of any utility to generate PureScript types
from XML schemas, we'll use a relatively similar language output
(Haskell). The Haskell type can then be manually converted to PureScript.
We have committed the XML schema and generated Haskell code so that
diffs of future schema versions may be readily obtained when
updating the PureScript datatypes. Also of note, it is important to
be aware of [some differences](https://blog.datacite.org/introducing-datacite-json/)
between DataCite JSON and XML.

All of the following commands should be run from the `schemas` subdirectory.

Using e.g. `nix`, bring HaXml into the environment:

```
nix-shell -p haskellPackages.HaXml
```

Create a copy of the schema that you will modify so that it will work with
HaXml (`XsdToHaskell`):

> cp metadata_4.3.xsd metadata_4.3_mod.xsd


### Manual Schema modifications 

The following changes need to be made to work with `XsdToHaskell`:

- `XsdToHaskell` doesn't support `xs:all`, and anyway, the Haskell representation would
be somewhat different (and not as good) as in PureScript, as in PureScript the
natural choice seems to be record. Replace all occurrences of `xs:all` with `xs:sequence`.
Currently this includes:
  - The `xs:complexType` children of `xs:element name="resource"`.
  - The `xs:complexType` children of `xs:element name="fundingReference"`.
  - Children of `xs:complexType name="point"`.
  - Children of `xs:complexType name="box"`.


### Generating the output

Generate Haskell output by running the following:

> XsdToHaskell < metadata_4.3_mod.xsd > metadata_4.3.xsd.hs

At this point, it is now a manual process to adapt (any changes to)
the code in `DataCite.Types`, but you can use diffs with the previously
generated `.hs` files (as well as diffs between actual `.xsd` schema files)
as a guide.
