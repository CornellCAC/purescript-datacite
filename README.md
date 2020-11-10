# Purescript DataCite

PureScript types for representing and reading DatCite (currently JSON only)
records.

## Usage


### Build code

> npm install

> npm run build

## Run Tests

Tests use static copies of data obtained from DataCite, so they
should periodically be checked against DataCite in case of a
Schema update.

## Generate Draft Haskell datatypes

As I'm unaware of any utility to generate PureScript types
from XML schemas, we'll use a relatively similar language output
(Haskell). The Haskell type can then be manually converted to PureScript.
We have committed the XML schema and generated Haskell code so that
diffs of future schema versions may be readily obtained when
updating the PureScript datatypes. Also of note, it is important to
be aware of [some differences](https://blog.datacite.org/introducing-datacite-json/)
between DataCite JSON and XML.

### Publish

> TODO
