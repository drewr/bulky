# bulky

ES bulk stream editor that doesn't edit (yet!)

## Installation

```
% cabal build
```

## Usage

```
% gzip -cd foo.bulk.gz | dist/build/bulky/bulky >foo2.bulk.gz
```

## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```
