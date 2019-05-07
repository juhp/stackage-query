[![Build Status](https://travis-ci.org/juhp/stackage-query.png)](https://travis-ci.org/juhp/stackage-query)
[![Hackage](http://img.shields.io/hackage/v/stackage-query.png)](http://hackage.haskell.org/package/stackage-query)
[![Stackage LTS](http://stackage.org/package/stackage-query/badge/lts)](http://stackage.org/lts/package/stackage-query)
[![Stackage Nightly](http://stackage.org/package/stackage-query/badge/nightly)](http://stackage.org/nightly/package/stackage-query)

# Stackage-Query

Small tool that queries package metadata from Stackage snapshots
using the yaml files from stackage-nightly and lts-haskell.

Example usage:

```
$ stackage package lts pandoc
2.5
$ stackage core lts-12
<list of ghc library versions>
$ stackage packages lts-13.20 | wc -l
2322
$ stackage consumers lts-12 --minimum 500
867 text
$ stackage users nightly pandoc
pandoc pandoc-citeproc pandoc-csv2table pandoc-pyplot
$ stackage latest nightly
nightly-2019-05-07
```
