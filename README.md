[![Build Status](https://travis-ci.org/juhp/stackage-query.png)](https://travis-ci.org/juhp/stackage-query)
[![Hackage](http://img.shields.io/hackage/v/stackage-query.png)](http://hackage.haskell.org/package/stackage-query)
[![Stackage LTS](http://stackage.org/package/stackage-query/badge/lts)](http://stackage.org/lts/package/stackage-query)
[![Stackage Nightly](http://stackage.org/package/stackage-query/badge/nightly)](http://stackage.org/nightly/package/stackage-query)

# Stackage-Query

Small tool that queries package versions on Stackage
via the stackage-nightly and lts-haskell repos.

Example usage:

```
$ stackage package lts-7 pandoc
1.17.1
$ stackage core lts-8
<list of ghc library versions>
$ stackage packages lts | wc -l
2269
$ stackage consumers lts --minimum 500
542 mtl
824 text
$ stackage users nightly pandoc
BlogLiterately BlogLiterately-diagrams hakyll pandoc pandoc-citeproc patat wai-middleware-content-type yesod-markdown
$ stackage latest nightly
nightly-2017-04-19
```
