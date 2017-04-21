[![Build Status](https://travis-ci.org/juhp/stackage-query.png)](https://travis-ci.org/juhp/stackage-query)

# Stackage-Query

Small tool that queries package versions on Stackage.

Example usage:

```
$ stackage list lts-7 pandoc
pandoc-1.17.1
$ stackage core lts-8
<lists ghc library versions>
$ stackage packages lts | wc -l
2269
$ stackage consumers lts --minimum 500
542 mtl
824 text
$ stackage package lts-7 stack
1.1.2
$ stackage users nightly pandoc
BlogLiterately BlogLiterately-diagrams hakyll pandoc pandoc-citeproc patat wai-middleware-content-type yesod-markdown
$ stackage latest nightly
nightly-2017-04-19
```
