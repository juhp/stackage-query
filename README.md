# Stackage-Query

Small tool that queries (returns url of) package versions on Stackage.

This is just a simple hack that looks at Stackage's HTTP redirects
to determine the package version URL.

Example usage:

```
$ stackage list lts pandoc
https://www.stackage.org/lts-7.14/package/pandoc-1.17.1
$ stackage config lts
$ stackage buildplan lts pandoc
array 0.5.1.1
:
pandoc 1.17.1
```
