# Revision history for stackage-query

## 0.1.3  -- 2018-11-15
- bundle Stackage.Types from old stackage-types package
- updates to build with Cabal-2 etc

## 0.1.2  -- 2017-09-11
* drop update command
* always update to latest Stackage data from git
* error if no package found

## 0.1.1  -- 2017-07-02

* new commands for `constraints`, `dependencies`, `executables`, and `modules`
* `tools` command can now take optional package arg
* `latest` command now git pulls
* `update` command shows new shortlog
* git pull is now quiet
* add --version option

## 0.1.0  -- 2017-04-24

* initial release on Hackage with commands to query package versions,
  reverse deps, Stackage owners, latest snapshots, and no of consumers.

# Local Variables:
# mode: text
# End:
