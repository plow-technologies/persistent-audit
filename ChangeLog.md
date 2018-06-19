# Revision history for persistent-audit

## 0.2.0.1 -- 2018-06-19

* Update README.md.
* Switch from cabal file to hpack.
* Add example to test the command line tool.

## 0.2.0.0  -- 2016-09-08

* Move parsing functions and types to their own package - persistent-parser.
* Move Main.hs to its own directory.
* Add tests for all audit queries.

## 0.1.0.3  -- 2016-08-16

* Add four queries: insertUniqueAndAudit, deleteByAndAudit, repsertAndAudit, replaceAndAudit

## 0.1.0.2  -- 2016-08-11

* Add ToJSON, FromJSON and Hashable instances for AuditAction.
* Clean up some compiler warnings.

## 0.1.0.1  -- 2016-05-03

* Fix Haddock documentation.

## 0.1.0.0  -- 2016-05-02

* First version.
