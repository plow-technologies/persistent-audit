# persistent-audit

This package includes an executable that takes a Persistent models (tables) file and returns a Persistent models file with audit models (tables) for each individual table in the original file. It can optionally return a file with an implementation of the `ToAudit` class for each pair of model and its audit model. The automatically produced audit model files and `ToAudit` instances are a best attempt and will not necessarily compile without editing. If you are interested in having different styles of Audit models supported or find any bugs, please inform me. It currently supports Persistent >= 2.2 && <= 2.5.

## Cabal

The executable can be run like this:

#### Produce audit types

```
cabal run -- -m models -a auditModels
```

#### Produce audit types from Haskell file

It can also parse Haskell files that have Persist model QuasiQuoters `[persistLowerCase||]` and `[persistUpperCase||]`:

```
cabal run -- -m Models.hs -a auditModels
```

#### Produce ToAudit instances

And if you want the optional `ToAudit` implementation:

```
cabal run -- -m models -a auditModels -i ToAuditInstances.hs
```

#### Produce cross database ToAudit instances

It can also help you build Audit Models that will be used in a separate database (MongoDB to SQL or SQL to MongoDB) by converting MongoDB keys to Bytestring or SQL keys to Int64.

```
cabal run -- -m models -a auditModels -i ToAuditInstances.hs -c mongoDbToSql
cabal run -- -m models -a auditModels -i ToAuditInstances.hs -c sqlToMongoDb
```

## Stack

#### Produce audit types

```
stack exec persistent-audit -- -m examples/models -a examples/auditModels
```

#### Produce audit types from Haskell file

```
stack exec persistent-audit -- -m examples/Models.hs -a examples/auditModels
```

#### Produce ToAudit instances

```
stack exec persistent-audit -- -m examples/models -a examples/auditModels -i examples/ToAuditInstances.hs
```

#### Produce cross database ToAudit instances

```
stack exec persistent-audit -- -m examples/models -a examples/auditModels -i examples/ToAuditInstances.hs -c mongoDbToSql
stack exec persistent-audit -- -m examples/models -a examples/auditModels -i examples/ToAuditInstances.hs -c sqlToMongoDb
```

## Other use cases

You can also use this as a library for the following purporses:

 * To parse Persistent model files
 * To generate Audit Models from other Models
 * To generate `ToAudit` instances
 * Use query functions from `Database.Persist.Audit.Queries`:`insertAndAudit`, `deleteAndAudit`, `deleteWhereAndAudit`, `updateAndAudit`, `updateWhereAndAudit`, `insertUniqueAndAudit`, `deleteByAndAudit`, `repsertAndAudit`, `replaceAndAudit`


This package is originally based on this article [Maintaining a Log of Database Changes](http://www.4guysfromrolla.com/webtech/041807-1.shtml). Specifically, the first example: 'A Separate "History" Table for Each Table Being Audited'.
