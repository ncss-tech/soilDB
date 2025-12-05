# NASIS 7 Metadata

NASIS 7 Metadata from MetadataDomainDetail, MetadataDomainMaster, and
MetadataTableColumn tables

## Format

A `data.frame` with the following columns:

- `DomainID` - Integer. ID that uniquely identifies a domain in a data
  model, not just within a database.

- `DomainName` - Character. Domain Name.

- `DomainRanked` - Integer. Is domain ranked? `0` = No; `1` = Yes

- `DisplayLabel` - Character. Domain Display Label.

- `ChoiceSequence` - Integer. Order or sequence of Choices.

- `ChoiceValue` - Integer. Value of choice level.

- `ChoiceName` - Character. Name of choice level.

- `ChoiceLabel` - Character. Label of choice level.

- `ChoiceObsolete` - Integer. Is choice level obsolete? `0` = No; `1` =
  Yes

- `ColumnPhysicalName` - Character. Physical column name.

- `ColumnLogicalName` - Character. Logical column name.
