# NASIS Local Database

This is a guide on using databases that follow the NASIS schema. Most of
the time users are querying an instance of the Microsoft SQL Server
NASIS local transactional database running on their computer. It is
possible to create file-based "snapshots" of a local instance of the
NASIS database using SQLite. See `[createStaticNASIS()]` for details.
These file-based snapshots, or other custom connections, can generally
be specified to NASIS-related functions via the `dsn` argument.

## Working With Coded Values and Decoding

Some values (choice lists) in NASIS are conventionally stored using
numeric codes. The codes are defined by "domain" and allow for both
"names" and "labels" as well as other descriptive information to be
provided for each choice list element. See
[`get_NASIS_column_metadata()`](http://ncss-tech.github.io/soilDB/reference/get_NASIS_metadata.md)
for details.

Many soilDB functions call the function
[`uncode()`](http://ncss-tech.github.io/soilDB/reference/uncode.md)
internally to handle conversion to human-readable values using official
NASIS domains. If writing queries directly against the database source,
such as a connection created with
[`NASIS()`](http://ncss-tech.github.io/soilDB/reference/dbConnectNASIS.md)
or query run with
[`dbQueryNASIS()`](http://ncss-tech.github.io/soilDB/reference/dbQueryNASIS.md),
you call
[`uncode()`](http://ncss-tech.github.io/soilDB/reference/uncode.md) on
the *data.frame* result of your query. Conversion of internal values to
choice list names is based on domains associated with result column
names.

When using a custom SQLite database, sometimes values in the database
are delivered pre-decoded to make the database more directly usable. An
example of this would be the Kellogg Soil Survey Laboratory morphologic
database, the NASIS data corresponding to the laboratory analyses
available through the [Lab Data Mart
(LDM)](http://ncss-tech.github.io/soilDB/reference/fetchLDM.md).

To avoid issues with offsets between internal storage value and external
readable value (for data such as farmland classification or Munsell
color value and chroma), you should not call
[`uncode()`](http://ncss-tech.github.io/soilDB/reference/uncode.md)
multiple times. Also, you can disable the "decoding" behavior made
internally in soilDB functions by setting
`options(soilDB.NASIS.skip_uncode = TRUE)`.
