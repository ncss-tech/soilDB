# Parse contents of a web report, based on supplied arguments.

Parse contents of a web report, based on supplied arguments.

## Usage

``` r
parseWebReport(url, args, index = 1)
```

## Arguments

- url:

  Base URL to a LIMS/NASIS web report.

- args:

  List of named arguments to send to report, see details.

- index:

  Integer index specifying the table to return, or, NULL for a list of
  tables

## Value

A `data.frame` object in the case of a single integer `index`, otherwise
a `list`

## Details

Report argument names can be inferred by inspection of the HTML source
associated with any given web report.

## Note

Most web reports are for internal use only.

## Author

D.E. Beaudette and S.M. Roecker
