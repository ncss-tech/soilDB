# Filter KSSL Geochemical Table

A function to subset KSSL "geochem" / elemental analysis result table to
obtain rows/columns based on: column name, preparation code, major /
trace element method.

## Usage

``` r
filter_geochem(
  geochem,
  columns = NULL,
  prep_code = NULL,
  major_element_method = NULL,
  trace_element_method = NULL
)
```

## Arguments

- geochem:

  geochemical data, as returned by fetchKSSL

- columns:

  Column name(s) to include in result

- prep_code:

  Character vector of prep code(s) to include in result.

- major_element_method:

  Character vector of major element method(s) to include in result.

- trace_element_method:

  Character vector of trace element method(s) to include in result.

## Value

A data.frame, subset according to the constraints specified in
arguments.

## Author

Andrew G. Brown.
