# Format vector of values into a string suitable for an SQL `IN` statement.

Concatenate a vector to SQL `IN`-compatible syntax: `letters[1:3]`
becomes `('a','b','c')`. Values in `x` are first passed through
[`unique()`](https://ncss-tech.github.io/aqp/reference/unique.html).

## Usage

``` r
format_SQL_in_statement(x)
```

## Arguments

- x:

  A character vector.

## Value

A character vector (unit length) containing concatenated group syntax
for use in SQL `IN`, with unique value found in `x`.

## Note

Only `character` output is supported.

## Examples

``` r
format_SQL_in_statement(c(2648889L, 2648890L))
#> [1] "('2648889','2648890')"
```
