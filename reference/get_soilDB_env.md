# Get the soilDB environment used for storing error messages and quality control output

The soilDB package uses an environment to store variables that are
created as side effects of various data access and processing routines.
`get_soilDB_env()` provides a method to access this environment from the
global (user) environment.

## Usage

``` r
soilDB.env

get_soilDB_env()
```

## Format

An object of class `environment` of length 0.

## Value

a `environment` object

## Examples

``` r
get_soilDB_env()
#> <environment: 0x5629775b2d80>
```
