# Get soilDB User Directory Path

Wrapper around
[`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html) for
soilDB-specific file storage in a platform-specific, user-level
directory. Requires R \>= 4.0.

These directories can be used for storing assets used as input to other
soilDB functions, or for caching results to allow for offline use or
fewer repeated requests to remote sources.

Use argument `remove=TRUE` to delete files stored in the specified
directories.

## Usage

``` r
soilDB_user_dir(
  which = c("data", "config", "cache"),
  ...,
  create = TRUE,
  remove = FALSE,
  fsep = .Platform$file.sep,
  mustWork = NA
)
```

## Arguments

- which:

  *character*. One of: `"data"`, `"config"`, or `"cache"`. Passed to
  [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html).

- ...:

  *character*. Sub-directories. Additional arguments are passed to
  [`file.path()`](https://rdrr.io/r/base/file.path.html)

- create:

  *logical*. When `TRUE` (default) the directory path is created
  (recursively).

- remove:

  *logical*. When `TRUE` the contents of the folder(s) are deleted
  (recursively). Default: `FALSE`.

- fsep:

  *character*. File path separator. Passed to
  [`file.path()`](https://rdrr.io/r/base/file.path.html) and
  [`normalizePath()`](https://rdrr.io/r/base/normalizePath.html).
  Default: `.Platform$file.sep`

- mustWork:

  *logical*. Throw error if path cannot be normalized; passed to
  [`normalizePath()`](https://rdrr.io/r/base/normalizePath.html).
  Default `NA` generates a warning.

## Value

*character*. File paths within specified soilDB user directory. When
`remove=TRUE` result is `NULL`.

## Details

If the directory path does not exist it is created (recursively) when
`create=TRUE`. Output paths are normalized using
[`normalizePath()`](https://rdrr.io/r/base/normalizePath.html) and the
specified path separator (`fsep`).

## Author

Andrew Gene Brown

## Examples

``` r
soilDB_user_dir("data", c("dataset1", "dataset2"), "source", create = FALSE)
#> Warning: path[1]="/home/runner/.local/share/R/soilDB/dataset1/source": No such file or directory
#> Warning: path[2]="/home/runner/.local/share/R/soilDB/dataset2/source": No such file or directory
#> [1] "/home/runner/.local/share/R/soilDB/dataset1/source"
#> [2] "/home/runner/.local/share/R/soilDB/dataset2/source"
```
