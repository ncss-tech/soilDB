# Generate chunk labels for splitting data

Generate chunk labels for splitting data

## Usage

``` r
makeChunks(ids, size = 100)
```

## Arguments

- ids:

  vector of IDs

- size:

  chunk (group) size

## Value

A numeric vector

## Examples

``` r
# split the lowercase alphabet into 2 chunks

aggregate(letters,
          by = list(makeChunks(letters, size=13)),
          FUN = paste0, collapse=",")
#>   Group.1                         x
#> 1       1 a,b,c,d,e,f,g,h,i,j,k,l,m
#> 2       2 n,o,p,q,r,s,t,u,v,w,x,y,z
```
