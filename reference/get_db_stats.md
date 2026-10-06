# Get Stats About Databrary

Returns basic summary information about the institutions, people, and
video data hosted on Databrary.

## Usage

``` r
get_db_stats(type = "stats", vb = options::opt("vb"), rq = NULL)
```

## Arguments

- type:

  Type of Databrary report to run "institutions", "people", "data"

- vb:

  Show verbose messages. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object.

## Value

A data frame with the requested data or NULL if there is no new
information.

## Examples

``` r
if (FALSE) { # \dontrun{
get_db_stats()
get_db_stats("stats")
} # }
```
