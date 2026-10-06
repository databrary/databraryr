# Report Information About A Funder.

Report Information About A Funder.

## Usage

``` r
search_for_funder(
  search_string = "national science foundation",
  approved_only = TRUE,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- search_string:

  String to search. Default is "national science foundation".

- approved_only:

  Logical. When TRUE (default) only approved funders are returned. Set
  to FALSE to include unapproved funders as well.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Default is NULL.

## Value

A data frame with information about the funder.

## Examples

``` r
if (FALSE) { # \dontrun{
search_for_funder("national+science+foundation")
} # }
```
