# Search For Tags on Volumes or Sessions.

Search For Tags on Volumes or Sessions.

## Usage

``` r
search_for_tags(search_string = "ICIS", vb = options::opt("vb"), rq = NULL)
```

## Arguments

- search_string:

  String to search.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Default is NULL.

## Value

An array of tags that match the tag_string.

## Examples

``` r
if (FALSE) { # \dontrun{
search_for_tags() # Searches for volumes that have the tag "ICIS"
} # }
```
