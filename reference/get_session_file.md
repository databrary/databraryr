# Get Session File Data From A Databrary Volume

Get Session File Data From A Databrary Volume

## Usage

``` r
get_session_file(
  vol_id = 1,
  session_id = 9578,
  file_id = 27227,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- vol_id:

  An integer indicating the volume identifier. Default is 1.

- session_id:

  An integer indicating a valid session/slot identifier linked to a
  volume. Default value is 9578.

- file_id:

  An integer indicating the file identifier. The default is 27227.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An httr2 request object.

## Value

Metadata about the file if the user has read privileges.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
get_session_file(vol_id = 2, session_id = 11, file_id = 3)
# A video from volume 1, session 11.
} # }
# }
```
