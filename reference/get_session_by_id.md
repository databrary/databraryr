# Get Session (Slot) Data From A Databrary Volume

Get Session (Slot) Data From A Databrary Volume

## Usage

``` r
get_session_by_id(
  session_id = 6256,
  vol_id = 1,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- session_id:

  An integer indicating a valid session/slot identifier linked to a
  volume. Default value is 6256 in volume 1.

- vol_id:

  An integer indicating the volume identifier. Default is 1.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An httr2 request object.

## Value

A JSON blob with the session data. If the user has previously logged in
to Databrary via
[`login_db()`](https://databrary.github.io/databraryr/reference/login_db.md),
then session(s) that have restricted access can be downloaded, subject
to the sharing release levels on those session(s).

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
get_session_by_id() # Default is Volume 1, Session 9807, the Materials folder
} # }
# }
```
