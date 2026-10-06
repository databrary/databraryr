# Request a Session or Volume CSV Export.

The Django API generates CSV reports asynchronously. This function
queues a CSV export for a specific session when `session_id` is
supplied, or for the entire volume when `session_id` is `NULL`. The API
delivers the final signed download link via email once the export is
ready.

## Usage

``` r
download_session_csv(
  vol_id = 1,
  session_id = NULL,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- vol_id:

  Integer. Target volume identifier. Default is 2.

- session_id:

  Optional integer. When provided, requests a session-level CSV export.
  When `NULL`, a volume-level CSV export is requested. Default is 9.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Default is `NULL`, meaning a default
  authenticated request is generated.

## Value

A list describing the processing task (`status`, `message`, `task_id`)
or `NULL` if the request fails.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
# Request a volume-wide CSV export
download_session_csv() # CSV for default volume 2

# Request a session-specific CSV export
download_session_csv(vol_id = 2, session_id = 9)
} # }
# }
```
