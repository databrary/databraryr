# Request a Signed ZIP Download for a Session.

The Django API prepares session-level ZIP archives asynchronously.
Calling `download_session_zip()` triggers the job and returns a
processing task summary. Once the archive is ready, Databrary emails a
signed download link to the authenticated user.

## Usage

``` r
download_session_zip(
  vol_id = 31,
  session_id = 9803,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- vol_id:

  Volume identifier that owns the session. Must be a positive integer.
  Default is 31.

- session_id:

  Session identifier within the volume. Must be a positive integer.
  Default is 9803.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Default is `NULL`, in which case a default
  authenticated request is generated.

## Value

A list describing the processing task (`status`, `message`, `task_id`)
or `NULL` when the request fails.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
download_session_zip(vol_id = 31, session_id = 9803)
} # }
# }
```
