# Download an Asset via Signed Link.

Databrary serves assets through short-lived, signed URLs. This helper
requests the signed link for a session asset and streams the file to the
requested directory.

## Usage

``` r
download_session_asset(
  vol_id = 1,
  session_id = 9807,
  asset_id = 1,
  file_name = NULL,
  target_dir = tempdir(),
  timeout_secs = REQUEST_TIMEOUT,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- vol_id:

  Integer. Volume identifier. Default is 1.

- session_id:

  Integer. Session identifier. Default is 9807.

- asset_id:

  Integer. Asset identifier within the session. Default is 1.

- file_name:

  Optional character string. Target file name. Defaults to the
  API-provided file name.

- target_dir:

  Character string. Directory where the file will be saved. Default is
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- timeout_secs:

  Numeric. Timeout (seconds) applied to the download request. Default is
  `REQUEST_TIMEOUT`.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Default is `NULL`, in which case a default
  authenticated request is generated.

## Value

The path to the downloaded file (character string) or `NULL` if the
download fails.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
download_session_asset() # Default public asset in volume 1
download_session_asset(vol_id = 1, session_id = 9825, asset_id = 11643,
                       file_name = "rdk.mp4")
} # }
# }
```
