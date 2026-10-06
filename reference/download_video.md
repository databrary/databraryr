# Download a Video Asset via Signed URL.

Download a Video Asset via Signed URL.

## Usage

``` r
download_video(
  vol_id = 1,
  session_id = 9807,
  asset_id = 1,
  file_name = NULL,
  target_dir = tempdir(),
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- vol_id:

  Volume identifier containing the session.

- session_id:

  Session identifier containing the asset.

- asset_id:

  Asset identifier for the video file.

- file_name:

  Optional explicit file name. Defaults to the API-provided value.

- target_dir:

  Directory to save the downloaded file. Defaults to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  Optional `httr2` request object reused when requesting the signed
  link.

## Value

Path to the downloaded video or `NULL` on failure.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
download_video() # Default public video from volume 1
download_video(vol_id = 1, session_id = 9825, asset_id = 11643,
               file_name = "rdk.mp4")
} # }
# }
```
