# Download a Folder Asset via Signed Link.

Databrary serves folder-scoped assets through signed URLs. This helper
requests the signed link for a folder asset and streams the file to the
specified directory.

## Usage

``` r
download_folder_asset(
  vol_id = 1,
  folder_id = 9807,
  asset_id = 1,
  file_name = "video.mp4",
  target_dir = tempdir(),
  timeout_secs = REQUEST_TIMEOUT,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- vol_id:

  Integer. Volume identifier containing the folder. Default is 1.

- folder_id:

  Integer. Folder identifier within the volume. Default is 9807, the
  Materials folder for Volume 1.

- asset_id:

  Integer. Asset identifier within the folder. Default is 1, a demo
  video called 'counting_demo_video.mp4'.

- file_name:

  Optional character string. File name to use when saving the asset.
  Defaults to the API-provided file name.

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
download_folder_asset() # Default public asset in folder 1 of volume 1
download_folder_asset(vol_id = 1, folder_id = 9807, asset_id = 1,
                      file_name = "video.mp4")
} # }
# }
```
