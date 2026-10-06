# Get the Current Status of an Upload

Poll the Databrary API for the lifecycle state of an upload started with
[`initiate_upload`](https://databrary.github.io/databraryr/reference/initiate_upload.md).
The returned status reflects both the storage-side transfer and the
post-upload server pipeline (e.g. virus scan, format probe).

Accepts either the absolute `status_url` returned by
[`initiate_upload()`](https://databrary.github.io/databraryr/reference/initiate_upload.md)
(preferred – avoids a second URL build) or the upload's `upload_guid`.

## Usage

``` r
get_upload_status(
  status_url = NULL,
  upload_guid = NULL,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- status_url:

  Absolute URL returned by
  [`initiate_upload()`](https://databrary.github.io/databraryr/reference/initiate_upload.md)
  under `status_url`. Mutually exclusive with `upload_guid`.

- upload_guid:

  Character upload GUID. Mutually exclusive with `status_url`.

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A length-1 character string with the status (e.g. `"scanning"`,
`"completed"`, `"infected"`, `"upload_failed"`, `"processing_failed"` on
the AWS deployment; raw `Upload.status` on the core deployment). Returns
`NULL` if the request fails.

## See also

[`initiate_upload`](https://databrary.github.io/databraryr/reference/initiate_upload.md),
[`upload_file`](https://databrary.github.io/databraryr/reference/upload_file.md)

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
init <- initiate_upload(
  filename = "clip.mp4",
  destination_type = "session",
  object_id = 42
)
get_upload_status(status_url = init$status_url)
} # }
# }
```
