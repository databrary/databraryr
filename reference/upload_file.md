# Upload a Local File to Databrary

High-level wrapper around the upload pipeline:
[`initiate_upload`](https://databrary.github.io/databraryr/reference/initiate_upload.md)
-\> PUT bytes to the signed URL(s) -\>
[`complete_upload`](https://databrary.github.io/databraryr/reference/complete_upload.md)
(multipart only). Returns the metadata needed to poll progress with
[`get_upload_status`](https://databrary.github.io/databraryr/reference/get_upload_status.md).

Picks the upload mode the server returned: single PUT on the on-prem
core deployment or for small files on AWS, S3 multipart for large files
on AWS.

## Usage

``` r
upload_file(
  path,
  destination_type,
  object_id,
  filename = NULL,
  content_type = NULL,
  source_session_id = NULL,
  source_folder_id = NULL,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- path:

  Path to the local file. Must exist and be readable.

- destination_type:

  See
  [`initiate_upload`](https://databrary.github.io/databraryr/reference/initiate_upload.md).

- object_id:

  See
  [`initiate_upload`](https://databrary.github.io/databraryr/reference/initiate_upload.md).

- filename:

  Optional display filename; defaults to `basename(path)`.

- content_type:

  Optional MIME type; auto-detected from the file extension when not
  supplied.

- source_session_id:

  See
  [`initiate_upload`](https://databrary.github.io/databraryr/reference/initiate_upload.md).

- source_folder_id:

  See
  [`initiate_upload`](https://databrary.github.io/databraryr/reference/initiate_upload.md).

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A named list with `upload_guid` (when known), `status_url`, and
`upload_type`. Returns `NULL` if any step fails.

## See also

[`initiate_upload`](https://databrary.github.io/databraryr/reference/initiate_upload.md),
[`get_upload_status`](https://databrary.github.io/databraryr/reference/get_upload_status.md),
[`complete_upload`](https://databrary.github.io/databraryr/reference/complete_upload.md)

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
info <- upload_file(
  path = "/tmp/clip.mp4",
  destination_type = "session",
  object_id = 42
)
get_upload_status(status_url = info$status_url)
} # }
# }
```
