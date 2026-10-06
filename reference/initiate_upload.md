# Initiate a File Upload

Ask the Databrary API to register a pending upload and return the signed
URL(s) needed to PUT file bytes directly to object storage. The server
decides whether to issue a single-PUT or a multipart upload based on
`file_size` and the deployment's storage backend (S3 multipart on the
AI/AWS deployment, single PUT on the on-prem core deployment).

This is the first step of the upload pipeline; pass the returned object
to
[`upload_file`](https://databrary.github.io/databraryr/reference/upload_file.md)
(high level) or use the returned URLs directly with
[`httr2::req_perform()`](https://httr2.r-lib.org/reference/req_perform.html)
for fine-grained control.

## Usage

``` r
initiate_upload(
  filename,
  destination_type,
  object_id,
  file_size = NULL,
  content_type = NULL,
  source_session_id = NULL,
  source_folder_id = NULL,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- filename:

  Display filename. Required, non-empty.

- destination_type:

  Where the upload will live. One of `"session"`, `"folder"`,
  `"linked_volume_session"`, `"linked_volume_folder"` (server
  validates). Required.

- object_id:

  Positive integer ID of the destination object (session or folder,
  depending on `destination_type`). Required.

- file_size:

  File size in bytes. Optional but strongly recommended: the AWS
  deployment uses this to decide multipart vs single PUT.

- content_type:

  MIME type of the file (e.g. `"video/mp4"`). Optional but recommended;
  some storage backends require it.

- source_session_id:

  Required when `destination_type == "linked_volume_session"`; positive
  integer.

- source_folder_id:

  Required when `destination_type == "linked_volume_folder"`; positive
  integer.

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A named list describing the upload. Always contains `status_url`. For a
single PUT (the on-prem core deployment, or any AWS upload smaller than
the multipart threshold), contains `signed_upload_url` and
`required_headers` (may be empty). For a multipart upload, contains
`upload_type = "multipart"`, `upload_guid`, `s3_upload_id`, `part_urls`
(a list of `list(part_number, url)`), and `part_size`. Returns `NULL` if
the request fails.

## See also

[`upload_file`](https://databrary.github.io/databraryr/reference/upload_file.md),
[`get_upload_status`](https://databrary.github.io/databraryr/reference/get_upload_status.md),
[`complete_upload`](https://databrary.github.io/databraryr/reference/complete_upload.md)

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
# Initiate a small upload to a session
init <- initiate_upload(
  filename = "clip_001.mp4",
  destination_type = "session",
  object_id = 42,
  file_size = 1024L * 1024L,
  content_type = "video/mp4"
)
} # }
# }
```
