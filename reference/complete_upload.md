# Complete a Multipart Upload

Tell the Databrary API that all parts of a multipart upload have been
PUT to S3 and the object can be assembled. The server forwards the part
list to S3's `CompleteMultipartUpload`, transitioning the upload into
the post-upload pipeline (virus scan, format probe).

Only needed when
[`initiate_upload()`](https://databrary.github.io/databraryr/reference/initiate_upload.md)
returned `upload_type == "multipart"`. Single-PUT uploads complete
implicitly once the object lands in storage.

## Usage

``` r
complete_upload(
  upload_guid,
  s3_upload_id,
  parts,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- upload_guid:

  Character GUID returned in `initiate_upload()$upload_guid`.

- s3_upload_id:

  Character S3 upload ID returned in `initiate_upload()$s3_upload_id`.

- parts:

  A list of one entry per part, each a list with `part_number` (positive
  integer) and `etag` (character, from the `ETag` response header of the
  part PUT). Order is not significant – the server sorts by
  `part_number`.

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

`TRUE` on success, `NULL` on failure.

## See also

[`initiate_upload`](https://databrary.github.io/databraryr/reference/initiate_upload.md),
[`upload_file`](https://databrary.github.io/databraryr/reference/upload_file.md)

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
complete_upload(
  upload_guid = "abc...",
  s3_upload_id = "xyz...",
  parts = list(
    list(part_number = 1L, etag = "etag-of-part-1"),
    list(part_number = 2L, etag = "etag-of-part-2")
  )
)
} # }
# }
```
