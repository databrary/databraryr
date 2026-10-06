# Request a Signed ZIP Download for a Folder.

Folder-level ZIP archives are prepared asynchronously by the Django API.
Calling `download_folder_zip()` queues the job and returns a processing
task descriptor. When the archive is ready, Databrary emails a signed
download link to the authenticated user.

## Usage

``` r
download_folder_zip(
  vol_id = 1,
  folder_id = 9807,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- vol_id:

  Volume identifier for the folder. Must be a positive integer. Default
  is 1.

- folder_id:

  Folder identifier scoped within the specified volume. Must be a
  positive integer. Default is 9807.

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
download_folder_zip() # Volume 1, folder 9807
} # }
# }
```
