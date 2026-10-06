# Request a Signed ZIP Download for a Volume.

Volume-level ZIP archives are prepared asynchronously by the Django API.
Calling `download_volume_zip()` queues the job and returns a processing
task descriptor. When the archive is ready, Databrary emails a signed
download link to the authenticated user.

## Usage

``` r
download_volume_zip(vol_id = 31, vb = options::opt("vb"), rq = NULL)
```

## Arguments

- vol_id:

  An integer. Volume identifier. Default is 31.

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
download_volume_zip(vol_id = 31)
} # }
# }
```
