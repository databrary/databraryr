# Check Whether Filenames Already Exist in a Folder

Ask the server which of the supplied filenames already exist as files in
the given folder. Useful before bulk uploads to detect name collisions
in advance.

## Usage

``` r
check_duplicate_files_in_folder(
  vol_id = 1,
  folder_id,
  filenames,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- vol_id:

  Target volume number. Must be a positive integer.

- folder_id:

  Numeric folder identifier. Must be a positive integer.

- filenames:

  Character vector of filenames to check. Length must be at least 1;
  each element must be a non-empty string.

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A `tibble` with columns `filename` (character) and `exists` (logical),
one row per input filename and in the same order. Returns `NULL` if the
request fails. Against current staging API, a missing `folder_id` still
yields a successful response with `exists = FALSE` for each filename
(nothing matches that folder).

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
check_duplicate_files_in_folder(
  vol_id = 1,
  folder_id = 42,
  filenames = c("clip_001.mp4", "clip_002.mp4")
)
} # }
# }
```
