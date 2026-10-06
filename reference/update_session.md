# Replace a Session in Databrary Volume (PUT)

Sends a PUT request to fully replace a session's writable fields. `name`
is required (non-empty); other fields are optional and default to
server-side values when omitted (the underlying serializer marks them
`required=FALSE`). Use
[`patch_session`](https://databrary.github.io/databraryr/reference/patch_session.md)
for partial updates when you don't want full-replacement semantics.

## Usage

``` r
update_session(
  vol_id = 1,
  session_id,
  name,
  release_level = NULL,
  source_date = NULL,
  date = NULL,
  date_precision = NULL,
  default_records = NULL,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- vol_id:

  Target volume number. Must be a positive integer.

- session_id:

  Numeric session identifier. Must be a positive integer.

- name:

  New session name. Required, non-empty after trim.

- release_level:

  Optional release level (e.g. `"PRIVATE"`, `"SHARED"`, `"EXCERPTS"`,
  `"PUBLIC"`).

- source_date:

  Optional session date. A length-1 `Date` object or ISO `"YYYY-MM-DD"`
  string. Mutually exclusive with `date`.

- date:

  Optional structured date list with named fields `year`, `month`,
  `day`, and optional `is_estimated` (logical). Mutually exclusive with
  `source_date`.

- date_precision:

  Optional precision for `date`: e.g. `"FULL"`, `"YEAR"`.

- default_records:

  Optional integer vector of record IDs. The server replaces the
  session's current default records with this set; omit to leave them
  unchanged.

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A list with the updated session's metadata (same shape as
[`get_session_by_id`](https://databrary.github.io/databraryr/reference/get_session_by_id.md)),
or `NULL` if the update fails.

## See also

[`patch_session`](https://databrary.github.io/databraryr/reference/patch_session.md),
[`create_session`](https://databrary.github.io/databraryr/reference/create_session.md)

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
# Rename a session via PUT
update_session(vol_id = 1, session_id = 42, name = "Replacement name")
} # }
# }
```
