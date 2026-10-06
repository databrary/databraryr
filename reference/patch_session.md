# Partially Update a Session in Databrary Volume

Sends a PATCH request to update selected fields of an existing session.
Only provided arguments are sent; omitted fields are left unchanged on
the server. Note that `default_records` is a full replacement on the
server – supplying it overwrites the entire current set of defaults;
omit it to keep them. To change just one default record use
[`add_default_record_to_session`](https://databrary.github.io/databraryr/reference/add_default_record_to_session.md)
or
[`remove_default_record_from_session`](https://databrary.github.io/databraryr/reference/remove_default_record_from_session.md).

## Usage

``` r
patch_session(
  vol_id = 1,
  session_id,
  name = NULL,
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

  Optional new session name. If provided, must be a non-empty length-1
  string.

- release_level:

  Optional release level (e.g. `"PRIVATE"`, `"SHARED"`, `"EXCERPTS"`,
  `"PUBLIC"`). Server validates the choice.

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
  session's current default records with this set.

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A list with the updated session's metadata (same shape as
[`get_session_by_id`](https://databrary.github.io/databraryr/reference/get_session_by_id.md)),
or `NULL` if the update fails or no fields were provided.

## See also

[`update_session`](https://databrary.github.io/databraryr/reference/update_session.md),
[`create_session`](https://databrary.github.io/databraryr/reference/create_session.md),
[`add_default_record_to_session`](https://databrary.github.io/databraryr/reference/add_default_record_to_session.md),
[`remove_default_record_from_session`](https://databrary.github.io/databraryr/reference/remove_default_record_from_session.md)

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
# Rename a session
patch_session(vol_id = 1, session_id = 42, name = "Renamed session")

# Update the date and precision
patch_session(
  vol_id = 1,
  session_id = 42,
  date = list(year = 2024, month = 3, day = 15),
  date_precision = "FULL"
)
} # }
# }
```
