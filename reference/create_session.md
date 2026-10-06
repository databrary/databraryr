# Create Session in Databrary Volume

Create a new session in a Databrary volume. A session (a.k.a. "slot")
groups files and metadata for a single recording or testing event.
`name` is required and must be non-empty. Provide either a flat
`source_date` or a structured `date` list (with optional
`date_precision`) to record when the session occurred – not both.

## Usage

``` r
create_session(
  vol_id = 1,
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

- name:

  Display name for the session. Required, non-empty after trim.

- release_level:

  Optional release level for the session (e.g. `"PRIVATE"`, `"SHARED"`,
  `"EXCERPTS"`, `"PUBLIC"`). The server validates the choice.

- source_date:

  Optional session date. A length-1 `Date` object or ISO `"YYYY-MM-DD"`
  string. Mutually exclusive with `date`.

- date:

  Optional structured date list with named fields `year`, `month`,
  `day`, and optional `is_estimated` (logical). Mutually exclusive with
  `source_date`.

- date_precision:

  Optional precision for `date`: e.g. `"FULL"`, `"YEAR"`. Server
  validates the choice.

- default_records:

  Optional integer vector of record IDs to set as default records on the
  new session.

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A list with the created session's metadata (same shape as
[`get_session_by_id`](https://databrary.github.io/databraryr/reference/get_session_by_id.md)),
or `NULL` if creation fails.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
# Minimal session
create_session(vol_id = 1, name = "Pilot 01")

# Session with a flat date
create_session(
  vol_id = 1,
  name = "Pilot 02",
  source_date = as.Date("2024-03-15")
)

# Session with a structured date and default records
create_session(
  vol_id = 1,
  name = "Pilot 03",
  date = list(year = 2024, month = 3, day = 15, is_estimated = FALSE),
  date_precision = "FULL",
  default_records = c(101, 102)
)
} # }
# }
```
