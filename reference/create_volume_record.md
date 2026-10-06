# Create Record in Databrary Volume

Create a new record in a Databrary volume. Records contain metadata
organized by category (e.g., participant, condition, task) with measures
(field values) stored per metric. The `name` is required and resolved to
the category's name/ID metric automatically. Use `measures` to add
additional metric values.

## Usage

``` r
create_volume_record(
  vol_id = 1,
  category_id,
  name,
  measures = list(),
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- vol_id:

  Target volume number. Must be a positive integer.

- category_id:

  Numeric category identifier for the record type (e.g., participant,
  condition, task).

- name:

  Display name for the record (e.g., "P001", "Control group"). Required;
  resolves to the category's name metric from volume configuration.

- measures:

  Optional named list mapping additional metric IDs (as strings) to
  values. Values can be strings (for text metrics), numbers (for numeric
  metrics), or lists with `year`, `month`, `day`, optional `month` and
  `day` fields (for date metrics).

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

Same shape as
[`get_volume_record_by_id`](https://databrary.github.io/databraryr/reference/get_volume_record_by_id.md),
or `NULL` if creation fails.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
# Create a task record with name only
create_volume_record(
  vol_id = 1,
  category_id = 6,
  name = "Control group"
)

# Create a record with name and additional measures
create_volume_record(
  vol_id = 1,
  category_id = 6,
  name = "Task A",
  measures = list("30" = "Extra value")
)

# Create a participant record with name and birthdate measure
create_volume_record(
  vol_id = 1,
  category_id = 1,
  name = "P001",
  measures = list("4" = list(year = 2020, month = 3, day = 15))
)
} # }
# }
```
