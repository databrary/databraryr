
<!-- README.md is generated from README.Rmd. Please edit that file -->



# databraryr <a href="https://databrary.github.io/databraryr/"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![CRAN status](https://www.r-pkg.org/badges/version/databraryr)](https://CRAN.R-project.org/package=databraryr)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/databrary/databraryr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/databrary/databraryr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

databraryr is a wrapper for the [Databrary](https://databrary.org) data library's application programming interface (API).
The package can be used to create reproducible data wrangling, analysis, and visualization pipelines from data stored and shared on Databrary.

## Installation


``` r
# The easiest way to install databraryr is from CRAN
install.packages("databraryr")

# The development release can be installed from GitHub
install.packages("pak")
pak::pak("databrary/databraryr")
```

## Usage

Databrary ([databrary.org](https://databrary.org)) is a restricted-access research data library specialized for storing and sharing video with capabilities of storing [other types](https://nyu.databrary.org/asset/formats/) of associated data.
Access to restricted data requires registration and formal approval by an institution.
The registration process involves the creation of an (email-account-based) user account and secure password.
Once institutional authorization has been granted, a user may gain access to shared video, audio, and other data.
See <https://databrary.org/about.html> for more information about gaining access to restricted data.

All API calls require OAuth2 authentication. Before
running the examples below, ensure you have set the following environment variables
or stored values with `login_db(store = TRUE)`:

- `DATABRARY_CLIENT_ID`
- `DATABRARY_CLIENT_SECRET`
- `DATABRARY_LOGIN` (your Databrary account email)
- `DATABRARY_PASSWORD` (optional; prompted securely via **getPass** if missing and not in the keyring)
- `DATABRARY_BASE_URL` (optional; defaults to `https://api.databrary.org`; use this for staging or a custom API host)

You can put these in your user or project `.Renviron` (for example `file.edit("~/.Renviron")` in R, or `usethis::edit_r_environ()` if you use the **usethis** package).


``` r
library(databraryr)

login_db()

whoami()

get_db_stats()

list_volume_assets(vol_id = 1) |>
  head()
```

Illustrative output (depends on your account, API host, and when you run the code):

``` text
#> Welcome to the databraryr package.

#> # A tibble: 1 × 11
#>   date                institutions affiliates investigators hours_of_recordings
#>   <dttm>                     <int>      <int>         <int>               <int>
#> 1 2026-05-18 12:05:57          120      3400          2100               95000
#> # ℹ 6 more variables: authorized_users <int>, total_volumes <int>,
#> #   public_volumes <int>, total_files <int>, total_duration_hours <dbl>,
#> #   total_storage_tb <dbl>
#> # (from get_db_stats(); default type aggregates summary counters)

#> # A tibble: 6 × 19
#>   asset_id asset_name                asset_permission asset_size asset_mime_type
#>      <int> <chr>                     <chr>                 <dbl> <chr>          
#> 1     9826 Introduction              public             88610655 video/mp4      
#> 2     9828 Databrary demo            public            917124852 video/mp4      
#> 3     9830 Databrary 1               public            899912341 video/mp4      
#> 4     9832 Datavyu                   public            764340542 video/mp4      
#> 5    22412 Slides                    public              4573426 application/pdf
#> 6     9834 Overview and Policy Upda… public           1301079971 video/mp4      
#> # ℹ 14 more variables: asset_format_id <int>, asset_format_name <chr>,
#> #   asset_duration <dbl>, asset_created_at <chr>, asset_updated_at <chr>,
#> #   asset_sha1 <chr>, asset_thumbnail_url <chr>, session_id <int>,
#> #   session_name <chr>, session_date <chr>, session_release <chr>,
#> #   asset_uploader_id <int>, asset_uploader_first_name <chr>,
#> #   asset_uploader_last_name <chr>
#> # (`asset_duration` / `asset_thumbnail_url` are NA when the API omits them.)
```

## Bulk operations

Functions whose names start with `bulk_*` run many API requests in order (sessions, folders, session files, volume records). Each returns a **tibble** with one row per item you asked to process:

| Column | Meaning |
|--------|---------|
| `input` | That row’s key (e.g. local path, session id, folder name) |
| `status` | `"success"`, `"failed"`, `"skipped"` (e.g. duplicate basename on upload preflight), or `"pending"` |
| `result` | List column with the API payload when `status` is `"success"` |
| `error` | Error message when `status` is `"failed"` |
| `reason` | Extra detail when relevant (e.g. `"duplicate"` for skipped uploads) |

### `on_error`, retries, and conditions

- **`on_error = "stop"`** (default): first failed row stops execution with an error of class **`databraryr_bulk_error`**. In **`tryCatch(..., databraryr_bulk_error = function(e) e$partial)`**, use **`e$partial`** for the tibble of all rows so far (`"failed"` / `"pending"` for rows after the stop).
- **`on_error = "collect"`**: every row is attempted; failures stay `"failed"` and the function returns the full tibble instead of throwing.
- **`max_retries`** / **`retry_delay`**: optional per-row retries before counting a row as failed.

### Resuming with `resume_bulk()`

Use **`resume_bulk()`** to run the **same** bulk function again on only **incomplete** rows (`"pending"` or `"failed"`), and merge those outcomes back into the original tibble (order preserved).

Typical pattern after **`on_error = "stop"`**:


``` r
partial <- tryCatch(
  bulk_upload_files(vol_id = 1, session_id = 42, file_paths = my_paths),
  databraryr_bulk_error = function(e) e$partial
)

# Fix data or environment, then:
partial <- resume_bulk(partial, bulk_upload_files, vol_id = 1, session_id = 42)
```

Looping fix-and-resume (safe: each `e$partial` always reflects the full job, not only the last retried subset):

``` r
partial <- tryCatch(
  bulk_upload_files(vol_id = 1, session_id = 42, file_paths = my_paths),
  databraryr_bulk_error = function(e) e$partial
)
while (any(partial$status %in% c("pending", "failed"))) {
  # fix whatever caused the failure, then:
  partial <- tryCatch(
    resume_bulk(partial, bulk_upload_files, vol_id = 1, session_id = 42),
    databraryr_bulk_error = function(e) e$partial
  )
}
```

Pass every argument the bulk function needs via `...` except the **vector argument that defines rows** (`file_paths`, `session_ids`, `folder_ids`, `file_ids`, `session_names`, `folder_names`, `record_ids`, or `record_names`). That vector is filled automatically from `partial$input` for incomplete rows; pass **`input_arg = "..."`** if auto-detection is wrong.

**Rename helpers** (`bulk_rename_sessions`, `bulk_rename_folders`, `bulk_rename_files`) also take **`new_names`** parallel to the ids being renamed. `resume_bulk()` only forwards the id/file vector—you must pass **`new_names`** (subset to match the incomplete rows, in the same order) yourself.

**Creates-from-names** helpers (`bulk_create_sessions`, `bulk_create_folders`, `bulk_create_records`) require **unique** names so each row’s `input` stays identifiable when resuming.

See `?resume_bulk` and the individual `bulk_*` help pages for details.

## Testing

The test suite includes integration tests that run against the Databrary API.
These tests require the following environment variables to be set:

| Variable | Description |
|---|---|
| `DATABRARY_LOGIN` | Email address for the account |
| `DATABRARY_PASSWORD` | Password for the account |
| `DATABRARY_CLIENT_ID` | OAuth client ID |
| `DATABRARY_CLIENT_SECRET` | OAuth client secret |
| `DATABRARY_BASE_URL` | *(optional)* API base URL; defaults to `https://api.databrary.org`. Integration tests use NYU staging (`https://api.stg-databrary.its.nyu.edu`) when this variable is **unset**. |

Tests that require authentication are automatically skipped when these variables are not available.

The recommended way to provide them is a project-level `.Renviron` file in the package root:

```bash
DATABRARY_LOGIN=you@example.com
DATABRARY_PASSWORD=your-password
DATABRARY_CLIENT_ID=your-client-id
DATABRARY_CLIENT_SECRET=your-client-secret
```

R loads this file automatically on startup, so `devtools::test()` and `devtools::check()` will pick up the credentials without any extra steps.

## Lifecycle

Rick Gilmore has been using experimental versions of databraryr for many years, but the package was only released to CRAN in the fall of 2023.
Some new features are on the roadmap, but the package is largely stable.
