# Changelog

## databraryr 1.0.0

### Major changes

- Adapted all package functions to Databrary 2.0 API.

## databraryr 0.6.6

CRAN release: 2024-07-15

### Major changes

- Incorporated package-wide option management using the {options}
  package.
- Added obligatory message when access to an internet resource fails.
  This responds to the 2024-07-04 archiving of the previous version due
  to a violation of CRAN’s policy on packages that access internet
  resources.

### Minor changes

- Removed superceded and deprecated functions: list_affiliate(),
  GET_db_contents(), download_party(), get_party_as_df(),
  list_individual_sponsors(), list_institutional_sponsors(),
  list_party(), list_people(), list_sessions(), list_volume(),
  list_sessions_in_volume()
- list_volume_sessions() returns text label for release level.

## databraryr 0.6.5

CRAN release: 2024-07-01

### Minor changes

- Removed many superceded and deprecated functions.
- Improved error handling in many functions.

## databraryr 0.6.4

CRAN release: 2024-04-18

### Minor changes

- Added `timeout_secs` parameter in
  [`download_session_assets_fr_df()`](https://databrary.github.io/databraryr/reference/download_session_assets_fr_df.md)
  to accommodate downloads of very big files.

## databraryr 0.6.3

CRAN release: 2024-03-26

### Major changes

- New
  [`download_session_assets_fr_df()`](https://databrary.github.io/databraryr/reference/download_session_assets_fr_df.md)
  function that supports piping syntax.
- New `list_session_assets_2()` function that provides input to the
  “pipe-able”
  [`download_session_assets_fr_df()`](https://databrary.github.io/databraryr/reference/download_session_assets_fr_df.md).
- Fixed bug in
  [`get_db_stats()`](https://databrary.github.io/databraryr/reference/get_db_stats.md)
  that did not return valid data except for summary statistics.

### Minor changes

- More condensed messages when the user supplies a NULL httr2 request.

## databraryr 0.6.2

CRAN release: 2024-03-15

## databraryr 0.6.1

CRAN release: 2024-02-22

### Major changes

- In testing 0.6.0, several users with installed versions of R before
  4.0.1 could not use the package because of its dependency on the
  native (`|>`) pipe operator. This version reverts to using the
  `magrittr` pipe (`%>%`).

## databraryr 0.6.0

CRAN release: 2024-02-09

#### Major changes

- Implement `httr2` functions throughout.
  - New
    [`make_default_request()`](https://databrary.github.io/databraryr/reference/make_default_request.md)
    and
    [`make_login_client()`](https://databrary.github.io/databraryr/reference/make_login_client.md)
    functions to pave the way for future data uploads.
  - Revised
    [`login_db()`](https://databrary.github.io/databraryr/reference/login_db.md)
    and
    [`logout_db()`](https://databrary.github.io/databraryr/reference/logout_db.md)
    to use `httr2`-style request objects.
- Reduce redundant calls to Databrary API and better filtering of
  responses.
- Introduce new function naming convention:
  - `list_*` functions return a list or a data frame.
    - `list_{volume,session,party}*` functions target volumes, sessions,
      or parties.
  - `download_*` functions download a file.

## databraryr 0.5.1

CRAN release: 2023-11-22

### Major changes

- Update `summarize_videos_in_volume()` to support multiple volumes
- Update `list_affiliates()` to support multiple party IDs.
- Update
  [`list_volume_funding()`](https://databrary.github.io/databraryr/reference/list_volume_funding.md)
  to support multiple volumes.
- Update `download_party_avatar()` to support multiple party IDs and to
  return a list with party info and the avatar image.
- Update `get_party_as_df()` to support multiple party IDs.
- Update `GET_db_contents()`; it now handles images.
- New
  [`download_session_zip()`](https://databrary.github.io/databraryr/reference/download_session_zip.md)
  and
  [`download_volume_zip()`](https://databrary.github.io/databraryr/reference/download_volume_zip.md)
  functions.
- New `summarize_videos_in_volume()` function to support multiple volume
  video statistics.
- Update `list_sessions()` to support multiple volumes.

## databraryr 0.5.0

CRAN release: 2023-10-24

### Major changes

- First release approved by CRAN:
  <https://cran.r-project.org/package=databraryr>

## databraryr 0.4.4

## databraryr 0.4.3

## databraryr 0.4.2

### Major changes

- First release submitted to CRAN.

### Minor improvements and bug fixes

- There is a long list of these from the latest 0.2.9 release. We do not
  list them here.
