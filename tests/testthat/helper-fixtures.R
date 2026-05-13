# Shared sandbox IDs for live API integration tests (staging volume 1777).
TEST_VOL_ID <- 1777L
TEST_CATEGORY_ID <- 6L

# create_session, then schedule delete_session on `envir` via withr::defer.
# Default `envir = parent.frame()`: call from test_that() so cleanup runs when
# the test ends. Wrappers must pass `envir = parent.frame()` through.
make_test_session <- function(
    name,
    vol_id = TEST_VOL_ID,
    release_level = NULL,
    source_date = NULL,
    date = NULL,
    date_precision = NULL,
    default_records = NULL,
    vb = FALSE,
    rq = NULL,
    envir = parent.frame()) {
  created <- create_session(
    vol_id = vol_id,
    name = name,
    release_level = release_level,
    source_date = source_date,
    date = date,
    date_precision = date_precision,
    default_records = default_records,
    vb = vb,
    rq = rq
  )

  if (is.null(created) || is.null(created$id)) {
    return(NULL)
  }

  sid <- created$id
  withr::defer(
    delete_session(vol_id = vol_id, session_id = sid, vb = FALSE),
    envir = envir
  )
  sid
}

# See make_test_session() for `envir` usage.
make_test_record <- function(
    name,
    category_id = TEST_CATEGORY_ID,
    vol_id = TEST_VOL_ID,
    measures = list(),
    participant = NULL,
    vb = FALSE,
    rq = NULL,
    envir = parent.frame()) {
  created <- create_volume_record(
    vol_id = vol_id,
    category_id = category_id,
    name = name,
    measures = measures,
    participant = participant,
    vb = vb,
    rq = rq
  )

  if (is.null(created) || is.null(created$record_id)) {
    return(NULL)
  }

  rid <- created$record_id
  withr::defer(
    delete_volume_record(vol_id = vol_id, record_id = rid, vb = FALSE),
    envir = envir
  )
  rid
}
