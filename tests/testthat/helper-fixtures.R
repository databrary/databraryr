# Shared sandbox IDs for live API integration tests
TEST_VOL_ID <- 1777L
TEST_CATEGORY_ID <- 6L
TEST_MISSING_ID <- 999999999L
TEST_METRIC_ID <- 29L
TEST_METRIC_ID_EXTRA <- 30L

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
    vb = FALSE,
    rq = NULL,
    envir = parent.frame()) {
  created <- create_volume_record(
    vol_id = vol_id,
    category_id = category_id,
    name = name,
    measures = measures,
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

# create_folder, then schedule delete_folder on `envir` via withr::defer.
# See make_test_session() for `envir` usage.
make_test_folder <- function(
    name,
    vol_id = TEST_VOL_ID,
    release_level = NULL,
    source_date = NULL,
    vb = FALSE,
    rq = NULL,
    envir = parent.frame()) {
  created <- create_folder(
    vol_id = vol_id,
    name = name,
    release_level = release_level,
    source_date = source_date,
    vb = vb,
    rq = rq
  )

  if (is.null(created) || is.null(created$id)) {
    return(NULL)
  }

  fid <- as.integer(created$id)
  withr::defer(
    delete_folder(vol_id = vol_id, folder_id = fid, vb = FALSE),
    envir = envir
  )
  fid
}

# Upload a small text file into a session and wait until it appears in listing.
# Use file_basename with a text extension (e.g. .txt); staging may reject
# application/octet-stream. Session defer deletes the session and assets.
upload_test_session_asset <- function(
    session_id,
    vol_id = TEST_VOL_ID,
    file_basename = "test_asset.txt",
    vb = FALSE,
    max_wait_s = 120L,
    envir = parent.frame()) {
  tmp <- tempfile(fileext = paste0(".", tools::file_ext(file_basename)))
  writeLines(strrep("x", 2048L), tmp, useBytes = FALSE)
  withr::defer(unlink(tmp, force = TRUE), envir = envir)

  ct <- databraryr:::guess_content_type(file_basename)

  up <- upload_file(
    path = tmp,
    destination_type = "session",
    object_id = session_id,
    content_type = ct,
    filename = file_basename,
    vb = vb
  )
  if (is.null(up)) {
    return(NULL)
  }

  deadline <- Sys.time() + as.numeric(max_wait_s)
  while (Sys.time() < deadline) {
    assets <- list_session_assets(vol_id = vol_id, session_id = session_id, vb = FALSE)
    if (!is.null(assets) && nrow(assets) > 0L) {
      m <- which(assets$asset_name == file_basename)
      if (length(m)) {
        return(as.integer(assets$asset_id[m[[1L]]]))
      }
      return(as.integer(assets$asset_id[nrow(assets)]))
    }
    Sys.sleep(1)
  }
  NULL
}

# Folder variant of upload_test_session_asset(); folder teardown via make_test_folder defer.
upload_test_folder_asset <- function(
    folder_id,
    vol_id = TEST_VOL_ID,
    file_basename = "folder_test_asset.txt",
    vb = FALSE,
    max_wait_s = 120L,
    envir = parent.frame()) {
  tmp <- tempfile(fileext = paste0(".", tools::file_ext(file_basename)))
  writeLines(strrep("x", 2048L), tmp, useBytes = FALSE)
  withr::defer(unlink(tmp, force = TRUE), envir = envir)

  ct <- databraryr:::guess_content_type(file_basename)

  up <- upload_file(
    path = tmp,
    destination_type = "folder",
    object_id = folder_id,
    content_type = ct,
    filename = file_basename,
    vb = vb
  )
  if (is.null(up)) {
    return(NULL)
  }

  deadline <- Sys.time() + as.numeric(max_wait_s)
  while (Sys.time() < deadline) {
    assets <- list_folder_assets(vol_id = vol_id, folder_id = folder_id, vb = FALSE)
    if (!is.null(assets) && nrow(assets) > 0L) {
      m <- which(assets$asset_name == file_basename)
      if (length(m)) {
        return(as.integer(assets$asset_id[m[[1L]]]))
      }
      return(as.integer(assets$asset_id[nrow(assets)]))
    }
    Sys.sleep(1)
  }
  NULL
}
