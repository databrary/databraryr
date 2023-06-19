library(databraryr)

# list_assets_by_type ---------------------------------------------------
test_that("list_assets_by_type returns data.frame", {
  expect_true(class(list_assets_by_type()) == "data.frame")
})

test_that("list_assets_by_type rejects bad input parameters", {
  expect_error(list_assets_by_type(vol_id = -1))
  expect_error(list_assets_by_type(vol_id = 0))
  expect_error(list_assets_by_type(vol_id = "a"))
  expect_error(list_assets_by_type(vol_id = list(a=1, b=2)))
  expect_error(list_assets_by_type(vol_id = TRUE))

  expect_error(list_assets_by_type(type = "a"))
  expect_error(list_assets_by_type(type = c(1,2)))
  expect_error(list_assets_by_type(type = TRUE))
  expect_error(list_assets_by_type(type = list(a=1, b=2)))

  expect_error(list_assets_by_type(vb = -1))
  expect_error(list_assets_by_type(vb = 3))
  expect_error(list_assets_by_type(vb = "a"))
  expect_error(list_assets_by_type(vb = list(a=1, b=2)))
})

# list_assets_in_session -----------------------------------------------
test_that("list_assets_by_type returns data.frame", {
  expect_true(class(list_assets_in_session()) == "data.frame")
})

test_that("list_assets_in_session rejects bad input parameters", {
  expect_error(list_assets_in_session(vol_id = -1))
  expect_error(list_assets_in_session(vol_id = 0))
  expect_error(list_assets_in_session(vol_id = "a"))
  expect_error(list_assets_in_session(vol_id = list(a=1, b=2)))
  expect_error(list_assets_in_session(vol_id = TRUE))

  expect_error(list_assets_in_session(session_id = -1))
  expect_error(list_assets_in_session(session_id = 0))
  expect_error(list_assets_in_session(session_id = "a"))
  expect_error(list_assets_in_session(session_id = list(a=1, b=2)))
  expect_error(list_assets_in_session(session_id = TRUE))

  expect_error(list_assets_in_session(vb = -1))
  expect_error(list_assets_in_session(vb = 3))
  expect_error(list_assets_in_session(vb = "a"))
  expect_error(list_assets_in_session(vb = list(a=-1, b=2)))
})

# list_assets_in_volume -----------------------------------------------
test_that("list_assets_in_volume returns data.frame", {
  expect_true(class(list_assets_in_volume()) == "data.frame")
})

test_that("list_assets_in_volume rejects bad input parameters", {
  expect_error(list_assets_in_volume(vol_id = -1))
  expect_error(list_assets_in_volume(vol_id = 0))
  expect_error(list_assets_in_volume(vol_id = "a"))
  expect_error(list_assets_in_volume(vol_id = list(a=1, b=2)))
  expect_error(list_assets_in_volume(vol_id = TRUE))

  expect_error(list_assets_in_volume(vb = -1))
  expect_error(list_assets_in_volume(vb = 3))
  expect_error(list_assets_in_volume(vb = "a"))
  expect_error(list_assets_in_volume(vb = list(a=1, b=2)))
})

# list_assets_json -----------------------------------------------------
test_that("list_assets_json returns character", {
  expect_true(class(list_assets_json()) == "character")
})

test_that("list_assets_json returns valid JSON", {
  expect_true(jsonlite::validate(list_assets_json()))
})

test_that("list_assets_json rejects bad input parameters", {
  expect_error(list_assets_json(session_id = "a"))
  expect_error(list_assets_json(session_id = c(1,2)))
  expect_error(list_assets_json(session_id = TRUE))
  expect_error(list_assets_json(session_id = list(a=1, b=2)))

  expect_error(list_assets_json(vol_id = -1))
  expect_error(list_assets_json(vol_id = 0))
  expect_error(list_assets_json(vol_id = "a"))
  expect_error(list_assets_json(vol_id = list(a=1, b=2)))
  expect_error(list_assets_json(vol_id = TRUE))

  expect_error(list_assets_json(vb = -1))
  expect_error(list_assets_json(vb = 3))
  expect_error(list_assets_json(vb = "a"))
  expect_error(list_assets_json(vb = list(a=1, b=2)))
})

# list_containers_records_json -----------------------------------------------
test_that("list_containers_records_json returns character", {
  expect_true(class(list_containers_records_json()) == "character")
})

test_that("list_containers_records_json is JSON", {
  expect_true(jsonlite::validate(list_containers_records_json()))
})

test_that("list_containers_records_json rejects bad input parameters", {
  expect_error(list_containers_records_json(vol_id = "a"))
  expect_error(list_containers_records_json(vol_id = c(1,2)))
  expect_error(list_containers_records_json(vol_id = TRUE))
  expect_error(list_containers_records_json(vol_id = list(a=1, b=2)))
  expect_error(list_containers_records_json(vol_id = -1))
  expect_error(list_containers_records_json(vol_id = 0))

  expect_error(list_containers_records_json(vb = -1))
  expect_error(list_containers_records_json(vb = 3))
  expect_error(list_containers_records_json(vb = "a"))
  #  expect_error(list_containers_records_json(vb = list(a=1, b=2)))
})

# list_people ----------------------------------------------------------------
test_that("list_people returns data.frame", {
  expect_true("data.frame" %in% class(list_people()))
})

test_that("list_people rejects bad input parameters", {
  expect_error(list_people(people_list = "a"))
  expect_error(list_people(people_list = TRUE))
  expect_error(list_people(people_list = 0))
  expect_error(list_people(people_list = -1))

  expect_error(list_people(vb = -1))
  expect_error(list_people(vb = 3))
  expect_error(list_people(vb = "a"))
  expect_error(list_people(vb = list(a=1, b=2)))
})

# list_sessions --------------------------------------------------------------
test_that("list_sessions returns data.frame", {
  expect_true(class(list_assets_in_session()) == "data.frame")
})

test_that("list_sessions rejects bad input parameters", {
  expect_error(list_sessions(vol_id = "a"))
  expect_error(list_sessions(vol_id = TRUE))
  expect_error(list_sessions(vol_id = list(a=1, b=2)))
  expect_error(list_sessions(vol_id = -1))

  expect_error(list_sessions(vb = -1))
  expect_error(list_sessions(vb = 3))
  expect_error(list_sessions(vb = "a"))
})

# list_sessions_in_volume --------------------------------------------------------------
test_that("list_sessions_in_volume returns data.frame", {
  expect_true(class(list_sessions_in_volume()) == "data.frame")
})

test_that("list_sessions_in_volume rejects bad input parameters", {
  expect_error(list_sessions_in_volume(vol_id = "a"))
  expect_error(list_sessions_in_volume(vol_id = TRUE))
  expect_error(list_sessions_in_volume(vol_id = list(a=1, b=2)))
  expect_error(list_sessions_in_volume(vol_id = -1))

  expect_error(list_sessions_in_volume(vb = -1))
  expect_error(list_sessions_in_volume(vb = 3))
  expect_error(list_sessions_in_volume(vb = "a"))
})

# list_volume_metadata --------------------------------------------------------------
test_that("list_volume_metadata returns data.frame", {
  expect_true(class(list_volume_metadata()) == "data.frame")
})

test_that("list_volume_metadata rejects bad input parameters", {
  expect_error(list_volume_metadata(vol_id = "a"))
  expect_error(list_volume_metadata(vol_id = TRUE))
  expect_error(list_volume_metadata(vol_id = list(a=1, b=2)))
  expect_error(list_volume_metadata(vol_id = -1))

  expect_error(list_volume_metadata(write_header = -1))
  expect_error(list_volume_metadata(write_header = 3))
  expect_error(list_volume_metadata(write_header = "a"))
  expect_error(list_volume_metadata(write_header = list(a=1, b=2)))

  expect_error(list_volume_metadata(data_frame = -1))
  expect_error(list_volume_metadata(data_frame = 3))
  expect_error(list_volume_metadata(data_frame = "a"))
  expect_error(list_volume_metadata(data_frame = list(a=1, b=2)))

  expect_error(list_volume_metadata(vb = -1))
  expect_error(list_volume_metadata(vb = 3))
  expect_error(list_volume_metadata(vb = "a"))
  expect_error(list_volume_metadata(vb = list(a=1, b=2)))
})

# list_volume_owners ---------------------------------------------------------
test_that("list_volume_owners returns data.frame", {
  expect_true(class(list_volume_owners()) == "data.frame")
})

test_that("list_volume_owners rejects bad input parameters", {
  expect_error(list_volume_owners(vol_id = "a"))
  expect_error(list_volume_owners(vol_id = c(1,2)))
  expect_error(list_volume_owners(vol_id = TRUE))
  expect_error(list_volume_owners(vol_id = list(a=1, b=2)))
  expect_error(list_volume_owners(vol_id = -1))

  expect_error(list_volume_owners(vb = -1))
  expect_error(list_volume_owners(vb = 3))
  expect_error(list_volume_owners(vb = "a"))
  expect_error(list_volume_owners(vb = list(a=1, b=2)))
})

# list_session_activity ---------------------------------------------------------
test_that("list_session_activity returns data.frame or is NULL", {
  expect_true((is.null(list_session_activity()) ||
                        (class(list_session_activity()) == "data.frame")))
})

test_that("list_session_activity rejects bad input parameters", {
  expect_error(list_session_activity(session_id = "a"))
  expect_error(list_session_activity(session_id = c(1,2)))
  expect_error(list_session_activity(session_id = TRUE))
  expect_error(list_session_activity(session_id = list(a=1, b=2)))
  expect_error(list_session_activity(session_id = -1))

  expect_error(list_session_activity(vb = -1))
  expect_error(list_session_activity(vb = 3))
  expect_error(list_session_activity(vb = "a"))
  expect_error(list_session_activity(vb = list(a=1, b=2)))
})

# list_volume_activity ---------------------------------------------------------
test_that("list_volume_activity returns data.frame or is NULL", {
  expect_true((is.null(list_volume_activity()) ||
                (class(list_volume_activity()) == "data.frame")))
})

test_that("list_volume_activity rejects bad input parameters", {
  expect_error(list_volume_activity(vol_id = "a"))
  expect_error(list_volume_activity(vol_id = c(1,2)))
  expect_error(list_volume_activity(vol_id = TRUE))
  expect_error(list_volume_activity(vol_id = list(a=1, b=2)))
  expect_error(list_volume_activity(vol_id = -1))

  expect_error(list_volume_activity(vb = -1))
  expect_error(list_volume_activity(vb = 3))
  expect_error(list_volume_activity(vb = "a"))
  expect_error(list_volume_activity(vb = list(a=1, b=2)))
})

# list_volume_links ---------------------------------------------------------
test_that("list_volume_links returns data.frame or is NULL", {
  expect_true((is.null(list_volume_links()) ||
                 (class(list_volume_links()) == "data.frame")))
})

test_that("list_volume_links rejects bad input parameters", {
  expect_error(list_volume_links(vol_id = "a"))
  expect_error(list_volume_links(vol_id = c(1,2)))
  expect_error(list_volume_links(vol_id = TRUE))
  expect_error(list_volume_links(vol_id = list(a=1, b=2)))
  expect_error(list_volume_links(vol_id = -1))

  expect_error(list_volume_links(vb = -1))
  expect_error(list_volume_links(vb = 3))
  expect_error(list_volume_links(vb = "a"))
  expect_error(list_volume_links(vb = list(a=1, b=2)))
})

# list_volume_funding ---------------------------------------------------------
test_that("list_volume_funding returns data.frame or is NULL", {
  expect_true((is.null(list_volume_funding()) ||
                 ("data.frame" %in% class(list_volume_funding()))))
})

test_that("list_volume_funding rejects bad input parameters", {
  expect_error(list_volume_funding(vol_id = "a"))
  expect_error(list_volume_funding(vol_id = c(1,2)))
  expect_error(list_volume_funding(vol_id = TRUE))
  expect_error(list_volume_funding(vol_id = list(a=1, b=2)))
  expect_error(list_volume_funding(vol_id = -1))

  expect_error(list_volume_funding(vb = -1))
  expect_error(list_volume_funding(vb = 3))
  expect_error(list_volume_funding(vb = "a"))
  expect_error(list_volume_funding(vb = list(a=1, b=2)))
})

# list_volume_tags ---------------------------------------------------------
test_that("list_volume_tags returns data.frame or is NULL", {
  expect_true((is.null(list_volume_tags()) ||
                 (class(list_volume_tags()) == "data.frame")))
})

test_that("list_volume_tags rejects bad input parameters", {
  expect_error(list_volume_tags(vol_id = "a"))
  expect_error(list_volume_tags(vol_id = c(1,2)))
  expect_error(list_volume_tags(vol_id = TRUE))
  expect_error(list_volume_tags(vol_id = list(a=1, b=2)))
  expect_error(list_volume_tags(vol_id = -1))

  expect_error(list_volume_tags(vb = -1))
  expect_error(list_volume_tags(vb = 3))
  expect_error(list_volume_tags(vb = "a"))
  expect_error(list_volume_tags(vb = list(a=1, b=2)))
})

test_that("list_volume_tags returns data.frame for volume with tags", {
  expect_true(class(list_volume_tags(vol_id = 1)) == "data.frame")
})

test_that("list_volume_tags returns NULL for volume without tags", {
  expect_true(is.null(list_volume_tags(vol_id = 3)))
})

# list_volume_excerpts ---------------------------------------------------------
test_that("list_volume_excerpts returns data.frame or is NULL", {
  expect_true((is.null(list_volume_excerpts()) ||
                 (class(list_volume_excerpts()) == "data.frame")))
})

test_that("list_volume_excerpts rejects bad input parameters", {
  expect_error(list_volume_excerpts(vol_id = "a"))
  expect_error(list_volume_excerpts(vol_id = c(1,2)))
  expect_error(list_volume_excerpts(vol_id = TRUE))
  expect_error(list_volume_excerpts(vol_id = list(a=1, b=2)))
  expect_error(list_volume_excerpts(vol_id = -1))

  expect_error(list_volume_excerpts(vb = -1))
  expect_error(list_volume_excerpts(vb = 3))
  expect_error(list_volume_excerpts(vb = "a"))
  expect_error(list_volume_excerpts(vb = list(a=1, b=2)))
})

# list_affiliates ---------------------------------------------------------
test_that("list_affiliates returns a data.frame or is NULL.", {
  expect_true((is.null(list_affiliates()) ||
                 (class(list_affiliates()) == "data.frame")))
})

test_that("list_affiliates rejects bad input parameters", {
  expect_error(list_affiliates(party_id = "a"))
  expect_error(list_affiliates(party_id = -1))
  expect_error(list_affiliates(party_id = TRUE))
  expect_error(list_affiliates(party_id = c(1,3)))
  expect_error(list_affiliates(party_id = list(a=1, b=2)))
  
  expect_error(list_affiliates(vb = "a"))
  expect_error(list_affiliates(vb = -1))
  expect_error(list_affiliates(vb = c(2,3)))
  expect_error(list_affiliates(vb = list(a=1, b=2)))
})

# list_sponsors ---------------------------------------------------------
test_that("list_sponsors returns a data.frame or is NULL.", {
  expect_true((is.null(list_sponsors()) ||
                 (class(list_sponsors()) == "data.frame")))
})

test_that("list_sponsors rejects bad input parameters", {
  expect_error(list_sponsors(party_id = "a"))
  expect_error(list_sponsors(party_id = -1))
  expect_error(list_sponsors(party_id = TRUE))
  expect_error(list_sponsors(party_id = c(1,3)))
  expect_error(list_sponsors(party_id = list(a=1, b=2)))
  
  expect_error(list_sponsors(vb = "a"))
  expect_error(list_sponsors(vb = -1))
  expect_error(list_sponsors(vb = c(2,3)))
  expect_error(list_sponsors(vb = list(a=1, b=2)))
})

# list_volume ---------------------------------------------------------
test_that("list_volume returns a list or is NULL.", {
  expect_true((is.null(list_volume()) ||
                 (class(list_volume()) == "list")))
})

test_that("list_volume rejects bad input parameters", {
  expect_error(list_volume(vol_id = "a"))
  expect_error(list_volume(vol_id = -1))
  expect_error(list_volume(vol_id = TRUE))
  expect_error(list_volume(vol_id = c(1,3)))
  expect_error(list_volume(vol_id = list(a=1, b=2)))
  
  expect_error(list_volume(vb = "a"))
  expect_error(list_volume(vb = -1))
  expect_error(list_volume(vb = c(2,3)))
  expect_error(list_volume(vb = list(a=1, b=2)))
})

# list_party ---------------------------------------------------------
test_that("list_party returns a list or is NULL.", {
  expect_true((is.null(list_party()) ||
                 (class(list_party()) == "list")))
})

test_that("list_party rejects bad input parameters", {
  expect_error(list_party(party_id = "a"))
  expect_error(list_party(party_id = -1))
  expect_error(list_party(party_id = TRUE))
  expect_error(list_party(party_id = c(1,3)))
  expect_error(list_party(party_id = list(a=1, b=2)))
  
  expect_error(list_party(vb = "a"))
  expect_error(list_party(vb = -1))
  expect_error(list_party(vb = c(2,3)))
  expect_error(list_party(vb = list(a=1, b=2)))
})

# list_institutional_sponsors ---------------------------------------------------------
test_that("list_institutional_sponsors returns a data.frame or is NULL.", {
  expect_true((is.null(list_institutional_sponsors()) ||
                 (class(list_institutional_sponsors()) == "data.frame")))
})

test_that("list_institutional_sponsors rejects bad input parameters", {
  expect_error(list_institutional_sponsors(party_id = "a"))
  expect_error(list_institutional_sponsors(party_id = -1))
  expect_error(list_institutional_sponsors(party_id = TRUE))
  expect_error(list_institutional_sponsors(party_id = c(1,3)))
  expect_error(list_institutional_sponsors(party_id = list(a=1, b=2)))
  
  expect_error(list_institutional_sponsors(vb = "a"))
  expect_error(list_institutional_sponsors(vb = -1))
  expect_error(list_institutional_sponsors(vb = c(2,3)))
  expect_error(list_institutional_sponsors(vb = list(a=1, b=2)))
})

# list_individual_sponsors ---------------------------------------------------------
test_that("list_individual_sponsors returns a data.frame or is NULL.", {
  expect_true((is.null(list_individual_sponsors()) ||
                 (class(list_individual_sponsors()) == "data.frame")))
})

test_that("list_individual_sponsors rejects bad input parameters", {
  expect_error(list_individual_sponsors(party_id = "a"))
  expect_error(list_individual_sponsors(party_id = -1))
  expect_error(list_individual_sponsors(party_id = TRUE))
  expect_error(list_individual_sponsors(party_id = c(1,3)))
  expect_error(list_individual_sponsors(party_id = list(a=1, b=2)))
  
  expect_error(list_individual_sponsors(vb = "a"))
  expect_error(list_individual_sponsors(vb = -1))
  expect_error(list_individual_sponsors(vb = c(2,3)))
  expect_error(list_individual_sponsors(vb = list(a=1, b=2)))
})
