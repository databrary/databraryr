# Session + record integration roundtrip (idempotent via helper-fixtures.R).
login_test_account()

test_that("full session and record lifecycle roundtrip", {
  sid <- make_test_session("session_record lifecycle session")
  skip_if_null_response(sid, "create_session for lifecycle")

  patched <- patch_session(
    vol_id = TEST_VOL_ID,
    session_id = sid,
    name = "session_record lifecycle session renamed",
    vb = FALSE
  )
  skip_if_null_response(patched, "patch_session in lifecycle")
  expect_equal(patched$name, "session_record lifecycle session renamed")

  rid <- make_test_record("session_record lifecycle record")
  skip_if_null_response(rid, "create_volume_record for lifecycle")

  # Optional metric TEST_METRIC_ID_EXTRA on TEST_CATEGORY_ID / TEST_VOL_ID (see create_volume_record tests).
  measure_result <- set_record_measure(
    vol_id = TEST_VOL_ID,
    record_id = rid,
    metric_id = TEST_METRIC_ID_EXTRA,
    value = "Lifecycle measure value",
    vb = FALSE
  )
  skip_if_null_response(measure_result, "set_record_measure in lifecycle")

  expect_true(
    add_default_record_to_session(
      vol_id = TEST_VOL_ID,
      session_id = sid,
      record_id = rid,
      vb = FALSE
    )
  )

  expect_true(
    remove_default_record_from_session(
      vol_id = TEST_VOL_ID,
      session_id = sid,
      record_id = rid,
      vb = FALSE
    )
  )

  updated <- update_volume_record(
    vol_id = TEST_VOL_ID,
    record_id = rid,
    measures = list("29" = "Lifecycle updated"),
    vb = FALSE
  )
  skip_if_null_response(updated, "update_volume_record in lifecycle")
  expect_equal(updated$record_id, rid)
})
