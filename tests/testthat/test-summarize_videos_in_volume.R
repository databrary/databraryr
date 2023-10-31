# summarize_videos_in_volume ---------------------------------------------------
test_that("summarize_videos_in_volume returns data.frame", {
  expect_true(class(summarize_videos_in_volume()) == "data.frame")
})

test_that("summarize_videos_in_volume rejects bad input parameters", {
  expect_error(summarize_videos_in_volume(vol_id = -1))
  expect_error(summarize_videos_in_volume(vol_id = 0))
  expect_error(summarize_videos_in_volume(vol_id = "a"))
  expect_error(summarize_videos_in_volume(vol_id = TRUE))

  expect_error(summarize_videos_in_volume(vb = -1))
  expect_error(summarize_videos_in_volume(vb = 3))
  expect_error(summarize_videos_in_volume(vb = "a"))
  expect_error(summarize_videos_in_volume(vb = list(a=1, b=2)))
})
