library(databraryr)

# summarize_demo_part_w_video ---------------------------------------------------
test_that("summarize_demo_part_w_video returns data.frame", {
  expect_true(class(summarize_demo_part_w_video()) == "data.frame")
})

test_that("summarize_demo_part_w_video rejects bad input parameters", {
  expect_error(summarize_demo_part_w_video(vol_id = -1))
  expect_error(summarize_demo_part_w_video(vol_id = 0))
  expect_error(summarize_demo_part_w_video(vol_id = "a"))
  expect_error(summarize_demo_part_w_video(vol_id = list(a=1, b=2)))
  expect_error(summarize_demo_part_w_video(vol_id = TRUE))

  expect_error(summarize_demo_part_w_video(vb = -1))
  expect_error(summarize_demo_part_w_video(vb = 3))
  expect_error(summarize_demo_part_w_video(vb = "a"))
  expect_error(summarize_demo_part_w_video(vb = list(a=1, b=2)))
})

# summarize_videos_in_volume ---------------------------------------------------
test_that("summarize_videos_in_volume returns data.frame", {
  expect_true(class(summarize_videos_in_volume()) == "data.frame")
})

test_that("summarize_videos_in_volume rejects bad input parameters", {
  expect_error(summarize_videos_in_volume(vol_id = -1))
  expect_error(summarize_videos_in_volume(vol_id = 0))
  expect_error(summarize_videos_in_volume(vol_id = "a"))
  expect_error(summarize_videos_in_volume(vol_id = list(a=1, b=2)))
  expect_error(summarize_videos_in_volume(vol_id = TRUE))

  expect_error(summarize_videos_in_volume(vb = -1))
  expect_error(summarize_videos_in_volume(vb = 3))
  expect_error(summarize_videos_in_volume(vb = "a"))
  expect_error(summarize_videos_in_volume(vb = list(a=1, b=2)))
})
