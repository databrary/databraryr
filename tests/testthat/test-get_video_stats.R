# get_video_stats ---------------------------------------------------------
# test_that("get_video_stats returns a data.frame", {
#   expect_true("data.frame" %in% class(get_video_stats()))
# })
# 
test_that("get_video_stats rejects bad input parameters", {
  expect_error(get_video_stats(vol_id = "a"))
  expect_error(get_video_stats(vol_id = -1))
  expect_error(get_video_stats(vol_id = c(1,3)))
  
  expect_error(get_video_stats(vb = "a"))
  expect_error(get_video_stats(vb = -1))
  expect_error(get_video_stats(vb = c(2,3)))
})
