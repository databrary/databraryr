# download_session_assets_fr_df ---------------------------------------------------------
test_that("download_session_assets_fr_df rejects bad input parameters",
          {
            expect_error(download_session_assets_fr_df(asset_id = -1))
            expect_error(download_session_assets_fr_df(asset_id = 0))
            expect_error(download_session_assets_fr_df(asset_id = "a"))
            expect_error(download_session_assets_fr_df(asset_id = list(a = 1, b =
                                                                        2)))
            expect_error(download_session_assets_fr_df(asset_id = TRUE))
            
            expect_error(download_session_assets_fr_df(file_name = 3))
            expect_error(download_session_assets_fr_df(file_name = list(a = 1, b =
                                                                         2)))
            expect_error(download_session_assets_fr_df(file_name = TRUE))
            
            expect_error(download_session_assets_fr_df(session_id = -1))
            expect_error(download_session_assets_fr_df(session_id = 0))
            expect_error(download_session_assets_fr_df(session_id = "a"))
            expect_error(download_session_assets_fr_df(session_id = list(a = 1, b =
                                                                          2)))
            expect_error(download_session_assets_fr_df(session_id = TRUE))
            
            expect_error(download_session_assets_fr_df(target_dir = 3))
            expect_error(download_session_assets_fr_df(target_dir = list(a = 1, b =
                                                                          2)))
            expect_error(download_session_assets_fr_df(target_dir = TRUE))
            
            expect_error(download_session_assets_fr_df(overwrite = -1))
            expect_error(download_session_assets_fr_df(overwrite = 3))
            expect_error(download_session_assets_fr_df(overwrite = "a"))
            expect_error(download_session_assets_fr_df(overwrite = list(a = 1, b = 2)))
            
            expect_error(download_session_assets_fr_df(vb = -1))
            expect_error(download_session_assets_fr_df(vb = 3))
            expect_error(download_session_assets_fr_df(vb = "a"))
            expect_error(download_session_assets_fr_df(vb = list(a = 1, b = 2)))
            
            expect_error(download_session_assets_fr_df(rq = "a"))
            expect_error(download_session_assets_fr_df(rq = -1))
            expect_error(download_session_assets_fr_df(rq = c(2, 3)))
            expect_error(download_session_assets_fr_df(rq = list(a = 1, b = 2)))
          })
