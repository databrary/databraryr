save_volume_summary_info <- function(vol_id, save_dir = tempdir(), vb = FALSE, rq = NULL) {
  message("\nStarting list_volume_info for vol_id ", vol_id, ".")
  vol_info <- list_volume_info(vol_id, vb = vb, rq = lrq)
  if (is.null(vol_info)) {
    message("No data for vol_id ", vol_id, ".")
  } else {
    out_fn <- file.path(save_dir, paste0("vol-", stringr::str_pad(vol_id, width = 4, pad = "0",
                                                                  ),"-info.csv"))
    readr::write_csv(vol_info, out_fn)
    if (vb) message("Wrote '", out_fn, "'.")
    out_fn
  }
} 