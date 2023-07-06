find_str_in_file <- function(fn, targ_str) {
  stopifnot(is.character(targ_str))
  stopifnot(is.character(fn))
  stopifnot(file.exists(fn))
  
  message("In: '", fn, "'")
  system(paste0("grep ", targ_str, " ", fn))
}