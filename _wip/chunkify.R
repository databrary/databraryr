chunkify <- function(filesize, chunk_size) {
  # filesize can be array
  assertthat::is.number(filesize)
  
  assertthat::assert_that(length(chunk_size) == 1)
  assertthat::is.number(chunk_size)
  
  cbind(chunk_size * (floor(filesize/chunk_size)), filesize %% chunk_size)
}