get_release_levels <- function(vb = FALSE) {
  c <- assign_constants(vb = vb)
  rl <- c$release
  return(rl)
}
