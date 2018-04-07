# Open file
con <- file("../../../tmp/db", "r")
dv <- readLines(con)
close(con)

con.out <- file("../../../tmp/db.csv", "w")
writeLines("code,onset,offset,code.value", con.out)
onset.offset <- "^([0-9]{2}:[0-9]{2}:[0-9]{2}:[0-9]{3},[0-9]{2}:[0-9]{2}:[0-9]{2}:[0-9]{3})"
code.values <- "\\([a-zA-Z ?,.'/0-9]+\\)$"
code.regex <- "^([a-zA-Z]+)"

for (l in 1:length(dv)) {
  if (stringr::str_detect(dv[l], code.regex)) {
    code <- stringr::str_extract(dv[l], code.regex)
    next
  }
  if (stringr::str_detect(dv[l], onset.offset)) {
    times <- stringr::str_extract(dv[l], onset.offset)
    if (stringr::str_detect(dv[l], code.values)) {
      code.values <- paste0('"', stringr::str_extract(dv[l], code.values), '"')
    } else {
      code.values <- "-"
    }
  } else {
    times <- "-,-"
  }
  writeLines(paste(code, times, code.values, sep=","), con = con.out)
}
close(con.out)
