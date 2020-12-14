dg2dec <- function(var, Dg = NA, Min = NA, Sec = NA, SW.hemisphere = "S|W") {
  DMS <- sapply(strsplit(var, paste0("[", Dg, Min, Sec, "]")), as.numeric)
  decdg <- abs(DMS[1,]) + DMS[2,]/60 + ifelse(dim(DMS)[1] > 2 & is.na(Sec), DMS[3,]/3600, 0)
  SW <- grepl(pattern = SW.hemisphere, x = var, ignore.case = T)
  return(ifelse(SW, -1, 1) * decdg)
}
