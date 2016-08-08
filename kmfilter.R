library(astsa)
source("getdata.R")

seasoni <- 1
gamedays <- unique(scoreSeasons[[seasoni]]$Tind)
yt <- sapply(gamedays, getY18, seasoni)
Ct <- sapply(gamedays, getC18, seasoni, simplify = "array")
Dt <- sapply(gamedays, getD18, seasoni, simplify = "array")
