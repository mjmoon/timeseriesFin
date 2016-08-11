### final rankings from 2010 to 2014 ###
filenames <- list.files("./data/", pattern = "rank*")
curwd <- getwd()
setwd("./data/")
franks <- lapply(filenames, read.csv) 
setwd(curwd)

getx0 <- function(rankscsv){
  rankscsv <- rankscsv[-31,1:2]
  rankscsv$x0 <- 14.5:-14.5
  rankscsv$Tm <- as.character(rankscsv$Tm)
  # rankscsv$Tm[!rankscsv$Tm %in% teamIds]
  rankscsv$Tm[rankscsv$Tm == "NYY"] <- "NYA"
  rankscsv$Tm[rankscsv$Tm == "LAA"] <- "ANA"
  rankscsv$Tm[rankscsv$Tm == "LAD"] <- "LAN"
  rankscsv$Tm[rankscsv$Tm == "STL"] <- "SLN"
  rankscsv$Tm[rankscsv$Tm == "SFG"] <- "SFN"
  rankscsv$Tm[rankscsv$Tm == "FLA"] <- "MIA"
  rankscsv$Tm[rankscsv$Tm == "TBR"] <- "TBA"
  rankscsv$Tm[rankscsv$Tm == "CHC"] <- "CHN"
  rankscsv$Tm[rankscsv$Tm == "CHW"] <- "CHA"
  rankscsv$Tm[rankscsv$Tm == "SDP"] <- "SDN"
  rankscsv$Tm[rankscsv$Tm == "NYM"] <- "NYN"
  rankscsv$Tm[rankscsv$Tm == "KCR"] <- "KCA"
  rankscsv$Tm[rankscsv$Tm == "WSN"] <- "WAS"
  rankscsv$Tm <- factor(rankscsv$Tm, teamIds)
  # rankscsv$Tm[!rankscsv$Tm %in% teamIds]
  rankscsv <- rankscsv[order(rankscsv$Tm), ]
  return(rankscsv$x0)
}

x0s <- sapply(franks, getx0)
