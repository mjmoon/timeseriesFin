library(retrosheet)
library(plyr)
library(reshape2)
yrs <- c(2010:2015)
teamIds <- unique(c(sapply(yrs, getTeamIDs)))
# Florida Marlins changed to Miami Marlins
teamIds <- teamIds[teamIds != "FLO"]
parkIds <- c(getParkIDs())

# Variables: Teams, Scores, Date; DblHdr; DayNight; ParkID; 
fields <- c("HmTm", "VisTm", "HmLine", "VisLine", "Date", "DblHdr", "DayNight", "ParkID")
dataGame <- lapply(yrs, function(x) getPartialGamelog(x, fields)) 
dataGameY <- lapply(dataGame, as.data.frame)

parseSeason <- function(season){
  season$GID <- paste(paste(season$Date, season$ParkID, sep = "-"), season$DblHdr, sep = "")
  season$Date <- as.Date(season$Date, "%Y%m%d")
  season$DblHdr <- as.factor(season$DblHdr)
  # Florida Marlins changed to Miami Marlins
  season$VisTm[season$VisTm == "FLO"] <- "MIA"
  season$VisTm <- factor(season$VisTm, teamIds)
  season$HmTm[season$HmTm == "FLO"] <- "MIA"
  season$HmTm <- factor(season$HmTm, teamIds)
  season$DayNight <- factor(season$DayNight, c("D", "N"))
  season$ParkID <- factor(season$ParkID, parkIds$PARKID)
  season$ScoreDiffHm <- 
    sapply(strsplit(season$HmLine, ""), 
           function(x) sum(as.numeric(x), na.rm = TRUE)) -
    sapply(strsplit(season$VisLine, ""), 
           function(x) sum(as.numeric(x), na.rm = TRUE))
  
  tmpVis <- subset(season, select = c("GID", "Date", "DblHdr", "VisTm", "DayNight",
                                      "ParkID", "ScoreDiffHm"))
  names(tmpVis) <- c("GID", "Date", "DblHdr", "Team", "DayNight",
                     "ParkID", "ScoreDiff")
  tmpVis$ScoreDiff <- -tmpVis$ScoreDiff
  tmpVis$HmVis <- -1
  
  
  tmpHm <- subset(season, select = c("GID", "Date", "DblHdr", "HmTm", "DayNight",
                                      "ParkID", "ScoreDiffHm"))
  names(tmpHm) <- c("GID", "Date", "DblHdr", "Team", "DayNight",
                     "ParkID", "ScoreDiff")
  tmpHm$HmVis <- 1
  
  
  tmp <- rbind(tmpVis, tmpHm)
  return(tmp[order(tmp$Date, tmp$ParkID), ])
}

dataSeasons <- lapply(dataGameY, parseSeason)
names(dataSeasons) <- c("S2010", "S2011", "S2012", "S2013", "S2014", "S2015")

# C data frame: match matrices by date with each row representing a single game 
#               (home team: 1; away team: -1)
parseMatches <- function(season){
  tmp <- ddply(season, "Date", function(x) dcast(x, GID ~ Team, sum, value.var = "HmVis"))
  tmp[is.na(tmp)] <- 0
  tmp <- tmp[c("Date", "GID", teamIds)] # match the column order
  return(tmp)
}

matchSeasons <- lapply(dataSeasons, parseMatches)
names(matchSeasons) <- c("S2010", "S2011", "S2012", "S2013", "S2014", "S2015")

head(matchSeasons$S2010)

# Y data frame: score differences by date for each game
# scoreSeasons <- lapply(dataSeasons, function(x) dcast(x, Date ~ Team, sum, value.var = "ScoreDiff"))
scoreSeasons <- lapply(dataSeasons, function(x) subset(x, HmVis == 1, select = c("Date", "GID", "ScoreDiff")))
names(scoreSeasons) <- c("S2010", "S2011", "S2012", "S2013", "S2014", "S2015")

head(scoreSeasons$S2010)

# D data frame: covariates matrices by date
parseCovs <- function(season){
  row.names(season) <- NULL
  return(subset(season,
                HmVis == 1,
                select = c("Date", "GID", "DblHdr", "DayNight", "ParkID")
                ))
}

covSeasons <- lapply(dataSeasons, parseCovs)
names(covSeasons) <- c("S2010", "S2011", "S2012", "S2013", "S2014", "S2015")

head(covSeasons$S2010)

save(matchSeasons, scoreSeasons, covSeasons, file = "./data/parsedData.RData")
load("./data/parsedData.RData")
