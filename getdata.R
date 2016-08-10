library(retrosheet)
library(plyr)
library(reshape2)
yrs <- c(2011:2015)
teamIds <- unique(c(sapply(yrs, getTeamIDs)))
# Florida Marlins changed to Miami Marlins
teamIds <- teamIds[teamIds != "FLO"]

# Variables: Teams, Scores, Date; DblHdr; 
fields <- c("HmTm", "VisTm", "HmLine", "VisLine", "Date", "DblHdr")
dataGame <- lapply(yrs, function(x) getPartialGamelog(x, fields)) 
dataGame <- lapply(dataGame, as.data.frame)

parseSeason <- function(season){
  season$GID <- paste(paste(season$Date, season$HmTm, sep = "-"), season$DblHdr, sep = "")
  season$Tind <- as.numeric(factor(season$Date))
  season$Date <- as.Date(season$Date, "%Y%m%d")
  season$DblHdr <- as.factor(season$DblHdr)
  # Florida Marlins changed to Miami Marlins
  season$VisTm[season$VisTm == "FLO"] <- "MIA"
  season$VisTm <- factor(season$VisTm, teamIds)
  season$HmTm[season$HmTm == "FLO"] <- "MIA"
  season$HmTm <- factor(season$HmTm, teamIds)
  season$Host <- season$HmTm
  season$ScoreDiffHm <- 
    sapply(strsplit(season$HmLine, ""), 
           function(x) sum(as.numeric(x), na.rm = TRUE)) -
    sapply(strsplit(season$VisLine, ""), 
           function(x) sum(as.numeric(x), na.rm = TRUE))
  
  tmpVis <- subset(season, select = c("GID", "Tind", "Date", "DblHdr", "VisTm", 
                                      "Host", "ScoreDiffHm"))
  names(tmpVis) <- c("GID", "Tind", "Date", "DblHdr", "Team", 
                     "Host", "ScoreDiff")
  tmpVis$ScoreDiff <- -tmpVis$ScoreDiff
  tmpVis$HmVis <- -1
  
  
  tmpHm <- subset(season, select = c("GID", "Tind", "Date", "DblHdr", "HmTm", 
                                      "Host", "ScoreDiffHm"))
  names(tmpHm) <- c("GID", "Tind", "Date", "DblHdr", "Team", 
                     "Host", "ScoreDiff")
  tmpHm$HmVis <- 1
  
  
  tmp <- rbind(tmpVis, tmpHm)
  return(tmp[order(tmp$Date, tmp$Host), ])
}

dataSeasons <- lapply(dataGame, parseSeason)
names(dataSeasons) <- c("S2011", "S2012", "S2013", "S2014", "S2015")
head(dataSeasons$S2011)
# C data frame: match matrices by date with each row representing a single game 
#               (home team: 1; away team: -1)
parseMatches <- function(season){
  tmp <- ddply(season, c("Date", "Tind"), 
               function(x) dcast(x, GID ~ Team, sum, value.var = "HmVis"))
  tmp[is.na(tmp)] <- 0
  tmp <- tmp[c("Date", "Tind", teamIds)] # match the column order
  return(tmp)
}

matchSeasons <- lapply(dataSeasons, parseMatches)
names(matchSeasons) <- c("S2011", "S2012", "S2013", "S2014", "S2015")

head(matchSeasons$S2011)

# Y data frame: score differences by date for each game
scoreSeasons <- lapply(dataSeasons, function(x) subset(x, HmVis == 1, 
                  select = c("Date", "Tind", "ScoreDiff")))
names(scoreSeasons) <- c("S2011", "S2012", "S2013", "S2014", "S2015")

head(scoreSeasons$S2011)

# D data frame: covariates matrices by date
parseCovs <- function(season){
  tmp <- subset(season, HmVis == 1, select = c("Date", "Tind", "DblHdr", "Host"))
  row.names(tmp) <- NULL
  # tmp <- cbind(tmp[,c(1,2)], model.matrix(~tmp$DblHdr + tmp$Host)[,-1])
  tmp <- cbind(tmp[,c(1,2)], model.matrix(~tmp$DblHdr + tmp$Host))
  return(tmp)
}

covSeasons <- lapply(dataSeasons, parseCovs)
names(covSeasons) <- c("S2011", "S2012", "S2013", "S2014", "S2015")

head(covSeasons$S2010)

# save(matchSeasons, scoreSeasons, covSeasons, file = "./data/parsedData.RData")

getByTind <- function(x, tind){
  return(x[x$Tind == tind, -c(1,2), drop = F])
}

getC <- function(t, seasoni){
  return(as.matrix(getByTind(matchSeasons[[seasoni]], t)))
}

getD <- function(t, seasoni, level = 2){
  D <- as.matrix(getByTind(covSeasons[[seasoni]], t))
  if(level < 2)
    if(level > 0) D <- D[ , c(1:3)]
    else D <- D[ , 1]
  return(D)
}

getY <- function(t, seasoni){
  return(as.matrix(getByTind(scoreSeasons[[seasoni]], t)))
}

# max number of games per day: 18
# gamedays <- sapply(scoreSeasons, function(x) unique(x$Tind))
# max(sapply(c(1:5), function(x) max(sapply(gamedays[[x]], function(y) length(getY(y, x))))))


getC18 <- function(t, seasoni){
  c <- getC(t, seasoni)
  return(rbind(c, matrix(0, nrow = 18 - nrow(c), ncol = ncol(c))))
}

getD18 <- function(t, seasoni, level = 2){
  d <- getD(t, seasoni, level = level)
  if(level == 0)
    return(c(d, rep(0, 18 - length(d))))
  return(rbind(d, matrix(0, nrow = 18 - nrow(d), ncol = ncol(d))))
}

getY18 <- function(t, seasoni){
  y <- getY(t, seasoni)
  return(c(y, rep(0, 18 - length(y))))
}