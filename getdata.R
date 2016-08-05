library(retrosheet)
library(plyr)
getFileNames()

teamIds <- getTeamIDs(2015)

data15 <- sapply(teamIds, function(x) getRetrosheet('play', 2015, x)) 
dataGame <- getRetrosheet('game', 2015)
dataRoster <- getRetrosheet('roster', 2015)
dataRoster$TOR
head(dataGame[dataGame$DblHdr > 0,2])
names(dataGame)
# Date; DblHdr; DayNight; ParkID; 
