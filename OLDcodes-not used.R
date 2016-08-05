
head(tor15[[1]])
tor15[[1]]$play[,6]
date01 <- as.Date(tor15[[1]]$info[4,2]) 

# sapply(tor, function(x) )
tor1game <- as.data.frame(tor15[[1]]$play)
tor50game <- as.data.frame(tor15[[50]]$play)
tor15[[50]]$info

# Batting average (BA): 
#  # hits / # at bats
# At-bat (AB): 
#  plate appearance 
#  !walk !hit-by-pitch !sacrifice !inteference
# On-base plus Slugging (OPS):
#  On-base percentage + slugging percentage
# On-base percentage (OBP):
#  (hits + walks + hit-by-pitches)/(appearance except sacrifice bunt)
# Slugging percentage (SLG):
#  total number of bases per at-bat (1B + 2Bx2 + 3Bx3 + HRx4)/AB
# Plate appearance:
#  completed turn !inning-end !replaced (unless at 2K's then KOut)
testttt <- laply(tor15, countstats)


countstats <- function(game){
  counts <- as.data.frame(game$play)
  counts <- subset(tor1game, select = c("retroID", "play"), team == 1)
  game$play$hit <- sapply(game$play, function(x) {
    if(substr(x, 1, 1) == "S" &
       !substr(x, 1, 2) == "SB") 1
    else if(substr(x, 1, 1) == "D") 2
    else if(substr(x, 1, 1) == "T") 3
    else if(substr(x, 1, 2) == "HR" |
            (substr(x, 1, 1) == "H" &
             !substr(x, 1, 2) == "HP")) 4
    else 0
  })
  
  return(counts)
}
# Hits...count:
# "S":    single
# "D":    double
# "T":    triple
# "H":    home run
# "HR":   home run
# not "HP":   hit by a pitch


# Plate appearance...exclude:
# "BK": balk
# "CS": caught stealing
# "DI": defensive indifference for a stolen base
# "OA": base runner advances
# "PB": passed ball
# "WP": wild pitch
# "PO": picked off
# "SB": stolen base
# "NP": no play
tor1game$papp <- (substr(tor1game$play,1,2) != "BK" & 
                    substr(tor1game$play,1,2) != "CS" &
                    substr(tor1game$play,1,2) != "DI" &
                    substr(tor1game$play,1,2) != "OA" &
                    substr(tor1game$play,1,2) != "PB" &
                    substr(tor1game$play,1,2) != "WP" &
                    substr(tor1game$play,1,2) != "PO" &
                    substr(tor1game$play,1,2) != "SB" &
                    substr(tor1game$play,1,2) != "NP"
)

# At bat...plate appearance - exclude:
# "HP":   hit by a pitch
# "C/E2": catcher interference 
# "C/E1": pitcher interference
# "C/E3": first basemane inteference
# "I":    intentional walk
# "IW":   intentional walk
# "W":    walk
# "SF":   sacrifice fly 
# "SH":   sacrifice hit/bunt
tor1game$atbat <- tor1game$papp &
  (substr(tor1game$play,1,1) != "W" & 
     substr(tor1game$play,1,1) != "I" & 
     substr(tor1game$play,1,2) != "HP" &
     substr(tor1game$play,1,3) != "C/E" &
     ! "SF" %in% tor1game$play &
     ! "SH" %in% tor1game$play
  )

