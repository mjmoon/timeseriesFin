ALfinal <- read.table('./data/2009AL.txt', header=T)
NLfinal <- read.table('./data/2009NL.txt', header=T)
ALfinal$Lg <- "AL"
NLfinal$Lg <- "NL"
final <- rbind(ALfinal, NLfinal)
# sort team according to wins
final <- final[order(final$W, decreasing=TRUE), ]
# assign initial rank centered at 0 with random errors
final$str <- 14.5:-14.5
# sort team according to team name
ALfinal <- final[final$Lg == 'AL', ]
NLfinal <- final[final$Lg == 'NL', ]
ALfinal <- ALfinal[order(ALfinal$Tm), ]
NLfinal <- NLfinal[order(NLfinal$Tm), ]
ind <- grep("MIA", NLfinal$Tm)
NLfinal[length(NLfinal$Tm) + 1, ] <- NLfinal[ind, ]
NLfinal <- NLfinal[-ind, ]
# x-hat0
x0 <- rbind(ALfinal, NLfinal)$str