library(nlme)
# library(retrosheet)
source(getdata.R)
# load("./data/parsedData.RData")

# sigma-hat0
sigma0 <- 3
Sig0 <- sigma0*diag(length(teamIds))
sigV <- 1
M <- sigV*diag(n)
sigW <- 1

ALfinal <- read.table('./data/2009AL.txt', header=T)
NLfinal <- read.table('./data/2009NL.txt', header=T)
ALfinal$Lg <- "AL"
NLfinal$Lg <- "NL"
final <- rbind(ALfinal, NLfinal)
# sort team according to wins
final <- final[order(final$W, decreasing=TRUE), ]
# assign initial rank centered at 0 with random errors
set.seed(20160806)
final$str <- 14.5:-14.5 + rnorm(length(final$Tm), 0, sigma0)
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
n <- length(final$Tm)

# C matrix
C1 <- as.matrix(matchSeasons$S2010[matchSeasons$S2010$Date == "2010-04-05", ][, -1:-2])
# D Matrix
D1 <- covSeasons$S2010[covSeasons$S2010$Date == "2010-04-05", ][,c(3,5)]
# y
y1 <- scoreSeasons$S2010[scoreSeasons$S2010$Date == "2010-04-05", ][,3]

# equation 9 of paper
#Predictive cov matrix, Sigma_t+1|t
sigma_pred <- function(t, matches){
  sigma_post(t - 1, matches)
}

# equation 10 of paper
# Posterior cov matrix, Sigma_t+1|t+1 
sigma_post <- function(t, matches){
  if(t > 0){
    Ct <- getByTind(matches, t) # C_t
    mt <- nrow(Ct)  # m: number of games on day t
    Nt <- sigW*diag(mt) # N: 
    sprev <- sigma_pred(t, matches)
    return(sprev - 
             sprev%*%t(Ct)%*%
             solve(as.matrix(Ct)%*%sprev%*%t(Ct) + Nt)%*%
             as.matrix(Ct)%*%sprev)
  }
  else{
    Sig0 + M
  }
}

## test ##
sigma_pred(1, matchSeasons$S2010) # Sig_pred_1|0 = Sig_pred_0|0 + M = Sig0 + M
sigma_pred(2, matchSeasons$S2010) # Sig_pred_2|1
sigma_pred(3, matchSeasons$S2010) # Sig_pred_3|2
