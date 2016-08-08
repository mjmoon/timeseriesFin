source("getdata.R")
source("getx0.R")
# load("./data/parsedData.RData")

# equation 9 of paper # Predictive cov matrix, Sigma_t+1|t
sigma_pred <- function(t, seasoni, sig0, sigW, sigV){
  sigma_post(t - 1, seasoni, sig0, sigV, sigW)
}

# equation 10 of paper # Posterior cov matrix, Sigma_t+1|t+1 
sigma_post <- function(t, seasoni, sig0, sigV, sigW){
  if(t > 0){
    Ct <- getC(t, seasoni) # C_t
    mt <- nrow(Ct)  # m: number of games on day t
    Nt <- sigW*diag(mt) # N: 
    sprev <- sigma_pred(t, seasoni, sig0, sigV, sigW)
    return(sprev - 
             sprev%*%t(Ct)%*%
             solve(Ct%*%sprev%*%t(Ct) + Nt)%*%
             Ct%*%sprev)
  }
  else{
    (sig0 + sigV)*diag(30)
  }
}

## test ##
# sigma_pred(3, 2, sigW, sigV, sig0) # Sig_pred_3|2 = Sig_post_2|2 + M = Sig0 + M

# 2.2.4 (ii) of paper
# prediction of system states
x_pred <- function(t, seasoni, x0, sig0, sigV, sigW, u, clevel = 0){
  x_post(t - 1, seasoni, x0, sig0, sigV, sigW, u, clevel)
}

# equation 11 of paper
# posterior estimate of system states
x_post <- function(t, seasoni, x0, sig0, sigV, sigW, u, clevel = 0){
  if(t > 0){
    Ct <- getC(t, seasoni) # C_t
    Dt <- getD(t, seasoni, clevel) # D_t
    yt <- getY(t, seasoni) # y_t, point differentials on games in t-th game day
    mt <- nrow(Ct)  # m: number of games on day t
    Nt <- sigW*diag(mt) # N:
    xprev <- x_post(t - 1, seasoni, x0, sig0, sigV, sigW, u, clevel) # X-hat_t-1 | t-1
    spost <- sigma_post(t, seasoni, sig0, sigV, sigW) # Sigma-hat_t | t
    if(clevel == 0)
      return(xprev + (1/sigW)*spost%*%t(Ct)%*%(yt - Ct%*%xprev - u*Dt))
    return(xprev + (1/sigW)*spost%*%t(Ct)%*%(yt - Ct%*%xprev - Dt%*%u))
  }
  else{
    x0
  }
}

## test ##
# third game of 2010
x_post(5, 1, x0s[,1], pars[1], pars[2], pars[3], meanYs[1], 0) # x_3|2
# sum(x_post(10, 1, x0, sigW, sigV, u[1], 0))

# likelihood function
# equation 21 of the paper
set.seed(20160806)
pars <- c(rbeta(3, 1, 1))

loglike <- function(pars, t, seasoni, x0, u){
  sig0 <- pars[1]
  sigV <- pars[2]
  sigW <- pars[3]
  
  Ct <- getC(t, seasoni)
  Dt <- getD(t, seasoni, 0)
  Yt <- getY(t, seasoni)
  xpred <- x_pred(t, seasoni, x0, sig0, sigV, sigW, u)
  spred <- sigma_pred(t, seasoni, sig0, sigV, sigW)
  Nt <- sigW*diag(nrow(Ct))
  
  yhat <- Ct%*%xpred + u*Dt
  Ohat <- Ct%*%spred%*%t(Ct) + Nt
  
  ydiff <- Yt - yhat
  
  loglik <- log(det(Ohat)) - t(ydiff)%*%solve(Ohat)%*%ydiff
  return(-as.numeric(loglik))
}

## total -log-like (optim will minimize the function)
nLoglike <- function(pars, seasoni, x0s, us, numdts = 20){
  x0 <- x0s[,seasoni]
  u <- us[seasoni]
  gamedays <- c(1: numdts)
  -sum(sapply(gamedays, FUN = loglike, pars = pars, 
              seasoni = seasoni, x0 = x0, u = u))
}

## test
loglike(pars, 5, 1, x0s[,1], meanYs[1])
nLoglike(pars, 3, x0s, meanYs)

## intercept only ##
meanYs <- sapply(scoreSeasons, function(x) mean(x$ScoreDiff[1:20]))
# parest1 <- optim(pars, nLoglike, seasoni = 1, x0s = x0s, us = meanYs,
#                  method = "L-BFGS-B", 
#                  lower = c(0.00001,0.00001,0.00001), upper = c(16,16,16))
parest2 <- optim(pars, nLoglike, seasoni = 2, x0s = x0s, us = meanYs,
                 method = "L-BFGS-B", 
                 lower = c(0.00001,0.00001,0.00001), upper = c(16,16,16))
parest3 <- optim(pars, nLoglike, seasoni = 3, x0s = x0s, us = meanYs,
                 method = "L-BFGS-B", 
                 lower = c(0.00001,0.00001,0.00001), upper = c(16,16,16))
parest4 <- optim(pars, nLoglike, seasoni = 4, x0s = x0s, us = meanYs,
                 method = "L-BFGS-B", 
                 lower = c(0.00001,0.00001,0.00001), upper = c(16,16,16))
parest5 <- optim(pars, nLoglike, seasoni = 5, x0s = x0s, us = meanYs,
                 method = "L-BFGS-B", 
                 lower = c(0.00001,0.00001,0.00001), upper = c(16,16,16))
parest6 <- optim(pars, nLoglike, seasoni = 6, x0s = x0s, us = meanYs,
                 method = "L-BFGS-B", 
                 lower = c(0.00001,0.00001,0.00001), upper = c(16,16,16))

# library(parallel)
# cl <- makeCluster(2, type = "FORK")
# t0 <- proc.time()
# parest0 <- clusterApply(c(1:5), optim, fn = nLoglike, pars = pars, 
#                      x0s = x0s, us = meanYs, method = "L-BFGS-B", 
#                      lower = c(0.00001,0.00001,0.00001), upper = c(16,16,16))
# proc.time() - t0

## get estimates for u using regression fit (MLE takes too long)
uts <- t(sapply(dataSeasons, function(x) 
  coef(lm(ScoreDiff ~ DblHdr + Host, data = subset(x, HmVis == 1)))))[-1,]
sigmas <- rbind(parest2$par, 
                parest3$par, 
                parest4$par, 
                parest5$par, 
                parest6$par)