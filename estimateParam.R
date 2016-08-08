library(nlme)
source("getdata.R")
source("getx0.R")
# load("./data/parsedData.RData")

# initial parameters
sig0 <- 3
sigV <- 1
sigW <- 1
u <- rep(1, 32) # initialize u

# equation 9 of paper
#Predictive cov matrix, Sigma_t+1|t
sigma_pred <- function(t, seasoni, sig0, sigW, sigV){
  sigma_post(t - 1, seasoni, sigW, sigV, sig0)
}

# equation 10 of paper
# Posterior cov matrix, Sigma_t+1|t+1 
sigma_post <- function(t, seasoni, sig0, sigW, sigV){
  if(t > 0){
    Ct <- getC(t, seasoni) # C_t
    mt <- nrow(Ct)  # m: number of games on day t
    Nt <- sigW*diag(mt) # N: 
    sprev <- sigma_pred(t, seasoni, sigW, sigV, sig0)
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
sigma_pred(3, 2, sigW, sigV, sig0) # Sig_pred_3|2 = Sig_post_2|2 + M = Sig0 + M

# recurssive function to calculate x
# 2.2.4 (ii) of paper
# prediction of system states
x_pred <- function(t, seasoni, x0, sigW, sigV, u){
  x_post(t - 1, seasoni, x0, sigW, sigV, u)
}

# equation 11 of paper
# posterior estimate of system states
x_post <- function(t, seasoni, x0, sigW, sigV, u){
  if(t > 0){
    Ct <- getC(t, seasoni) # C_t
    Dt <- getD(t, seasoni) # D_t
    yt <- getY(t, seasoni) # y_t, point differentials on games in t-th game day
    mt <- nrow(Ct)  # m: number of games on day t
    Nt <- sigW*diag(mt) # N:
    xprev <- x_post(t - 1, seasoni, x0, sigW, sigV, u) # X-hat_t-1 | t-1
    spost <- sigma_post(t, seasoni, sig0, sigW, sigV) # Sigma-hat_t | t
    return(xprev + (1/sigW)*spost%*%t(Ct)%*%(yt - Ct%*%xprev - Dt%*%u))
  }
  else{
    x0
  }
}

## test ##
# third game of 2010
x_post(3, 1, x0, sigW, sigV, u) # x_3|2

# likelihood function
# equation 21 of the paper
set.seed(20160806)
pars <- c(sig0, sigV, sigW, u)

loglike <- function(pars, t, seasoni, x0){
  sig0 <- pars[1]
  sigW <- pars[3]
  sigV <- pars[2]
  u <- pars[-c(1:3)]
  
  Ct <- getC(t, seasoni)
  Dt <- getD(t, seasoni)
  Yt <- getY(t, seasoni)
  xpred <- x_pred(t, seasoni, x0, sigW, sigV, u)
  spred <- sigma_pred(t, seasoni, sig0, sigW, sigV)
  Nt <- sigW*diag(nrow(Ct))
  
  yhat <- Ct%*%xpred + Dt%*%u
  Ohat <- Ct%*%spred%*%t(Ct) + Nt
  
  ydiff <- Yt - yhat
  
  loglik <- log(det(Ohat)) - t(ydiff)%*%solve(Ohat)%*%ydiff
  return(as.vector(loglik))
}

## test
loglike(pars, 3, 1, x0)

## total log-like
Loglike <- function(pars, seasoni, x0){
  # gamedays <- unique(scoreSeasons[[seasoni]]$Tind)
  gamedays <- c(1:25)
  sum(sapply(gamedays, loglike, pars = pars, seasoni = seasoni, x0 = x0))
}

Loglike(pars, 1, x0)
