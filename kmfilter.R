library(FKF)
source("estimateParam.R")

mlbkm <- function(seasoni, covs = F){
  if(covs) {
    u <- array(c(utoptim[seasoni-1,-c(1:3)], utlm[seasoni-1,]), c(32,1))
    level <- 2
  } else {
    u <- array(utoptim[seasoni-1, 4], c(1, 1))
    level <- 0
  }
  gamedays <- which(!unique(scoreSeasons[[seasoni]]$Tind) %in% c(1:20))
  yt <- sapply(gamedays, getY18, seasoni)
  Ct <- sapply(gamedays, getC18, seasoni, simplify = "array")
  Dt <- sapply(gamedays, getD18, seasoni, level, simplify = "array")
  if(!covs) Dt <- array(Dt, dim = c(18, 1, ncol(Dt)))
  uDt <- apply(Dt, 3, function(x) x%*%u)
  
  testkf <- fkf(a0 = x0s[,seasoni], 
                P0 = utoptim[seasoni-1, 1]*diag(30),
                dt = matrix(0, nrow = 30, ncol = 1),
                ct = uDt, 
                Tt = array(diag(30), c(30,30,1)),
                Zt = Ct,
                HHt = array(utoptim[seasoni-1, 2]*diag(30), c(30,30,1)), 
                GGt = array(utoptim[seasoni-1, 3]*diag(18), c(18,18,1)),
                yt)
  
  xtts <- testkf$att
  xts <- testkf$at
  yts <- yt - round(testkf$vt)
  ytvec <- as.vector(yt)
  winprederr <- sum((as.vector(yts > 0)[ytvec != 0]) != 
                       (ytvec[ytvec != 0] > 0))/sum(ytvec != 0)
  scoremse <- sum((as.vector(testkf$vt)[ytvec != 0])^2)/sum(ytvec != 0)
  return(list(xpost = xtts, xpred = xts, ypred = yts,
              gprederr = winprederr, smse = scoremse))
}

library(ggplot2)
library(reshape2)
getSmse <- function(res) {
  return(res$smse)
}
getGpred <- function(res) {
  return(res$gprederr)
}

kmfres.int <- lapply(c(2:6), mlbkm)
kmfres.cov <- lapply(c(2:6), mlbkm, covs = T)

smse.int <- data.frame(value = sapply(kmfres.int, getSmse))
smse.cov <- data.frame(value = sapply(kmfres.cov, getSmse))

perr.int <- data.frame(value = sapply(kmfres.int, getGpred))
perr.cov <- data.frame(value = sapply(kmfres.cov, getGpred))

smse.int$model <- "Intercept-only"
smse.cov$model <- "With covariates"

perr.int$model <- "Intercept-only"
perr.cov$model <- "With covariates"

smse.int$var <- "smse"
smse.cov$var <- "smse"

perr.int$var <- "perr"
perr.cov$var <- "perr"

smse.int$season <- c(2011:2015)
smse.cov$season <- c(2011:2015)

perr.int$season <- c(2011:2015)
perr.cov$season <- c(2011:2015)

validdt <- rbind(smse.int, smse.cov, perr.int, perr.cov)

ggplot(data = subset(validdt, var == "smse")) + theme_minimal() +
  geom_bar(aes(x = season, y = value, fill = model), stat = "identity", 
           position = "dodge") +
  labs(x = "Season", y = "MSEs", title = "Score MSEs") +
  scale_fill_discrete(guide = FALSE) 
  # coord_cartesian(ylim = c(20, 25)) 
  # coord_cartesian(ylim = c(27, 35)) 

ggplot(data = subset(validdt, var == "perr")) + theme_minimal() +
  geom_bar(aes(x = season, y = value, fill = model), stat = "identity", 
           position = "dodge") +
  labs(x = "Season", y = "Error rates", title = "Game prediction error rates") +
  scale_fill_discrete(guide = FALSE) +
  coord_cartesian(ylim = c(0,1))

xdt <- as.data.frame(rbind(x0s[,6], t(kmfres.int[[5]]$xpost)))
names(xdt) <- teamIds
xdt.m <- melt(as.matrix(xdt))
names(xdt.m) <- c("Day", "Team", "Rating")
xdt.m$Day <- xdt.m$Day + 19


ggplot(data = subset(xdt.m, Team == "TOR" | Team == "BAL" |
                       Team == "BOS" | Team == "NYA" | 
                       Team == "TBA")) + theme_minimal() +
  geom_line(aes(x = Day, y = Rating, col = Team)) +
  labs(title = "2015 AL Easton Division rating estimation\nScaled parameters") 