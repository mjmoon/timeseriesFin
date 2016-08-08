library(FKF)
source("getdata.R")

seasoni <- 1
gamedays <- unique(scoreSeasons[[seasoni]]$Tind)
yt <- sapply(gamedays, getY18, seasoni)
Ct <- sapply(gamedays, getC18, seasoni, simplify = "array")
Dt <- sapply(gamedays, getD18, seasoni, simplify = "array")
uDt <- apply(Dt, 3, function(x) x%*%u)

t0 <- proc.time()
testkf <- fkf(a0 = x0, 
              P0 = diag(30),
              dt = matrix(0, nrow = 30, ncol = 18),
              ct = uDt, 
              Tt = diag(30), 
              Zt = Ct,
              HHt = diag(30), 
              GGt = diag(18), 
              yt, check.input = F)
proc.time() - t0

dim(uDt)
dim(yt)
length(x0)
dim(Ct)
