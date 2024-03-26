rm(list=ls()) 
library(anacor)
library(readxl)
library(xtable)

dt <- read_excel("madeupblock.xlsx")

dt <- as.data.frame(dt)
rownames(dt) <- dt[,1]
dt <- dt[,-1]
dim(dt)
if (sum(apply(dt, 1, sum) == 0) != 0){
  dt <- dt[-c(which(apply(dt, 1, sum) == 0)),]
} else {
  dt <- dt}
if (sum(apply(dt, 2, sum) == 0) != 0){
  dt <- dt[,-c(which(apply(dt, 2, sum) == 0))]
} else {
  dt <- dt}
dt <- as.matrix(dt)

X <- dt
dim(X)
dim(X)[1]*dim(X)[2]
sum(X)
sum(X == 0)

X.P    <- X/sum(X)
X.r    <- apply(X.P, 1, sum)
X.c    <- apply(X.P, 2, sum)

X.Dr   <- diag(X.r)
X.Dc   <- diag(X.c)
X.Drmh <- diag(1/sqrt(X.r))
X.Dcmh <- diag(1/sqrt(X.c))

X.P   <- as.matrix(X.P)
X.S   <- X.Drmh%*%(X.P-X.r%o%X.c)%*%X.Dcmh
X.svd <- svd(X.S)

round((X.svd$d), 3)
round((X.svd$d^2), 3)
round(100*(X.svd$d^2)/sum(X.svd$d^2), 1)


colproj    <- X.Dcmh %*% X.svd$v
rownames(colproj) <- colnames(X)

rowproj    <- X.Drmh %*% X.svd$u
rownames(rowproj) <- rownames(X)

round(rowproj, 2)
round(colproj, 2)


X[order(rowproj[,1]), order(colproj[,1])]

print(xtable(rowproj,digits=rep(2, (ncol(X) + 1))), include.rownames=T, include.colnames=T)
print(xtable(colproj,digits=rep(2, (ncol(X) + 1))), include.rownames=T, include.colnames=T)


print(xtable(X[order(rowproj[,1]), order(colproj[,1])],digits=rep(0, (ncol(X) + 1))), include.rownames=T, include.colnames=T)

