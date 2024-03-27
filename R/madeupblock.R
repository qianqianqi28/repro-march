rm(list = ls())
library(anacor)
library(readxl)
library(xtable)
source("R/deletezerovector.R")
dt <- read_excel("data/processed/madeupblock.xlsx")

dt <- as.data.frame(dt)
rownames(dt) <- dt[, 1]
dt <- dt[, -1]
dim(dt)

# This is for delete the 0 vectors in rows
dt <- deletezerovector(dt, dim = 1)

# This is for delete the 0 vectors in columns
dt <- deletezerovector(dt, dim = 2)

  
dt <- as.matrix(dt)

X <- dt
dim(X)
dim(X)[1] * dim(X)[2]
sum(X)
sum(X == 0)

#CA

X.P <- X / sum(X)
X.r <- apply(X.P, 1, sum)
X.c <- apply(X.P, 2, sum)

X.Dr <- diag(X.r)
X.Dc <- diag(X.c)
X.Drmh <- diag(1 / sqrt(X.r))
X.Dcmh <- diag(1 / sqrt(X.c))

X.P <- as.matrix(X.P)
X.S <- X.Drmh %*% (X.P - X.r %o% X.c) %*% X.Dcmh
X.svd <- svd(X.S)

#Singular values
round((X.svd$d), 3)
round((X.svd$d^2), 3)
round(100 * (X.svd$d^2) / sum(X.svd$d^2), 1)

#Standard coordinates for columns
colproj <- X.Dcmh %*% X.svd$v
rownames(colproj) <- colnames(X)

#Standard coordinates for rows
rowproj <- X.Drmh %*% X.svd$u
rownames(rowproj) <- rownames(X)

round(rowproj, 2)
round(colproj, 2)

#reorder the rows and columns
X[order(rowproj[, 1]), order(colproj[, 1])]

#convenient for the user of latex
print(xtable(rowproj, digits = rep(2, (ncol(X) + 1))), include.rownames = T, include.colnames = T)
print(xtable(colproj, digits = rep(2, (ncol(X) + 1))), include.rownames = T, include.colnames = T)


print(xtable(X[order(rowproj[, 1]), order(colproj[, 1])], digits = rep(0, (ncol(X) + 1))), include.rownames = T, include.colnames = T)
