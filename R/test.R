### test ##

matriz.ent <- matrix(sample(0:5, 100, replace = T), nrow = 10, ncol = 10)



n.matrix <- function(x, it) {
  row <- length(x[,1])
  output = matrix(0, row, it)
  for(i in 1:it){
    r = x[,sample(ncol(x), size = 1, replace = T), drop = FALSE]
    output[,i] <- r
    ms <- it+1
  }
  output
}


n.breadth <- function(x, it){ OUTPUT <- list()
for(j in 1:1){
  OUTPUT[[j]] <- matrix(0, it, 3)
  colnames(OUTPUT[[j]]) <- c("q0", "q1", "q2")
  rownames(OUTPUT[[j]]) <- 1:it
  for(i in 1:it){
    st1<-t(x)
    VE = st1[i,]
    OUTPUT[[j]][i,1] <- sum(VE>0)
    Pi <- VE/sum(VE)
    Pi <- Pi[which(Pi>0)]
    OUTPUT[[j]][i,2]  <- exp(-sum(Pi*log(Pi)))
    OUTPUT[[j]][i,3]  <- 1/ sum(Pi^2)
    OUTPUT[[j]][i,][is.infinite(OUTPUT[[j]][i,])] <- 0
  }
}

OUTPUT2 <- list()
for(j in 1:1){
  Mean <- colMeans(OUTPUT[[j]])
  DS <- sqrt(diag(var(OUTPUT[[j]])))
  IC95i=Mean-(1.96*(DS/sqrt(it)))
  IC95s=Mean+(1.96*(DS/sqrt(it)))
  OUTPUT2[[j]] <- data.frame(Mean=Mean, DS=DS, IC95i=IC95i, IC95s=IC95s)

}
OUTPUT2 <- structure(OUTPUT2, names=colnames(x)[2:(it+1)])
OUTPUT2}

m.ent <- n.matrix(matriz.ent, it = 50)
n.breadth(m.ent, it = 50)


### n.overlap

## matriz entrada

sp1 <- matrix(sample(0:5, 100, replace = T), nrow = 10, ncol = 10)

sp2 <- matrix(sample(0:7, 100, replace = T), nrow = 10, ncol = 10)

n.matrix <- function(x, it) {
  row <- length(x[,1])
  output = matrix(0, row, it)
  for(i in 1:it){
    r = x[,sample(ncol(x), size = 1, replace = T), drop = FALSE]
    output[,i] <- r
    ms <- it+1
  }
  output
}

## Overlap estimation function

library(spaa)

sp1.ent <- n.matrix(sp1, 100) ## name for first matrix
sp2.ent <- n.matrix(sp2, 100)


n.overlap <- function(sp1,sp2,it, method = c("pianka", "schoener", "morisita")){OUTPUT <- list()
for(j in 1:1){
  OUTPUT[[j]] <- matrix(0, it, 1)
  colnames(OUTPUT[[j]]) <- c("overlap")
  rownames(OUTPUT[[j]]) <- 1:it
  for(i in 1:it){
    st1<-t(sp1)
    st2<-t(sp2)
    Rm=niche.overlap.pair(st1[i,],st2[i,], method = method) ## change method for other overlap functions
    OUTPUT[[j]][i,] <- Rm
    OUTPUT[[j]][i,][is.nan(OUTPUT[[j]][i,])] <- 0
  }
}
OUT2 <- list()
for(j in 1:1){
  Mean <- colMeans(OUTPUT[[j]])
  DS <- sqrt(diag(var(OUTPUT[[j]])))
  IC95i=Mean-(1.96*(DS/sqrt(it)))
  IC95s=Mean+(1.96*(DS/sqrt(it)))
  OUT2[[j]] <- data.frame(Mean=Mean, DS=DS, IC95i=IC95i, IC95s=IC95s)
}
OUT2
}

n.overlap(sp1.ent, sp2.ent, 100, method = "morisita")

### overlap hill

sp1.ent <- n.matrix(sp1, 100) ## name for first matrix
sp2.ent <- n.matrix(sp2, 100)

n.overlap.hill <- function(sp1, sp2, it, order = c(0, 1, 2)){OUTPUT <- list()
for(j in 1:1){
  OUTPUT[[j]] <- matrix(0, it, 1)
  colnames(OUTPUT[[j]]) <- c("overlap")
  rownames(OUTPUT[[j]]) <- 1:it
  for(i in 1:it){
    st1<-t(sp1)
    st2<-t(sp2)
    Rm= 1-(vegetarian::turnover(st1[i,],st2[i,], q=order))
    OUTPUT[[j]][i,] <- Rm
    OUTPUT[[j]][i,][is.nan(OUTPUT[[j]][i,])] <- 0
  }
}
OUT2 <- list()
for(j in 1:1){
  Mean <- colMeans(OUTPUT[[j]])
  DS <- sqrt(diag(var(OUTPUT[[j]])))
  IC95i=Mean-(1.96*(DS/sqrt(it)))
  IC95s=Mean+(1.96*(DS/sqrt(it)))
  OUT2[[j]] <- data.frame(Mean=Mean, DS=DS, IC95i=IC95i, IC95s=IC95s)
}
OUT2
}

n.overlap.hill(sp1.ent, sp2.ent, 100, order = 2)
