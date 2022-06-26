n.overlap.hill <- function(s1, s2, it, order = c("0", "1", "2")){OUTPUT <- list()
for(j in 1:1){
  OUTPUT[[j]] <- matrix(0, it, 1)
  colnames(OUTPUT[[j]]) <- c("Overlap")
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
