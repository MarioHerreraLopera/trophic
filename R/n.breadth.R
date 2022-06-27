#' @title Calculate the trophic niche breadth and 95 percent confidence intervals based on Hill series.
#'
#' @param x An input matrix created with the n.matrix function or a data matrix of prey abundance consumed by a species, where rows correspond to prey and columns correspond to individuals..
#' @param it Desired iterations number, MUST be equal to the number of iterations used in the input matrix. In case n.matrix generated matrix are not used, it MUST be equal to columns matrix number.
#' @return A matrix of three rows and four columns, where each row corresponds to an diversity order (i.e. q0, q1 and q2) and the columns correspond, in order, to the mean of the estimated value, the standard deviation (SD) and the inferior and superior 95 percent confidence intervals.
#' @usage n.breadth(x, it)
#' @examples
#' sp1 <- matrix(sample(0:5, 100, replace = T), nrow = 10, ncol = 10) ## a random abundance matrix
#' sp1.ent <- n.matrix(sp1, it = 100) ## Input matrix for the species
#' n.breadth(sp1.ent, it = 100)
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
  OUTPUT2[[j]] <- data.frame(Mean=Mean, SD=DS, IC95i=IC95i, IC95s=IC95s)

}
OUTPUT2 <- structure(OUTPUT2, names=colnames(x)[2:(it+1)])
OUTPUT2}
