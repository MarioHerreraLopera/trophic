#' @title Calculate the trophic niche overlap and its 95% confidence intervals for species pairs based on Hill series.
#'
#' @param sp1 First species input matrix created with n.matrix or a data matrix of prey abundance.
#' @param sp2 Second species input matrix created with n.matrix or a data matrix of prey abundance.
#' @param it Desired iterations number, MUST be equal to number of columns.
#' @param order Corresponds to the desired diversity order for the overlap calculation.
#'
#' @return A matrix of a single row and four columns, where the first column corresponds to the mean overlap value, the second to the standard deviation (SD) and the third and fourth to the inferior and superior confidence intervals, respectively.
#' @usage n.overlap.hill(sp1, sp2, it, order = c("0", "1", "2"))
#' @examples
#' sp1 <- matrix(sample(0:5, 100, replace = T), nrow = 10, ncol = 10) ## a Random abundance matrix
#' sp2 <- matrix(sample(0:7, 100, replace = T), nrow = 10, ncol = 10) ## a Random abundance matrix
#' sp1.ent <- n.matrix(sp1, it = 100) ## Input matrix for Species 1
#' sp2.ent <- n.matrix(sp2, it = 100) ## Input matrix for Species 2
#' n.overlap.hill(sp1.ent, sp2.ent, it = 100, order = 0) ## overlap by prey richness
#' n.overlap.hill(sp1.ent, sp2.ent, it = 100, order = 1) ## overlap by equally common prey
#' n.overlap.hill(sp1.ent, sp2.ent, it = 100, order = 2) ## overlap by dominant prey

n.overlap.hill <- function(sp1, sp2, it, order = c(0, 1, 2)){OUTPUT <- list()
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
  OUT2[[j]] <- data.frame(Mean=Mean, SD=DS, IC95i=IC95i, IC95s=IC95s)
}
OUT2
}
