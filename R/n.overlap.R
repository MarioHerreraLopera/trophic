#' @title Calculate the trophic niche overlap and its 95 percent confidence intervals for species pairs.
#'
#' @param sp1 Species input matrix created with the n.matrix or a data matrix of prey abundance consumed by a species, where rows correspond to prey and columns correspond to individuals..
#' @param sp2 Species input matrix created with the n.matrix or a data matrix of prey abundance consumed by a species, where rows correspond to prey and columns correspond to individuals..
#' @param it Desired iterations number, MUST be equal to the number of iterations used in the input matrix. In case n.matrix generated matrix are not used, it MUST be equal to columns matrix number.
#' @param method Overlap metric used to measure the overlap between the pair of species. Pianka, Renkonen or Morisita can be used.
#'
#' @return A matrix of a single row and four columns, where the first column corresponds to the mean overlap value, the second to the standard deviation (SD) and the third and fourth to the inferior and superior confidence intervals, respectively.
#' @usage n.overlap(sp1, sp2, it, method = c("pianka", "schoener", "morisita"))
#' @examples
#' sp1 <- matrix(sample(0:5, 100, replace = T), nrow = 10, ncol = 10) ## a Random abundance matrix
#' sp2 <- matrix(sample(0:7, 100, replace = T), nrow = 10, ncol = 10) ## a Random abundance matrix
#' sp1.ent <- n.matrix(sp1, it = 100) ## Input matrix for Species 1
#' sp2.ent <- n.matrix(sp2, it = 100) ## Input matrix for Species 2
#' n.overlap(sp1.ent, sp2.ent, it = 100, method = "pianka") ## Pianka's overlap
#' n.overlap(sp1.ent, sp2.ent, it = 100, method = "schoener") ## Renkonen's = Schoener overlap
#' n.overlap(sp1.ent, sp2.ent, it = 100, method = "morisita") ## Morisita's overlap measure

n.overlap <- function(sp1, sp2, it, method = c("pianka", "schoener", "morisita")){OUTPUT <- list()
for(j in 1:1){
  OUTPUT[[j]] <- matrix(0, it, 1)
  colnames(OUTPUT[[j]]) <- c("Overlap")
  rownames(OUTPUT[[j]]) <- 1:it
  for(i in 1:it){
    st1<-t(sp1)
    st2<-t(sp2)
    Rm=spaa::niche.overlap.pair(st1[i,],st2[i,], method = method) ## change method for other overlap functions
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
