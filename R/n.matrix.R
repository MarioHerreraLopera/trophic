#' n.matrix: Input matrix for calculations with intervals of niche width and niche overlap
#'
#' @param x A data matrix of prey abundance consumed by a species, where rows correspond to prey and columns correspond to individuals.
#' @param it Desired iterations number
#' @return A matrix with columns = it, where each column corresponds to the random selection of an individual from the original matrix.
#' @examples
#' sp1 <- matrix(sample(0:5, 100, replace = T), nrow = 10, ncol = 10) ## a Random abundance matrix
#' n.matrix(sp1, it = 100)

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




