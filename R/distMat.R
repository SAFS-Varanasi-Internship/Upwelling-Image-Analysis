#' Distance matrices
#' @aliases corMat, distMat
#'
#' `distMat` returns `stats::dist(X, diag=TRUE, upper=TRUE)`. `corMat` returns a distance matrix
#' from the correlation matrix.
#' 
#' @param X A matrix
#' @param remove.mean remove the mean from each row of X
#' 
#' @export
distMat <- function(X, remove.mean=FALSE){
  K <- nrow(X)
  if(remove.mean==TRUE) X <- t(scale(t(X), scale=FALSE))
  distanceMatrix <- matrix(NA, nrow=K, ncol=K)
  for(r in 1:K){
    for(c in 1:K){
      distanceMatrix[r,c] <- sqrt(sum((X[r,]-X[c,])^2))
    }
  }
  distanceMatrix
}

corMat <- function(X){
  K <- nrow(X)
  distanceMatrix <- matrix(NA, nrow=K, ncol=K)
  for(r in 1:K){
    for(c in 1:K){
      distanceMatrix[r,c] <- stats::cor(X[r,], X[c,])
    }
  }
  distanceMatrix
}