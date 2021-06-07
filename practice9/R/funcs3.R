SIM <- function(A, f, u0, iter=10e5, eps=10e-7) {
  stopifnot(is.matrix(A) & length(A[1,]) == length(A[,1]))
  stopifnot(is.vector(f) & length(f) == length(A[1,]))
  stopifnot(is.vector(u0) & length(u0) == length(f))
  stopifnot(is.numeric(iter) & iter %% 1 == 0 & iter > 0)
  stopifnot(is.numeric(eps) & eps > 0)
  
  n <- length(f)
  
  m <- max(max(A), max(f))
  A <- A / m
  f <- f / m
  
  B <- diag(1, n, n) - A
  
  U <- matrix(0, iter + 1, n)
  U[1,] <- u0
  
  for (i in 2:(iter + 1)) {
    U[i,] <- B %*% U[i - 1,] + f
    
    if (max(abs(U[i,]  - U[i - 1,])) < eps) {
      return (U[i,])
    }
  }
  
  return (U[iter + 1,])
}