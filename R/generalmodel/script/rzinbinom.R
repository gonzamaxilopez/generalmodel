rzinbinom <- function (n,k,m,zi.prob) {
  Y <- rnbinom(n,mu=m,size=k)
  U <- sample(c(0, 1), size = n, prob = c(zi.prob, 1-zi.prob), replace = TRUE)
  U*Y 
  }