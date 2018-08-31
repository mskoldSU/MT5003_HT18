L <- function(theta, y, X){
    p <- 1 / (1 + exp(- X %*% theta))
    L <- prod(dbinom(y, 1, p))
    return(L)
}
l <- function(theta, y, X){
    p <- 1 / (1 + exp(- X %*% theta))
    l <- sum(dbinom(y, 1, p))
    return(l)
}
S <- function(theta, y, X){
    p <- 1 / (1 + exp(- X %*% theta))
    S <- t(X) %*% (y - p)
    return(S)
}
I <- function(theta, y, X){
    p <- 1 / (1 + exp(- X %*% theta))
    D <- diag(as.vector(p * (1 - p)))
    I <- t(X) %*% D %*% X
    return(I)
}
NR <- function(theta0, niter, y, X){
    theta <- theta0
    for (i in 1:niter){
        I.mat <- I(theta, y, X)
        S.vec <- S(theta, y, X)
        theta <- theta + solve(I.mat) %*% S.vec
    }
    return(theta)
}
    