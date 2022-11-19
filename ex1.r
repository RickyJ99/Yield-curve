library(matlib, help, pos = 2, lib.loc = NULL)
phi       <- 0.3

prob      <- matrix(c(phi, 1 - phi), 2, 2)
prob[, 2] <- c(1 - phi, phi)
prob
gamma  <- 2
beta    <- .96
h       <- 1.05
l       <- .97
state   <- c(h, l) ^ - gamma
yield_mcmc <- function(n, prob, state){
    out <- list()
    for (count in 1:n) {
        state <-  prob %*% state
        y <-  -(1 / count) * log(state) - log(beta)
        out <- cbind(out, y)
        prob  <- prob   %*% prob
        state <- state  %*% t(state)
    }
    return(out)
}
yield_mcmc(1, prob, state)
yield_mcmc(2, prob, state)
yield_mcmc(4, prob, state)
# .0468 h
#0.658 l

