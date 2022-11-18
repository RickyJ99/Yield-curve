phi       <- 0.3

prob      <- matrix(c(phi, 1 - phi), 2, 2)
prob[, 2] <- c(1 - phi, phi)
prob
sigma   <- 2
beta    <- .96
h       <- 1.05
l       <- .97
state   <- c(h, l)
yield_mcmc <- function(n, prob, state){
    out <- list()
    for (count in 1:n) {
        state <-  prob %*% state
        y <- -(1 / count) * log(beta^count) +
        log((state %*% c(0.3, .7)) ^-sigma)
        out <- cbind(out, y)
        prob  <- prob   %*% prob
        state <- state  %*% t(state)
    }
    return(out)
}
yield_mcmc(1, prob, state)
yield_mcmc(2, prob, state)
# .0468 h
#0.658 l

