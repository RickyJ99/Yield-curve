phi       <- 0.3
prob      <- matrix(c(phi, 1 - phi), 2, 2)
prob[, 2] <- c(1 - phi, phi)
prob
sigma   <- 2
beta    <- .96
h       <- 1.05
l       <- .97
state   <- c(h, l)^-sigma


yield_mcmc <- function(n, prob, state){
    out <- c()
    for (count in 1:n) {
        prob  <- prob   %*% prob
        state <- state  %*% prob
        y <- -(1 / count) * log(beta^count * (prob %*% state))
        out <- cbind(out, y)
    }
    return(out)
}
yield(1, prob, state)

#.0468 h
#0.658 l
states <- state %*% t(state)^-sigma
states
y_log      <- -0.5 * log(beta^2 * ( %*% ))
y_log
