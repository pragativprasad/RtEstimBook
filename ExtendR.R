## code to prepare ALL datasets goes here

get_extend_N <- function(N, Rsample, w, ndays = 7) {

  tend <- length(N)

  Rmatrix = c(rep(NA, tend - 1), Rsample)
  tmax <- length(Rmatrix)

  N <- rbind(N, matrix(NA, nrow = ndays))

  stopifnot(length(N) == length(Rmatrix))

  # initialize
  S <- length(w)

  for(tt in (tend+1):tmax) {

    # what are the boundaries of tau
    tau_end = min(S, tt - 1)

    RR <- Rmatrix[tt]
    RR

    ## MM is m(t - 1), ..., m(1)
    ## where rows are regions
    ## and columns are time
    MM <- t(as.matrix(N[tt - 1:tau_end]))
    MM

    WW <-  as.matrix(w[1:tau_end])
    WW

    inner_vec <- RR %*% MM %*% WW
    inner_vec

    N[tt] = rpois(1, inner_vec)
    N[tt]
  }

  # plot(N)

  return(tail(N, ndays))

}





