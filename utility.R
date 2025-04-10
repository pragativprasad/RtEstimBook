get_nowcast <- function(reporting_pmf,
                               incidence_df,
                               NSIM = 100) {

  ## reverse the repording PMF to get the amount that will be truncated
  tail_Px <- rev(cumsum(reporting_pmf))
  px_len <- length(tail_Px)

  incidence_df$Px = 1
  cases_len <- nrow(incidence_df)

  incidence_df$Px[(cases_len - px_len + 1):cases_len] <- tail_Px

  ## estimate the expected value reporting delay
  ## by getting the estimate and subtracting from the known cases
  incidence_df$confirm_EST <-
    incidence_df$confirm / incidence_df$Px

  ##
  incidence_df$confirm_EST_diff <-
    incidence_df$confirm_EST -
    incidence_df$confirm

  ## if starting with phi and lambda
  ## then using mu and size
  ##  -- mu = lambda, size = mu / (phi - 1)
  ## if using size and prob
  ##  -- size = mu / (phi - 1), prob = 1 / phi

  nowcast_l <- lapply(incidence_df$confirm_EST_diff,
                   function(s) {
                     if(is.na(s) | s == 0) return(rep(0, NSIM))
                     rnbinom(NSIM, size = 5, mu = s)

                     #rpois(NSIM, lambda = s)
                   })

  ## return as a matrix
  nowcast_matrix <- do.call(rbind, nowcast_l)

  return(nowcast_matrix)
}
