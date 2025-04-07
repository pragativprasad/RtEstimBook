project_local <- function (x, R, si, n_sim = 100, n_days = 7, R_fix_within = FALSE,
          model = c("poisson", "negbin"), size = 0.03, time_change = NULL,
          instantaneous_R = FALSE)
{

  ## ****
  x = evd_incid # truncated incidence object
  R = getR$R$`Median(R)` # R estimate
  si = getR$si_distr[-1] # SI (starting on day 1)
  instantaneous_R = T
  n_sim = 1000           # simulate 1000 trajectories
  n_days = 7 + INCUBATION_SHIFT + REPORTINGDELAY_SHIFT             # over 7 days
  R_fix_within = F
  ## ****
  model = c("poisson")
  time_change = NULL

  n_time_periods <- 1
  if (!is.null(time_change)) {
    if (!is.numeric(time_change)) {
      msg <- sprintf("`time_change` must be `numeric`, but is a `%s`",
                     paste(class(time_change), collapse = ", "))
      stop(msg)
    }
    n_time_periods <- length(time_change) + 1
    if (!is.vector(R)) {
      msg <- sprintf("`R` must be a `vector` or a `list` if `time_change` provided; it is a `%s`",
                     paste(class(R), collapse = ", "))
      stop(msg)
    }
    if (length(R) != n_time_periods) {
      msg <- sprintf("`R` must be a `list` of size %d to match %d time changes; found %d",
                     n_time_periods, n_time_periods - 1, length(R))
      stop(msg)
    }
  }

  assert_R <- function(x) {
    if (is.list(x)) {
      x <- unlist(x)
    }
    if (!is.numeric(x)) stop("R is not numeric")
    if (!all(is.finite(x))) stop("R is not a finite value")
    if (any(x < 0)) stop(sprintf("R < 0 (value: %.2f)", x[x<0]))
  }

  assert_R(R)
  n_dates_x <- nrow(incidence::get_counts(x))
  t_max <- n_days + n_dates_x - 1

  if (inherits(si, "distcrete")) {
    if (as.integer(si$interval) != 1L) {
      msg <- sprintf("interval used in si is not 1 day, but %d)",
                     si$interval)
      stop(msg)
    }
    si <- si$d(1:t_max)
    si <- si/sum(si)
  } else {
    if (si[1] == 0) {
      msg1 <- "si[1] is 0. Did you accidentally input the serial interval"
      msg2 <- "distribution starting at time 0 instead of 1? If so, rerun with"
      msg3 <- "a new si where si[1] is the PMF for serial interval of 1."
      warning(paste(msg1, msg2, msg3))
    }
    si <- si/sum(si)
    si <- c(si, rep(0, t_max - 1))
  }

  if (is.null(time_change)) {
    time_change <- Inf
  }

  I0 <- matrix(incidence::get_counts(x), nrow = n_dates_x,
               ncol = n_sim)
  out <- I0
  t_start <- n_dates_x + 1
  t_stop <- t_max + 1
  t_sim <- seq(from = t_start - 1, to = t_stop - 1, by = 1L)
  time_change <- t_start + time_change - 1
  if (!is.list(R)) {
    if (n_time_periods > 1L) {
      R <- as.list(R)
    } else {
      R <- list(R)
    }
  }

  if (all(is.finite(time_change))) {
    time_change_boundaries <- c(1, time_change, t_stop +
                                  1)
  } else {
    time_change_boundaries <- c(1, t_stop + 1)
  }

  R_t <- matrix(nrow = 0, ncol = n_sim)

  sample_ <- function(x, ...) {
    x[sample.int(length(x), ...)]
  }

  if (R_fix_within) {
    for (time_period in 1:n_time_periods) {
      R_time_period <- sample_(R[[time_period]], n_sim,
                               replace = TRUE)
      period_duration <- time_change_boundaries[time_period +
                                                  1] - time_change_boundaries[time_period]
      current_R_t <- do.call("rbind", replicate(period_duration,
                                                R_time_period, simplify = FALSE))
      R_t <- rbind(R_t, current_R_t)
    }
  } else {
    time_period <- 1L
    for (i in 1:t_stop) {
      R_time_period <- R[[time_period]]
      current_R_t <- sample_(R_time_period, n_sim, replace = TRUE)
      R_t <- rbind(R_t, current_R_t)
      if (i %in% time_change) {
        time_period <- time_period + 1
      }
    }
  }

  dim(R_t)
  summary(R_t[1, ])
  summary(R_t[50, ])

  rownames(R_t) <- NULL
  for (i in t_sim) {
    lambda <- compute_force_infection(si, out, R_t, i, instantaneous_R)
    if (model == "poisson") {
      out <- rbind(out, stats::rpois(n_sim, lambda))
    }
    else {
      size_adj <- compute_relative_infectivity(w = si,
                                               cases = out, t = i) * size
      idx <- which(lambda == 0)
      size_adj[idx] <- 1
      out <- rbind(out, stats::rnbinom(n_sim, size = size_adj,
                                       mu = lambda))
    }
  }
  out <- out[(n_dates_x + 1):(n_dates_x + n_days), , drop = FALSE]
  dates <- utils::tail(get_dates(x), 1) + seq_len(nrow(out))
  build_projections(out, dates, FALSE)
}
