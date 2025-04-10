library(tidyverse)

## ============================================================================
###
## FOR EXAMPLE - the basic epinow doesn't have a warning for a generation interval
## that starts with 0
## only the DEV version has this update
## this has a huge impact on results
# install.packages("EpiNow2",
#                  repos = c("https://epiforecasts.r-universe.dev",
#                            getOption("repos")))
library(EpiNow2)
##


## weird things (1) see generation interval thing above

## and (2)
## it also assumes that the tail has incomplete data ...
## or maybe not?

## ============================================================================

####
# does this have to start with 0? i think so
generation_interval <- c(
  0.0610, 0.1540, 0.2198, 0.2178, 0.1536, 0.1122, 0.0486, 0.0224,
  0.0078, 0.0022, 0.0004, 0.0002
)
gi_pmf <- NonParametric(pmf = c(0, generation_interval))
gi_pmf
###

# Delays
# - time from taking a test to it getting into a state database
reporting_pmf <- c(0.3786, 0.3724, 0.1662, 0.0622, 0.0166, 0.0034, 0.0006)
sym_report_delay_pmf <- NonParametric(pmf = reporting_pmf)

sym_report_delay_pmf

# Incidence to taking a test
# sum of: infect to symtpom onset + symptom onset to taking a test
# for this, assuming symptom onset to taking a test = 0
infect_to_test_pmf <- c(
  0.1422, 0.2714, 0.2664, 0.1832, 0.0846, 0.0340, 0.0136, 0.0030,
  0.0014, 0.0002
)
infect_to_test <- NonParametric(pmf = infect_to_test_pmf)
infect_to_test

# ----------------------------------------------------------------
incidence_data$confirm <- as.integer(incidence_data$confirm)
incidence_data$date <- as.Date(incidence_data$report_date)

res_epinow <- epinow(
  data = incidence_data,
  generation_time = generation_time_opts(gi_pmf),
  delays = delay_opts(infect_to_test),
  truncation = trunc_opts(),
  #rt = rt_opts(rw = 1),
  backcalc = backcalc_opts(prior = 'reports'),
  stan = stan_opts(chains = 4, cores = 4)
)
# ----------------------------------------------------------------

plot(res_epinow)
# huh zoomed in this doesn't look that good

# stan_matrix <- rstan::extract(res_epinow$estimates$fit)
#
# R_matrix <- stan_matrix$R
# dim(R_matrix)

## WHEN IS THIS START DATE
infections_matrix <- res_epinow$estimates$summarised %>%
  filter(variable == 'infections')

##
Rt_est <- res_epinow$estimates$summarised %>%
  filter(variable == 'R')

##
plot_data <- data.frame(
  package = "EpiNow2",
  # date_num = 0:(nrow(Rt_est) - 1),
  date = Rt_est$date,
  ##
  Rt_median = Rt_est$median,
  Rt_lb = Rt_est$lower_90,
  Rt_ub = Rt_est$upper_90,
  ##
  ## daily_reports = c(all_data$cases$daily_reports, rep(NA, 7)),
  infections_median = infections_matrix$median,
  infections_lb = infections_matrix$lower_90,
  infections_ub = infections_matrix$upper_90
  )

head(plot_data, 10)
tail(plot_data, 10)

saveRDS(plot_data, "plot_objects/plot_data_EpiNow2_report.RDS")
