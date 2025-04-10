# =============================================================================
# when do you need to do this
# -- to create truncations in data from a time-series of reports
# -- this works with either simulated data or real observations
# =============================================================================

library(tidyverse)
source('utility.R')

# load in simple incidence data
incidence_data <- readRDS("sim_data.RDS")
head(incidence_data)

# see that there is right truncation happening
ggplot(incidence_data) +
  geom_line(aes(x = report_date, y = confirm))

# and a reporting delay
# this is the administrative lag between when a positive test is recorded
# and when that record makes it way to the administrative dataset
reporting_pmf <- c(0.3786, 0.3724, 0.1662, 0.0622, 0.0166, 0.0034, 0.0006)

# get nowcasting data frame assuming a negative binomial distribution
# with mu = diff between estimate and actual and size = 5.
# see utility.R for details or if you want to change this distribution,
# there are other ways to do this (e.g., library(epinowcast))
nowcast_matrix <- get_nowcast(reporting_pmf, incidence_data, NSIM = 1000)

# get some credible intervals for the nowcast of each day
nowcast_quantiles <- apply(nowcast_matrix, 1, function(x) {
  quantile(x, probs = c(0.025, 0.5, 0.975))
})
nowcast_df <- data.frame(t(nowcast_quantiles))
names(nowcast_df) <- paste0('now_', c('lb', 'med', 'ub'))

# join by date
nowcast_df$report_date = incidence_data$report_date
incidence_data_updated <- left_join(incidence_data, nowcast_df)
head(incidence_data_updated)

# see now that you have a distribution of nowcasted variables
ggplot(incidence_data_updated) +
  geom_ribbon(aes(x = report_date, ymin = confirm + now_lb,
                  ymax = confirm + now_ub), fill =  'lightblue') +
  geom_line(aes(x = report_date, y = confirm + now_med), color =  'blue',
            linetype = '11') +
  geom_line(aes(x = report_date, y = confirm))

# for our work, we want a dataset now for each realization of this
incidence_data_l <- lapply(1:ncol(nowcast_matrix), function(i) {
  df <- cbind(incidence_data, nowcast_matrix[, i])
  names(df)[ncol(df)] <- 'new_confirm'
  df$new_confirm = df$confirm + df$new_confirm
  df
})

incidence_data_l[[1]]
