# Load required libraries
library(tidyverse)

## ============================================================================

# GET THE LATEST VERSION

# install.packages('EpiEstim',
#   repos = c('https://mrc-ide.r-universe.dev',
#   'https://cloud.r-project.org'))

library(EpiEstim)
library(incidence)

## ============================================================================
## ok now include random four times

## -- offsetting reporting delay
## -- offsetting incubation time

## <NOWCASTING> removes the reporting truncation
## (1) once for the expected value of the daily
##     reports based on the reporting delay

## <FORECASTING>
## (2) and once for sampling from what this will do to potential cases if
##     R(t) stays the same in the reporting window

## ============================================================================
## NOW-CASTING
## -
## ============================================================================

source("nowcaster.R")

## ===========================================================================
### NOW DO THIS A COUPLE TIMES WITHIN THE RANGE THAT YOU ESTIMATE ABOVE
for(sim_i in 1:NSIM) {}

# get this set of simulated reports
daily_reports_df <- incidence_data_l[[sim_i]]

# ok now convert to linelist
SIM_reports <- lapply(1:nrow(daily_reports_df), function(i) {
  dt = daily_reports_df$date[i]
  N  = daily_reports_df$SIM_REPORTS[i]
  if(is.na(N)) return(NULL)
  rep(as.character(dt), times = N)
})
SIM_reports <- do.call(c, SIM_reports)
SIM_reports <- lubridate::as_date(SIM_reports)

SIM_linelist <- data.frame(SIM_report = SIM_reports)
SIM_linelist$id <- 1:nrow(SIM_linelist)

# now get rid of the reporting delay to get onset times
SIM_reporting_delays <- sample(1:(length(reporting_pmf)),
       prob = reporting_pmf,
       replace = T,
       size = length(SIM_reports))

SIM_linelist$SIM_onset <- SIM_reports - SIM_reporting_delays

# now sample an incubation time
SIM_incubation_delays <- sample(1:(length(incubation_pmf)),
                               prob = incubation_pmf,
                               replace = T,
                               size = length(SIM_reports))

SIM_linelist$SIM_infection <- SIM_onsets - SIM_incubation_delays

SIM_linelist$cutoff <- SIM_linelist$SIM_infections[SIM_linelist$SIM_infections <= cutoff]

head(SIM_linelist)

# have to simulate a branching process

# Create an incidence object:
evd_incid <- incidence(SIM_infections)
plot(evd_incid)
evd_incid$counts
dim(evd_incid)

# Plot the incidence:
# plot(evd_incid, xlab = "Date")

# Serial interval from data PMF
si_distr <- as.matrix(all_data$serial$Px)
if (all_data$serial$Day[1] == 1) si_distr <- c(0, si_distr)
si_distr

# Estimate R DAILY
getR <- EpiEstim::estimate_R(
  incid = evd_incid,
  method = "non_parametric_si",
  config = make_config(list(
    si_distr = si_distr
    # t_start = 2:nrow(incidence_df),
    # t_end = 2:nrow(incidence_df)
  ))
)

plot(getR)
tail(getR$R)
# so 60 = max(infection_dates)
ref_date <- min(infection_dates)

# subtract 1 so you can index correctly
ref_date <- ref_date - 1
ref_date

# plot(getR)

output_df <- getR$R

output_df$date_start = ref_date + output_df$t_start
output_df$date_end = ref_date + output_df$t_end

output_df2 <- output_df[, c('date_end', 'Median(R)',
                            'Quantile.0.05(R)', 'Quantile.0.95(R)')]

names(output_df2) <- c('date', 'Rt_median', 'Rt_lb', 'Rt_ub')

## change this to be the middle of the block and not the end
output_df2$date <- output_df2$date

tail(output_df)

#

head(output_df)

### SOME CHANGES
### - INCUBATION SHIFT
### - only the tail of R not the full distribution
### - pre-comptuing evd_inc
### - INSTATENTEOUS R

source("ExtendR.R")

N <- evd_incid$counts
w <- all_data$serial$Px

NSIM <- 1000
ndays <- 7 + INCUBATION_SHIFT + REPORTINGDELAY_SHIFT

probs <- c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975)
quants <- getR$R[nrow(getR$R), 5:11]

# Create inverse CDF (quantile function) using interpolation
inv_cdf <- splinefun(probs, quants, method = "monoH.FC")

# Plot to visualize
u = runif(NSIM * (ndays + 1))
R_sample <- sapply(u, inv_cdf)
R_sample <- matrix(R_sample, nrow = NSIM)

R_df <- data.frame(
  date = max(output_df2$date) + 1:ndays,
  Rt_median = apply(R_sample[,-1], 2, function(N) quantile(N, probs = 0.5)),
  Rt_lb = apply(R_sample[,-1], 2, function(N) quantile(N, probs = 0.05)),
  Rt_ub = apply(R_sample[,-1], 2, function(N) quantile(N, probs = 0.95))
)



dim(R_df)
R_df

dim(R_sample)
head(R_sample)

N_samples <- lapply(1:NSIM, function(i) {
  get_extend_N(N, R_sample[i, ], w, ndays = ndays)
})

N_samples[[1]]
N_samples_mat <- do.call(cbind, N_samples)

proj_df <- data.frame(
  date = max(output_df2$date) + 1:ndays,
  infections_median = apply(N_samples_mat, 1, function(N) quantile(N, probs = 0.5)),
  infections_lb = apply(N_samples_mat, 1, function(N) quantile(N, probs = 0.05)),
  infections_ub = apply(N_samples_mat, 1, function(N) quantile(N, probs = 0.95))
)

proj_df

#
#
# # Project future incidence over 30 days since last day in truncated incidence
# library(projections)
# proj <- project(evd_incid, # truncated incidence object
#                 R = tail(getR$R$`Median(R)`, 1), # R estimate, the last one
#                 si = getR$si_distr[-1], # SI (starting on day 1)
#                 instantaneous_R = T,
#                 n_sim = 1000,           # simulate 1000 trajectories
#                 n_days = 7 + INCUBATION_SHIFT + REPORTINGDELAY_SHIFT,             # over 7 days
#                 R_fix_within = F)       # don't the same value of R every day
#
# ## Can I extract what R is being used? this seems important
#
#
# proj_df <- summary(proj, quantiles = c(0.05, 0.5, 0.95))
# proj_df
# names(proj_df)[c(1, 7, 6, 8)] <- c('date', 'infections_median',
#                                    'infections_lb', 'infections_ub')
#
# plot(evd_incid) %>%
#   projections::add_projections(proj)

cases_df <- data.frame(
  date = evd_incid$dates,
  infections_median = evd_incid$counts,
  infections_lb = NA,
  infections_ub = NA
)

plot_data <- rbind(cases_df, proj_df)

plot_data$package = 'EpiEstim'

head(plot_data)
tail(plot_data)

output_df2 <- rbind(output_df2, R_df)
plot_data <- plot_data %>% left_join(output_df2)

head(plot_data)
tail(plot_data)

saveRDS(plot_data, "plot_objects/plot_data_EpiEstim_report.RDS")

