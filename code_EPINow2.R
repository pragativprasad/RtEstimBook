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

url <- "https://raw.githubusercontent.com/cmilando/RtEval/main/all_data.RDS"
all_data <- readRDS(url(url))

#
all_data$cases <- all_data$cases[1:45, ]

## impose vintages
MAX_DAY <- max(all_data$cases$day)
ReportPX <- all_data$reporting_delay %>% arrange(-Day)
names(ReportPX)[1] <- 'rev_day'
all_data$cases$rev_day <- (nrow(all_data$cases) - 1):0
all_data$cases <- left_join(all_data$cases, ReportPX)
all_data$cases$Px[which(is.na(all_data$cases$Px))] <- 0
all_data$cases$Px_rev <- 1 - all_data$cases$Px
all_data$cases$daily_reports_vintage <-
  all_data$cases$daily_reports * all_data$cases$Px_rev

# columsn are called `date` and `confirm`
incidence_df = data.frame(
  date = lubridate::make_date(2020, 3, 20) + all_data$cases$day,
  confirm = as.integer(as.vector(all_data$cases$daily_reports_vintage)))

dim(incidence_df)

####
# does this have to start with 0? i think so
gi_pmf <- NonParametric(pmf = c(0, all_data$serial$Px))
gi_pmf
###

sym_report_delay_pmf <- NonParametric(pmf = all_data$reporting_delay$Px)

sym_report_delay_pmf

incubation_pmf <- NonParametric(pmf = all_data$incubation$Px)

# ----------------------------------------------------------------
# WOW THIS TAKES A LONG TIME ~ 10min for 60 day example

res_epinow <- epinow(
  data = incidence_df,
  generation_time = generation_time_opts(gi_pmf),
  delays = delay_opts(incubation_pmf + sym_report_delay_pmf),
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
