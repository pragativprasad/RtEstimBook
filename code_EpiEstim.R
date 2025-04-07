# Load required libraries
library(tidyverse)
## ============================================================================
# install.packages('EpiEstim', repos = c('https://mrc-ide.r-universe.dev', 'https://cloud.r-project.org'))
library(EpiEstim)
library(incidence)

#### NOTES
#### -- (1)
#### -- know that this doesn't take into account the reporting delay
#### -- so pre-calculate some variability and run EpiEstim on those multiple
#### -- realizations of the data
#### -- so calculate what the Poisson mean should be and sample from that?

#### -- (2)
#### -- probably need to put the vintage delay into both of them tbh
## ============================================================================

# Load data
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

tail(all_data$cases)

NSIM <- 100

all_data$cases$daily_reports_vintage_EST <-
  all_data$cases$daily_reports_vintage / all_data$cases$Px_rev

all_data$cases$daily_reports_vintage_EST_diff <-
  all_data$cases$daily_reports_vintage_EST - all_data$cases$daily_reports_vintage

add_df <- lapply(all_data$cases$daily_reports_vintage_EST_diff, function(s) {
  if(is.na(s)) return(data.frame(q50 = NA,
                                 q10 = NA,
                                 q90 = NA))
  x <- rpois(NSIM, s)
  data.frame(q50 = quantile(x, probs = 0.5, na.rm = T),
             q10 = quantile(x, probs = 0.1, na.rm = T),
             q90 = quantile(x, probs = 0.9, na.rm = T))
})

add_df <- do.call(rbind, add_df)
add_df

all_data$cases$daily_reports_vintage_EST_q50 <- add_df$q50
all_data$cases$daily_reports_vintage_EST_q10 <- add_df$q10
all_data$cases$daily_reports_vintage_EST_q90 <- add_df$q90


ggplot(all_data$cases) +
  # geom_line(aes(x = day, y = daily_infections)) +
  # geom_line(aes(x = day, y = daily_onsets), color = 'blue') +
  geom_ribbon(aes(x = day,
                  ymin = daily_reports_vintage + daily_reports_vintage_EST_q10,
                  ymax = daily_reports_vintage + daily_reports_vintage_EST_q90),
              fill = 'pink') +
  geom_line(aes(x = day, y = daily_reports_vintage + daily_reports_vintage_EST_q50),
            color = 'red')

## ===========================================================================
### NOW DO THIS A COUPLE TIMES WITHIN THE RANGE THAT YOU ESTIMATE ABOVE

# get incidence
incidence_df <- data.frame(
  date = lubridate::make_date(2020, 3, 20) + all_data$cases$day,
  I = as.vector(all_data$cases$daily_reports) ## CHANGE THIS
)

# get a vector of report_dates
report_dates <- c()
for(i in 1:nrow(all_data$cases)) {
  if(!is.na(all_data$cases$daily_reports[i])) {
    report_dates <- c(report_dates, rep(all_data$cases$day[i],
                                        all_data$cases$daily_reports[i]))
  }
}


report_dates

# SHIFT TO GET INFECTIONS R(T)
# COULD ALSO DO THIS PROBABILITSTICALLY IF YOU REALLY WANTED TO
INCUBATION_SHIFT = round(weighted.mean(x = all_data$incubation$Day,
                                       w = all_data$incubation$Px))
INCUBATION_SHIFT

REPORTINGDELAY_SHIFT = round(weighted.mean(x = all_data$reporting_delay$Day,
                                           w = all_data$reporting_delay$Px))
REPORTINGDELAY_SHIFT

#
infection_dates <- report_dates - INCUBATION_SHIFT - REPORTINGDELAY_SHIFT

infection_dates <- lubridate::make_date(2020, 3, 20) + infection_dates
summary(infection_dates)

# Create an incidence object:
evd_incid <- incidence(infection_dates)
evd_incid$counts

# Plot the incidence:
plot(evd_incid, xlab = "Date")

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

# so 60 = max(infection_dates)
ref_date <- max(infection_dates) - nrow(all_data$rt) + 1
ref_date <- min(infection_dates)

# subtract 1 so you can index correctly
ref_date <- ref_date - 1
ref_date

plot(getR)

output_df <- getR$R

output_df$date_start = ref_date + output_df$t_start
output_df$date_end = ref_date + output_df$t_end

output_df2 <- output_df[, c('date_end', 'Median(R)',
                            'Quantile.0.05(R)', 'Quantile.0.95(R)')]
names(output_df2) <- c('date', 'Rt_median', 'Rt_lb', 'Rt_ub')

## change this to be the middle of the block and not the end
output_df2$date <- output_df2$date



#

head(output_df)

### SOME CHANGES
### - INCUBATION SHIFT
### - only the tail of R not the full distribution
### - pre-comptuing evd_inc
### - INSTATENTEOUS R

# Project future incidence over 30 days since last day in truncated incidence
library(projections)
proj <- project(evd_incid, # truncated incidence object
                R = tail(getR$R$`Median(R)`, 1), # R estimate, the last one
                si = getR$si_distr[-1], # SI (starting on day 1)
                instantaneous_R = T,
                n_sim = 1000,           # simulate 1000 trajectories
                n_days = 7 + INCUBATION_SHIFT + REPORTINGDELAY_SHIFT,             # over 7 days
                R_fix_within = F)       # don't the same value of R every day

## Can I extract what R is being used? this seems important


proj_df <- summary(proj, quantiles = c(0.05, 0.5, 0.95))
proj_df
names(proj_df)[c(1, 7, 6, 8)] <- c('date', 'infections_median',
                                   'infections_lb', 'infections_ub')

plot(evd_incid) %>%
  projections::add_projections(proj)

cases_df <- data.frame(
  date = evd_incid$dates,
  infections_median = evd_incid$counts,
  infections_lb = NA,
  infections_ub = NA
)

plot_data <- rbind(cases_df,
                  proj_df[, c(1, 7, 6, 8)])

plot_data$package = 'EpiEstim'

head(plot_data)

plot_data <- plot_data %>% left_join(output_df2)

head(plot_data)


saveRDS(plot_data, "plot_objects/plot_data_EpiEstim_report.RDS")

