Load required libraries
library(tidyverse)
library(incidence)
## ============================================================================

# GET THE LATEST VERSION

# install.packages('EpiEstim',
#   repos = c('https://mrc-ide.r-universe.dev',
#   'https://cloud.r-project.org'))

library(EpiEstim)
library(incidence)

#
source('code_makeData.R')
daily_reports_df = daily_reports_df
reporting_pmf    = all_data$reporting_delay$Px
serial_interval  = all_data$serial$Px
incubation_pmf   = all_data$incubation$Px

NSIM <- 100

## ============================================================================
## NOW-CASTING
## -
## ============================================================================

# Px is the percentage of reports
# its probably more like the probability that a report is reported
# but this is expected value so its fine
tail_Px <- rev(cumsum(reporting_pmf))
px_len <- length(tail_Px)

daily_reports_df$Px = 1
cases_len <- nrow(daily_reports_df)

daily_reports_df$Px[(cases_len - px_len + 1):cases_len] <- tail_Px

cutoff <- daily_reports_df$date[
  min(which(daily_reports_df$Px != 1, arr.ind = T))-1]

cutoff

head(daily_reports_df)
tail(daily_reports_df)

## reverse out the reporting truncation
daily_reports_df$daily_reports_EST <-
  daily_reports_df$x / daily_reports_df$Px

##
daily_reports_df$daily_reports_vintage_EST_diff <-
  daily_reports_df$daily_reports_EST -
  daily_reports_df$daily_reports_vintage

# plot(x = daily_reports_df$date,
#      y = daily_reports_df$daily_reports_EST)
#
# lines(x = daily_reports_df$date,
#       y = daily_reports_df$daily_reports_vintage)

add_df <- lapply(daily_reports_df$daily_reports_vintage_EST_diff,
                 function(s) {
                   if(is.na(s) | s == 0) return(rep(NA, NSIM))
                   rnbinom(NSIM, size = s/10, mu = s)
                 })
add_df_mat <- do.call(rbind, add_df)
dim(add_df_mat)

xx <- do.call(rbind, add_df)
dim(xx)
dim(daily_reports_df)

daily_reports_df2  <- cbind(daily_reports_df, xx)
head(daily_reports_df)

ggplot(daily_reports_df2) +
  geom_ribbon(aes(x = date,
                  ymin = daily_reports_vintage + q10,
                  ymax = daily_reports_vintage + q90)) +
  geom_line(aes(x = date, y = daily_reports_EST))

