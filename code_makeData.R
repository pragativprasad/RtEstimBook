
library(lubridate)
library(tidyverse)

url <- "https://raw.githubusercontent.com/cmilando/RtEval/main/all_data.RDS"
all_data <- readRDS(url(url))
reporting_pmf <- all_data$reporting_delay$Px

#
all_data$cases <- all_data$cases[1:45, c('day', 'daily_reports')]
head(all_data$cases)
tail(all_data$cases)

# reference date
ref_date <- make_date(2020, 3, 20)
all_data$cases$date <- all_data$cases$day + ref_date
head(all_data$cases)

## impose vintages
tail_Px <- rev(cumsum(reporting_pmf))
px_len <- length(tail_Px)

all_data$cases$Px = 1
cases_len <- nrow(all_data$cases)

all_data$cases$Px[(cases_len - px_len + 1):cases_len] <- tail_Px
tail(all_data$cases$Px)

all_data$cases$daily_reports_vintage <-
  all_data$cases$daily_reports * all_data$cases$Px

head(all_data$cases)
tail(all_data$cases)

##
daily_reports_df <- all_data$cases[2:nrow(all_data$cases),
                                   c('date', 'daily_reports_vintage')]
names(daily_reports_df) = c('report_date', 'confirm')

head(daily_reports_df)
tail(daily_reports_df)

saveRDS(daily_reports_df, 'sim_data.RDS')
