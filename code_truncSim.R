# you should be able to indepedently sim the reporting delay to do
# what are you are doing now

set.seed(123)

sim = 1000

x_all <- lapply(0:15, function(xi) {
  data.frame(report_delay = sample(1:(length(reporting_pmf)),
                        prob = reporting_pmf,
                        replace = T,
                        size = sim),
             infection_dt = rep(xi, times = sim))
})

x_all_df <- do.call(rbind, x_all)

head(x_all_df)

library(tidyverse)

x_all_sum <- x_all_df %>%
  mutate(report_dt = infection_dt + report_delay) %>%
  group_by(report_dt, infection_dt) %>%
  summarize(
    n = n()
  )

x_all_df %>%
  mutate(report_dt = infection_dt + report_delay) %>%
  mutate(report_lt_10 = report_dt <= 10) %>%
  filter(report_lt_10) %>%
  group_by(report_lt_10, infection_dt, report_dt) %>%
  summarize(
    n = n()
  ) %>%
  pivot_wider(id_cols = c(report_dt),
              names_from = infection_dt,
              names_prefix = 'inf',
              values_from = n, values_fill = 0) %>%
  arrange(report_dt)

ggplot(x_all_sum) +
  geom_col(aes(x = report_dt, fill = factor(infection_dt), y = n),
           position = 'stack', color = 'white', show.legend = F)


x_all_sum %>%
  filter(report_dt == 10)

# so the question i want to know is,
# (after burn in)
# if I cut off on day 10

