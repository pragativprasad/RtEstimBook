
library(tidyverse)

url <- "https://raw.githubusercontent.com/cmilando/RtEval/main/all_data.RDS"
all_data <- readRDS(url(url))

plot_data <- rbind(
  readRDS("plot_objects/plot_data_EpiEstim_report.RDS"),
  readRDS("plot_objects/plot_data_EpiNow2_report.RDS")
)

# ----------------------------------------------------------------
# R(t)



p1 <- as_tibble(plot_data) %>%
  ggplot() +
  geom_hline(yintercept = 1, linetype = "11") +
  coord_cartesian(xlim = c(
    make_date(2020,3,15), make_date(2020,5,20)
  ), ylim = c(0, 5)) +
  # *******
  # this is the true r(t), back-calculated
  # geom_line(aes(x = date, y = Rt_calc)) +
  # *******
  geom_ribbon(aes(x = date, ymin = Rt_lb, ymax = Rt_ub, fill = package),
              alpha = 0.25) +
  geom_line(aes(x = date, y = Rt_median, color = package)) +
  geom_point(aes(x = date, y = Rt_median, color = package),
             size = 0.5) +
  xlab("Day") +
  ylab("Rt") +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14)
  )

# ----------------------------------------------------------------

## HMM SO THIS GIVES AN ESTIMATE OF CASES ON DAY 0

p2 <- as_tibble(plot_data) %>%
  ggplot() +
  # *******
  # this is the true r(t), back-calculated
  geom_col(data = all_data$cases,
           mapping = aes(x = day + lubridate::make_date(2020,3,20),
               y = daily_infections), fill = 'grey', color = NA) +
  # *******
  geom_ribbon(aes(x = date, ymin = infections_lb,
                  ymax = infections_ub, fill = package),
              alpha = 0.25) +
  geom_line(aes(x = date, y = infections_median, color = package)) +
  geom_point(aes(x = date, y = infections_median, color = package),
             size = 0.5) +
  coord_cartesian(xlim = c(
    make_date(2020,3,15), make_date(2020,5,20)
  )) +
  xlab("Day") +
  ylab("Infections") +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14)
  )


# ----------------------------------------------------------------
library(patchwork)
p1 / p2
# ----------------------------------------------------------------
