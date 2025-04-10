
reporting_pmf
cumsum(reporting_pmf)
px_len = length(reporting_pmf)

set.seed(123)

NDAYS <- 30

true_reports <- runif(NDAYS, min = 500, 2000)

nmax <- length(true_reports) + NDAYS
cdf_matrix <- matrix(0, nrow = nmax, ncol = NDAYS)
case_matrix <- matrix(0, nrow = nmax, ncol = NDAYS)
calc_case_matrix <- matrix(0, nrow = nmax, ncol = NDAYS)

for(i in 1:ncol(cdf_matrix)) {
  cdf_matrix[i:(i+px_len-1), i] = reporting_pmf
  case_matrix[, i] = true_reports[i] * cdf_matrix[, i]
}

case_matrix

cdf_matrix

day_reports = apply(case_matrix, 1, sum)[1:NDAYS]

# Y
day_reports <- matrix(day_reports, ncol = 1)
day_reports

# X
XMAT <- cdf_matrix[1:NDAYS, ]
XMAT

# Beta
library(MASS)
calc_day_reports = solve(t(XMAT) %*% XMAT) %*% t(XMAT) %*% day_reports
calc_day_reports
day_reports
percent_reported = rep(NA, length(calc_day_reports))
# ok and then
# what is the percent that is missing

for(i in 1:ncol(cdf_matrix)) {
  calc_case_matrix[, i] = calc_day_reports[i] * cdf_matrix[, i]
  minI = max(1, i - length(reporting_pmf))
  percent_reported[i] = sum(calc_case_matrix[1:NDAYS, i]) / calc_day_reports[i]
}
calc_case_matrix

percent_reported
rev(cumsum(reporting_pmf))
