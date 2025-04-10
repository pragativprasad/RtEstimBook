get_case_gen_matrix <- function(reporting_pmf, day_reports) {

  NDAYS <- length(day_reports)
  nmax  <- NDAYS + length(reporting_pmf)

  cdf_matrix <- matrix(0, nrow = nmax, ncol = NDAYS)
  calc_case_matrix <- matrix(0, nrow = nmax, ncol = NDAYS)

  for(i in 1:ncol(cdf_matrix)) {
    cdf_matrix[i:(i+px_len-1), i] = reporting_pmf
  }

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

  # ok and then
  # what is the percent that is missing

  for(i in 1:ncol(cdf_matrix)) {
    calc_case_matrix[, i] = calc_day_reports[i] * cdf_matrix[, i]
  }

  calc_case_matrix
  return(calc_case_matrix)
  # colnames(calc_case_matrix)

  # Your matrix
  # mat = calc_case_matrix
  # colnames(mat) <- paste0("col", 1:ncol(mat))
  # rownames(mat) <- paste0("row", 1:nrow(mat))

  # Convert to long format
  # library(tidyverse)
  #
  # long_df <- mat %>%
  #   as.data.frame() %>%
  #   rownames_to_column("row") %>%
  #   pivot_longer(-row, names_to = "col", values_to = "value") %>%
  #   mutate(
  #     row_num = as.integer(sub("row", "", row)),
  #     col_num = match(col, colnames(mat))
  #   ) %>%
  #   filter(row_num >= col_num, value > 0) %>% arrange(col_num, row_num) %>%
  #   dplyr::select(row_num, col_num, value) %>%
  #   rename(dt = row_num,
  #          gen_i = col_num)
  #
  # return(long_df)

}
