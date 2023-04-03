#' Run Bootstrap
#'
#' @param bq_data data retrieved from big query
#' @param num_var numeric column with values to bootstrap
#' @param group_var character column identifying  variants
#' @param b_samples numeric indicating the number of bootstrap samples
#' @param seed numeric - optional
#'
#' @return
#'
run_bootstrap <- function(bq_data, exp_variants, num_var, group_var, b_samples, seed = NULL) {

  if(!is.null(seed)){
    set.seed(seed)
  }

  df <- bq_data %>%
    dplyr::select(!!group_var, !!num_var) %>%
    dplyr::arrange(dplyr::across(1)) %>%
    as.data.frame()

  levels(df[, group_var]) <- c(exp_variants[1], exp_variants[2])

  #  diff in means
  mean_variants <- with(df, tapply(df[, num_var], df[, group_var], mean))
  mean_diff <- mean_variants[2] - mean_variants[1]
  diff_mean <- (mean_diff)

  #  diff in medians
  median_variants <- with(df, tapply(df[, num_var], df[, group_var], median))
  median_diff <- median_variants[2] - median_variants[1]
  diff_median <- (median_diff)

  ###########################################

  # Independent 2-sample t-test - Ho: means are equal
  # t_test <- t.test(df[, num_var] ~ df[, group_var], paired = FALSE, var.eq = FALSE)

  t_test_form <- as.formula(paste0(num_var,"~", group_var))
  t_test_pub <- rstatix::t_test(df, t_test_form) # neatly format t-test results

  # Wilcoxon - Ho: medians are equal
  wilcox_test <- wilcox.test(df[, num_var] ~ df[, group_var], paired = FALSE)

  # Kolmogorov-Smirnov 2-sample test - Ho: distributions are same
  ks_test <- ks.test(df[, num_var][df[, group_var] == exp_variants[1]], df[, num_var][df[, group_var] == exp_variants[2]])


  #############################  Bootstrapping


  n <- length(df[, group_var])  # number of observations to sample
  resample_var <- df[, num_var]  # variable to resample from

  BootstrapSamples <- matrix(
    sample(resample_var, size = n*b_samples, replace = TRUE),
    nrow = n, ncol = b_samples)

  # initialize the vector to store the test-stats
  boot_diff_mean <- rep(0, b_samples)
  boot_diff_median <- rep(0, b_samples)

  obs <- table(df[, group_var])
  length_var1 <- obs[[1]]
  length_var2 <- obs[[2]]
  total_length <- length_var1 + length_var2

  for (i in 1:b_samples) {
    boot_diff_mean[i] <- (mean(BootstrapSamples[1:length_var1, i]) - mean(BootstrapSamples[(length_var1+1):total_length, i]))
    boot_diff_median[i] <- (median(BootstrapSamples[1:length_var1, i]) - median(BootstrapSamples[(length_var1+1):total_length, i]))
  }

  # bootstrap p-value
  p_val_diff_mean <- mean(boot_diff_mean >= diff_mean)
  p_val_diff_median <- mean(boot_diff_median >= diff_median)

  boot_df <- data.frame("boot_diff_mean" = boot_diff_mean,
                                  "boot_diff_median" = boot_diff_median)

  result_list <- list (data = df,
                       diff_mean = diff_mean,
                       diff_median = diff_median,
                       bootstrapped_data = boot_df,
                       t_results = t_test_pub,
                       wilcox_results = wilcox_test,
                       ks_results = ks_test)

  result <- tibble::enframe(result_list)
}
