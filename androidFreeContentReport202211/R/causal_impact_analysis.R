pre_p_start <- as.Date('2022-11-09') # pre-period start point
# start_dt is the experiment start
# end_dt is the analysis period end
# end_alloc_dt is the end of allocating new users to the experiment

reqd_export_data <- bq_exports_data %>%
  dplyr::filter(user_type == 'new') %>%
  dplyr::arrange(date) %>%
  dplyr::filter(date >= pre_p_start & date <= end_dt) %>%
  dplyr::select(date, exports_avg, users)

reqd_export_data_aa <- bq_exports_data %>%
  dplyr::filter(user_type == 'new') %>%
  dplyr::arrange(date) %>%
  dplyr::filter(date >= pre_p_start & date <= start_dt) %>%
  dplyr::select(date, exports_avg, users)

exports_xts <- xts::as.xts(reqd_export_data)
exports_xts_aa <- xts::as.xts(reqd_export_data_aa)

post_p <- as.numeric(difftime(end_dt, as.Date(start_dt)+1))
pre_p <-  as.numeric(difftime(start_dt, pre_p_start))
p_ratio <- post_p / (post_p + pre_p)

# Set pre-period
pre_period <- c(pre_p_start, as.Date(start_dt)-1)
pre_period_ind <- c(which(zoo::index(exports_xts) == pre_period[1]), which(zoo::index(exports_xts) == pre_period[2]))

pre_period_ind_aa <- c(which(zoo::index(exports_xts) == pre_period[1]), which(zoo::index(exports_xts) == pre_period[2]) * (1 - p_ratio))
pre_period_aa <- zoo::index(exports_xts[pre_period_ind_aa])

# Set post-period
post_period <- c(as.Date(start_dt), as.Date(end_dt))
post_period_ind <- c(which(zoo::index(exports_xts) == post_period[1]), which(zoo::index(exports_xts) == post_period[2]))

post_period_ind_aa <- c(which(zoo::index(exports_xts) == pre_period[2]) * (1 - p_ratio) + 1, which(zoo::index(exports_xts) == post_period[1]))
post_period_aa <- zoo::index(exports_xts[post_period_ind_aa])

# Calculate the pre-daily average
pre_daily_avg <- mean(exports_xts$exports_pct[pre_period_ind[1]:pre_period_ind[2]])
# Calculate the post-daily average
post_daily_avg <- mean(exports_xts$exports_pct[post_period_ind[1]:post_period_ind[2]])
# Pre-post difference
pre_post_diff <- post_daily_avg - pre_daily_avg

# Causal impact model
impact_aa <- CausalImpact::CausalImpact(data = exports_xts_aa, pre.period = pre_period_aa, post.period = post_period_aa)
impact <- CausalImpact::CausalImpact(data = exports_xts, pre.period = pre_period, post.period = post_period)


# Visualization
ggplotify::as.ggplot(plot(impact_aa))
ggplotify::as.ggplot(plot(impact))

# aa_args <- list(niter = 1000, nseasons = 7, prior.level.sd = 0.01)
#
# reqd_export_data_2 <- bq_exports_data %>%
#   dplyr::filter(user_type == 'new') %>%
#   dplyr::arrange(date) %>%
#   dplyr::filter(date >= pre_p_start & date <= end_dt) %>%
#   dplyr::mutate(obs_no = as.numeric(rownames(.))) %>%
#   tibble::column_to_rownames(var = 'date') %>%
#   dplyr::select(obs_no, exports_avg, users)
#
# pre_period_ind_2 <- c(which(rownames(reqd_export_data_2) == pre_p_start), which(rownames(reqd_export_data_2) == start_dt))
#
# ci_aa <- CausalImpactHelper::RunAATest(full_dataset = reqd_export_data_2, pre_period = pre_period_ind_2, causalimpact_args = aa_args)
# plot(ci_aa$aa_causalimpact_model)
#
# cointegration_test <- CausalImpactHelper::RunCointegrationTest(reqd_export_data_2, pre_period_ind_2, "exports_avg", run_stationarity_on_controls = TRUE)
# knitr::kable(cointegration_test$test_results)
# plotly::subplot(cointegration_test$cointegration_residual_graphs)
