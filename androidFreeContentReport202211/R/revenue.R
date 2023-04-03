exp_start_dt <- '2023-01-09'

conv_pct <- bq_revenue_data %>%
  dplyr::filter(date < exp_start_dt) %>%
  dplyr::mutate(conv_pct_vec = actual_annual_conv / new_users) %>%
  dplyr::summarise(avg = mean(conv_pct_vec, na.rm = TRUE)) %$%
  avg

sub_pct <- bq_revenue_data %>%
  dplyr::filter(date < exp_start_dt) %>%
  dplyr::mutate(sub_pct_vec = actual_monthly_subs / new_users) %>%
  dplyr::summarise(avg = mean(sub_pct_vec, na.rm = TRUE)) %$%
  avg

monthly_mrr_sub <- bq_revenue_data %>%
  dplyr::filter(date < exp_start_dt) %>%
  dplyr::mutate(mrr_sub = mrr_monthly / subs_monthly) %>%
  dplyr::summarise(avg = mean(mrr_sub, na.rm = TRUE)) %$%
  avg

annual_mrr_sub <- bq_revenue_data %>%
  dplyr::filter(date < exp_start_dt) %>%
  dplyr::mutate(mrr_sub = mrr_annual / subs_annual) %>%
  dplyr::summarise(avg = mean(mrr_sub, na.rm = TRUE)) %$%
  avg

conv_rate <- bq_revenue_data %>%
  dplyr::filter(date < exp_start_dt) %>%
  dplyr::summarise(avg = mean(conversion_rate, na.rm = TRUE)) %$%
  avg

extrapolated_subs <- bq_revenue_data %>%
  dplyr::select(date, new_users) %>%
  dplyr::mutate(extrap_monthly_subs = new_users * sub_pct,
                extrap_monthly_mrr = extrap_monthly_subs * monthly_mrr_sub,
                extrap_annual_subs = new_users * conv_pct * conv_rate,
                extrap_annual_mrr = extrap_annual_subs * annual_mrr_sub)

mrr_tbl <- bq_revenue_data %>%
  dplyr::select(date, actual_monthly_subs, actual_annual_conv) %>%
  dplyr::mutate(monthly_mrr = actual_monthly_subs * monthly_mrr_sub,
                annual_mrr = actual_annual_conv * annual_mrr_sub)

exp_tbl <- bq_revenue_data %>%
  dplyr::select(date, dplyr::contains("exp"))

monthly_revenue_tbl <- extrapolated_subs %>%
  dplyr::left_join(mrr_tbl, by = "date") %>%
  dplyr::left_join(exp_tbl, by = "date") %>%
  tidyr::pivot_longer(
    cols = tidyr::contains("monthly"),
    names_to = "variable",
    values_to = "value",
    values_drop_na = TRUE
  ) %>%
  dplyr::select(date, new_users, variable, value) %>%
  dplyr::mutate(freq = "monthly")

annual_revenue_tbl <- extrapolated_subs %>%
  dplyr::left_join(mrr_tbl, by = "date") %>%
  dplyr::left_join(exp_tbl, by = "date") %>%
  tidyr::pivot_longer(
    cols = tidyr::contains("annual"),
    names_to = "variable",
    values_to = "value",
    values_drop_na = TRUE
  ) %>%
  dplyr::select(date, new_users, variable, value) %>%
  dplyr::mutate(freq = "annual")

potential_revenue_monthly <- monthly_revenue_tbl %>%
  dplyr::filter(freq == "monthly") %>%
  dplyr::bind_rows(annual_revenue_tbl) %>%
  dplyr::mutate(variable = stringr::str_remove_all(variable, "monthly_")) %>%
  tidyr::pivot_wider(names_from = variable, values_from = value) %>%
  dplyr::summarise(potential_subscribers = sum(actual_subs - extrap_subs, na.rm = TRUE),
                   potential_mrr = sum(mrr - extrap_mrr, na.rm = TRUE)
                   ) %>%
  dplyr::mutate(period = "monthly")

potential_revenue_annual <- monthly_revenue_tbl %>%
  dplyr::filter(freq == "annual") %>%
  dplyr::bind_rows(annual_revenue_tbl) %>%
  dplyr::mutate(variable = stringr::str_remove_all(variable, "annual_")) %>%
  tidyr::pivot_wider(names_from = variable, values_from = value) %>%
  dplyr::summarise(potential_subscribers = sum(actual_conv - extrap_subs, na.rm = TRUE),
                   potential_mrr = sum(mrr - extrap_mrr, na.rm = TRUE)
) %>%
  dplyr::mutate(period = "annual")

potential_revenue_tbl <- potential_revenue_monthly %>%
  dplyr::bind_rows(potential_revenue_annual) %>%
  dplyr::select(period, potential_subscribers, potential_mrr)

exp_revenue_monthly <- exp_tbl %>%
  dplyr::select(date, exp_subs_monthly, exp_monthly_subs, exp_mrr_monthly) %>%
  dplyr::mutate(exp_mrr_sub = exp_mrr_monthly / exp_subs_monthly) %>%
  dplyr::mutate(exp_mrr = exp_monthly_subs * exp_mrr_sub) %>%
  dplyr::summarise(realised_subscribers = sum(exp_monthly_subs, na.rm = TRUE),
                   realised_mrr = sum(exp_mrr, na.rm = TRUE)
  ) %>%
  dplyr::mutate(period = "monthly")

exp_revenue_annual <- exp_tbl %>%
  dplyr::select(date, exp_subs_annual, exp_annual_conv, exp_mrr_annual) %>%
  dplyr::mutate(exp_mrr_sub = exp_mrr_annual / exp_subs_annual) %>%
  dplyr::mutate(exp_mrr = exp_annual_conv * exp_mrr_sub) %>%
  dplyr::summarise(realised_subscribers = sum(exp_annual_conv, na.rm = TRUE),
                   realised_mrr = sum(exp_mrr, na.rm = TRUE)
  ) %>%
  dplyr::mutate(period = "annual")

realised_revenue_tbl <- exp_revenue_monthly %>%
  dplyr::bind_rows(exp_revenue_annual) %>%
  dplyr::select(period, realised_subscribers, realised_mrr)

revenue_tbl <- potential_revenue_tbl %>%
  dplyr::left_join(realised_revenue_tbl, by = "period") %>%
  dplyr::select(period, realised_subscribers, potential_subscribers, realised_mrr, potential_mrr) %>%
  dplyr::mutate(diff_subs = realised_subscribers / potential_subscribers - 1,
                diff_mrr = realised_mrr / potential_mrr - 1)
