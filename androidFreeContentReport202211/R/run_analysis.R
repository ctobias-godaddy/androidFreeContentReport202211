# sql params
sql_script_path <- 'experiment_retention_script.sql'

platform <- 'android'
exp_name <- 'android-customer-202211-free-content'
exp_variants <- c('control', 'treatment-7')
effect_type <- 'observed_effect' # observed_effect or true_effect where true_effect only includes users in their first session (not just new users)

start_dt <- '2022-01-09'
end_dt <- Sys.Date()

# bootstrap params
seed = 123 # set seed for reproducibility with sampling
b <- 10000  # number of bootstrap samples
num_var <- 'pct_users_retained' # numeric variable containing the values to test
group_var <- 'variant_name' # grouping variable - used to identify the variants we wish to test

# plot params
density_plot_ttl = "Bootstrapped Difference in Retention"
html_ttl = paste0("Android Free Content Experiment: Retention Testing - 7day vs Control: ", Sys.Date())

source("read_sql.R")
source("run_bootstrap.R")
source("plot_funcs.R")

# --------------------------------------------------------------------------------------------------------------

# Get data from BQ
bq_tbl <- read_sql(sql_script_path, exp_name, exp_variants, platform, start_dt, end_dt, effect_type)
bq_data <- bq_tbl$data[[1]]

bq_data_set1 <- dplyr::filter(bq_data, variant_name %in% c(exp_variants[1], exp_variants[2]))
# bq_data_set2 <- dplyr::filter(bq_data, variant_name %in% c(exp_variants[1], exp_variants[3]))

# Run bootstrap
bootstrap_data_set1 <- run_bootstrap(bq_data_set1, exp_variants, num_var, group_var, b_samples = b, seed = seed)
# bootstrap_data_set2 <- run_bootstrap(bq_data_set2, exp_variants, num_var, group_var, b_samples = b, seed = seed)

# -----------------------------------------  Plot
retention_curve_data <- bq_data_set1 %>%
  dplyr::select(variant_name, nday, pct_users_retained)

retention_curves <- plot_retention_curves(retention_curve_data)

retention_diff_data <- retention_curve_data %>%
  tidyr::pivot_wider(names_from = variant_name, values_from = pct_users_retained) %>%
  dplyr::mutate(absolute_diff = !!dplyr::sym(exp_variants[2]) - !!dplyr::sym(exp_variants[1])) %>%
  dplyr::mutate(relative_diff = (!!dplyr::sym(exp_variants[2]) / !!dplyr::sym(exp_variants[1]) - 1) * 100) %>%
  dplyr::select(!dplyr::all_of(exp_variants)) %>%
  tidyr::pivot_longer(!nday, names_to = "diff_type", values_to = "diff_value")

diff_curves <- plot_diff_data(retention_diff_data)

boxplot_data <- dplyr::filter(bootstrap_data_set1, name == "data") %>% tidyr::unnest(value)
box_plot <- plot_box(boxplot_data)

densityplot_data <- dplyr::filter(bootstrap_data_set1, name == "bootstrapped_data") %>% tidyr::unnest(value)
diff_mean <- dplyr::filter(bootstrap_data_set1, name == "diff_mean") %>% tidyr::unnest(value)
diff_mean <- round(diff_mean$value, 2)
t_test_pub <- dplyr::filter(bootstrap_data_set1, name == "t_results") %>% tidyr::unnest(value)

density_plot <- plot_bootstrapped_distro(densityplot_data, diff_mean, density_plot_ttl)

# ----------------------------------------- render results

widget_out <- manipulateWidget::combineWidgets(ncol = 2,
                                               title = html_ttl,
                                               plotly::ggplotly(box_plot),
                                               plotly::ggplotly(density_plot),
                                               manipulateWidget::combineWidgets(
                                                 DT::datatable(t_test_pub, rownames = FALSE, filter = "none")
                                                )
                                               )

htmlwidgets::saveWidget(
  widget = widget_out,
  file = "android_free_trial_retention_bootstrapping.html",
  selfcontained = TRUE
)
