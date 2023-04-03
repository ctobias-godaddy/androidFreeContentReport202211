---
title: "Android Free Content and Features Experiment"
subtitle: "Results Analysis"
date: last-modified
date-format: "[Last Updated:] DD MMMM, YYYY"
author: "Cesaire Tobias"
format: PrettyPDF-pdf
---

```{r eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE}
# auth big query
options(gargle_oauth_email = TRUE) # sets an option that allows gargle to use cached tokens whenever there’s a unique match
if(bigrquery::bq_has_token() == FALSE) {
  bigrquery::bq_auth() # you will have to run this in an interactive session to auth. the access token will be cached
}

# optimizely results params
optimizely_results_path <- './inst/extdata/optimizely_export_20230109_20230317.csv'
reqd_metric_tbl <- tibble::tribble(
  ~ reqd_metric,                 ~reqd_type,
  'Project Exported',            'total conversions',
  'Project Exported',            'unique conversions',
  'D0 Second Project Exported',  'unique conversions',
  'Application Opened After D0', 'unique conversions') %>%
  tidyr::unite("metric_id", reqd_metric, reqd_type, sep = "-")

# sql params
ret_script_path <- './inst/experiment_retention_script.sql'
act_script_path <- './inst/experiment_activation_script.sql'
cad_script_path <- './inst/experiment_cad_script.sql'
pro_content_script_path <- './inst/experiment_pro_content_taps_script.sql'
revenue_script_path <- './inst/experiment_revenue_script.sql'

platform <- 'android'
exp_name <- 'android-customer-202211-free-content'
exp_variants <- c('control', 'treatment-7', 'treatment-14')
effect_type <- 'observed_effect' # observed_effect or true_effect where true_effect only includes users in their first session (not just new users)

start_dt <- '2023-01-09' # experiment started
end_alloc_dt <- '2023-02-01' # stop allocating users to experiment
end_dt <- '2023-03-17' # results analysed at

# bootstrap params
seed = 123 # set seed for reproducibility with sampling
b <- 10000  # number of bootstrap samples
num_var <- 'pct_users_retained' # numeric variable containing the values to test
group_var <- 'variant_name' # grouping variable - used to identify the variants we wish to test

# plot params
density_plot_ttl = "Bootstrapped Difference in Retention"
```

```{r eval = TRUE, echo = FALSE}
read_sql <- function(path, exp_name, exp_variants,
                     platform = c('ios', 'android'), start_dt, end_dt,
                     effect_type = c('observed_effect', 'true_effect')) {

  sql_script <- readr::read_file(path)

  reqd_string <- sql_script %>%
    stringr::str_replace_all(
      c("SQL_PLATFORM" = paste0("'", platform, "'"),
        "SQL_START_DATE" = paste0("DATE('", start_dt, "')"),
        "SQL_END_DATE" = paste0("DATE('", end_dt, "')"),
        "SQL_EXP_NAME" = paste0("'", exp_name, "'"),
        "SQL_EXP_VARIANTS" = paste("'", exp_variants, "'", sep = "", collapse = ","),
        "SQL_EFFECT_TYPE" = paste0("'", effect_type, "'"))
      )

  results_table <- bigrquery::bq_project_query('over-data', query = reqd_string)
  query_results <- bigrquery::bq_table_download(results_table)

  result_tbl <- tibble::tibble(name = basename(path),
                               query = reqd_string,
                               data = list(query_results))

  result_tbl
}
```

```{r eval = TRUE, echo = FALSE}
# Get data from BQ for retention testing
bq_tbl_ret <- read_sql(ret_script_path, exp_name, exp_variants, platform, start_dt, end_dt, effect_type)
bq_ret_data <- bq_tbl_ret$data[[1]]
```

```{r eval = TRUE, echo = FALSE}
# Get activation data from BQ
bq_tbl_act <- read_sql(act_script_path, exp_name, exp_variants, platform, start_dt, end_dt, effect_type)
bq_act_data <- bq_tbl_act$data[[1]]
```

```{r eval = TRUE, echo = FALSE}
# Get CAD data from BQ
bq_tbl_cad <- read_sql(cad_script_path, exp_name, exp_variants, platform, start_dt, end_dt, effect_type)
bq_cad_data <- bq_tbl_cad$data[[1]]
```

```{r eval = TRUE, echo = FALSE}
# Get pro content data from BQ
bq_tbl_pro_content <- read_sql(pro_content_script_path, exp_name, exp_variants, platform, start_dt, end_dt, effect_type)
bq_pro_content_data <- bq_tbl_pro_content$data[[1]]
```

```{r eval = TRUE, echo = FALSE}
# Get data for revenue calculations from BQ
bq_tbl_revenue <- read_sql(revenue_script_path, exp_name, exp_variants = c(exp_variants[[2]], exp_variants[[3]]), platform, start_dt, end_dt, effect_type)
bq_revenue_data <- bq_tbl_revenue$data[[1]]
```

```{r echo = FALSE, message=FALSE, warning=FALSE}

# convert raw optimizely results extract to neatly formatted table of results

reqd_cols <- c('Variation Name', 'Metric Event Name', 'Metric Numerator Type', 'Metric Denominator Type', 'Numerator Value',	'Denominator Value', 'Metric Value', 'Improvement Value from Baseline',	'Statistical Significance', 'Confidence Interval - Low',	'Confidence Interval - High')

raw_optimizely <- readr::read_csv(optimizely_results_path)

clean_optimizely <- raw_optimizely %>%
  dplyr::select(dplyr::all_of(reqd_cols)) %>%
  dplyr::rename(variation_name = 1,
                metric = 2,
                metric_numerator_type = 3,
                metric_denominator_type = 4,
                numerator_value = 5,
                denominator_value = 6,
                value = 7,
                change_vs_baseline = 8,
                statsig = 9,
                ci_low = 10,
                ci_high = 11) %>%
  dplyr::mutate(variation_name = stringr::str_remove_all(variation_name, '"'),
                metric = stringr::str_remove_all(metric, '"'),
                metric = stringr::str_remove_all(metric, '"')) %>%
  dplyr::mutate(change_vs_baseline = as.numeric(change_vs_baseline) * 100,
                statsig = as.numeric(statsig) * 100,
                ci_low = as.numeric(ci_low) * 100,
                ci_high = as.numeric(ci_high) * 100
  )
```

```{r eval = TRUE, echo = FALSE}
ret_act_df <- bq_act_data %>% 
  dplyr::arrange(variant_name, dplyr::desc(max_nday)) %>% 
  dplyr::group_by(variant_name, max_nday) %>%
  dplyr::mutate(users = dplyr::n_distinct(user_id),
                activations = dplyr::n_distinct(first_project_export_time)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(variant_name) %>% 
  dplyr::mutate(total_users_per_variant = dplyr::n_distinct(user_id),
                total_activations_per_variant = dplyr::n_distinct(first_project_export_time)
                ) %>% 
  dplyr::distinct(max_nday, .keep_all = TRUE) %>% 
  dplyr::mutate(users_retained = cumsum(users),
                users_activated = cumsum(activations)
                ) %>% 
  dplyr::arrange(variant_name, max_nday) %>% 
  dplyr::mutate(retained_pct = users_retained / total_users_per_variant,
                activated_pct = users_activated / total_activations_per_variant
                ) %>%
  dplyr::ungroup() %>% 
  dplyr::select(variant_name,
                nday = max_nday,
                users,
                total_users_per_variant,
                users_retained,
                retained_pct,
                activations,
                total_activations_per_variant,
                users_activated,
                activated_pct
                )
```

```{r eval = TRUE, echo = FALSE}
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
  mean_diff <- mean_variants[1] - mean_variants[2]
  diff_mean <- (mean_diff)

  #  diff in medians
  median_variants <- with(df, tapply(df[, num_var], df[, group_var], median))
  median_diff <- median_variants[1] - median_variants[2]
  diff_median <- (median_diff)

  ###########################################

  # Independent 2-sample t-test - Ho: means are equal
  # t_test <- t.test(df[, num_var] ~ df[, group_var], paired = FALSE, var.eq = FALSE)

  t_test_form <- as.formula(paste0(num_var,"~", group_var))
  t_test_pub <- rstatix::t_test(df, t_test_form, conf.level = 0.9, detailed = TRUE) # neatly format t-test results

  # Wilcoxon - Ho: medians are equal
  # wilcox_test <- wilcox.test(df[, num_var] ~ df[, group_var], paired = FALSE)

  # Kolmogorov-Smirnov 2-sample test - Ho: distributions are same
  # ks_test <- ks.test(df[, num_var][df[, group_var] == exp_variants[1]], df[, num_var][df[, group_var] == exp_variants[2]])


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

  #achieve significance levels
  asl_mean <- mean(boot_diff_mean >= diff_mean)
  asl_median <- mean(boot_diff_median >= diff_median)
  
  boot_df <- data.frame("boot_diff_mean" = boot_diff_mean,
                        "boot_diff_median" = boot_diff_median)

  result_list <- list (data = df,
                       diff_mean = diff_mean,
                       diff_median = diff_median,
                       asl_mean = asl_mean,
                       asl_median = asl_median,
                       bootstrapped_data = boot_df,
                       t_results = t_test_pub
                       # wilcox_results = wilcox_test,
                       # ks_results = ks_test
                       )

  result <- tibble::enframe(result_list)
  result
}
```

```{r eval = TRUE, echo = FALSE}
# Run bootstrap
# the bootstrapping algo only supports 2 variants at a time
exp_variants_1 <- c(exp_variants[1], exp_variants[2])
bs_req_data_1 <- dplyr::filter(bq_ret_data, variant_name %in% exp_variants_1)

bootstrap_data_set1 <- run_bootstrap(bs_req_data_1, 
                                     exp_variants = exp_variants_1,
                                     num_var, 
                                     group_var, 
                                     b_samples = b, 
                                     seed = seed)

exp_variants_2 <- c(exp_variants[1], exp_variants[3])
bs_req_data_2 <- dplyr::filter(bq_ret_data, variant_name %in% exp_variants_2)

bootstrap_data_set2 <- run_bootstrap(bs_req_data_2, 
                                     exp_variants = exp_variants_2,
                                     num_var, 
                                     group_var, 
                                     b_samples = b, 
                                     seed = seed)
```

## Background

The Android Free content and features experiment was designed to test whether a period of unrestricted usage would be an effective means of activating new users. 'Features' refers to, remove background, ability to upload custom logo, fonts, colour palettes etc while 'Content' refers to, templates, images, videos, graphics, fonts, articles, shapes etc. Note that users where not required to opt-in to join the treatment groups. Three variants were used, `control`, `treatment-7` (7 days free) and `treatment-14` (14 days free). The following analysis provides a summary of findings with further details provided for the M1 retention rates observed in the experiment participants. For more details on the experiment, see the proposal [here](https://godaddy-corp.atlassian.net/wiki/spaces/MV/pages/52660745/Free+content+and+features+experiment+for+new+users). For details, on the experiment outcomes for other metrics, see the Optimizely results [here](https://app.optimizely.com/v2/projects/14410340138/results/22576171398/experiments/22575942165?previousView=EXPERIMENTS).

## Experiment Details

The experiment allocated new users on Android to the treatment groups (`treatment-7` and `treatment-14`) and `control` for 3 weeks between 9 January 2023 and 1 February 2023. New user allocation was then reduced to 0% while the experiment participants were tracked for a further 6 weeks to 17 March 2023, in order to assess retention rates. The analysis makes use of maximum n day retention.

```{r echo=FALSE}
ss_data <- dplyr::select(clean_optimizely, variation_name, denominator_value) %>% dplyr::group_by(variation_name) %>% dplyr::distinct() %>% dplyr::ungroup()
var1 <- dplyr::filter(ss_data, variation_name == exp_variants[[1]]) %$% denominator_value
var2 <- dplyr::filter(ss_data, variation_name == exp_variants[[2]]) %$% denominator_value
var3 <- dplyr::filter(ss_data, variation_name == exp_variants[[3]]) %$% denominator_value
```

sample size (n) = `r format(sum(ss_data$denominator_value), big.mark = ',')`

-   `r exp_variants[[1]]`: `r format(var1, big.mark = ',')` `r paste0('(', round(var1 / sum(ss_data$denominator_value) * 100, 2), "%)")`

-   `r exp_variants[[2]]`: `r format(var2, big.mark = ',')` `r paste0('(', round(var2 / sum(ss_data$denominator_value) * 100, 2), "%)")`

-   `r exp_variants[[3]]`: `r format(var3, big.mark = ',')` `r paste0('(', round(var3 / sum(ss_data$denominator_value) * 100, 2), "%)")`

## Summary Results

```{r echo = FALSE, warning = FALSE, message = FALSE}

    pub_tbl <- clean_optimizely %>%
      # dplyr::filter(variation_name %in% dplyr::all_of(reqd_variations)) %>%
      tidyr::unite("metric_id", metric, metric_numerator_type, sep = "-", remove = FALSE) %>%
      dplyr::right_join(reqd_metric_tbl, by = "metric_id") %>%
      dplyr::mutate(numerator_value = format(numerator_value, big.mark = ',')) %>% 
      dplyr::mutate(value = ifelse(metric_numerator_type == 'unique conversions', paste0(round(value * 100, 2), "%"), round(value, 2))) %>%
      dplyr::mutate(statsig = ifelse(statsig == 100, ">99%", paste0(statsig, "%"))) %>%
      dplyr::mutate(metric_id = stringr::str_replace_all(metric_id, "-total conversions", " (Avg)"),
                    metric_id = stringr::str_replace_all(metric_id, "-unique conversions", " (%)")
      ) %>%
      dplyr::mutate(conf_int = paste0("[", round(ci_low, 2),"%;", round(ci_high, 2),"%]"),
                    change_vs_baseline = paste0(round(change_vs_baseline, 2), "%")
      ) %>%
      dplyr::select(variation_name,
                    Metric = metric_id,
                    Conversions = numerator_value,
                    `Conversion Rate` = value,
                    Improvement = change_vs_baseline,
                    `Stat-Sig` = statsig,
                    `Conf int.` = conf_int)
```

```{r echo = FALSE, warning = FALSE, message = FALSE}
#| label: tbl-optimizely_results
#| tbl-cap: Optimizely Results
#| tbl-subcap:
#| - "Treatment-7 vs Control"
#| - "Treatment-14 vs Control"
#| layout-nrow: 2
pub_tbl %>% 
  dplyr::filter(variation_name == exp_variants[2]) %>% 
  dplyr::select(!variation_name) %>% 
knitr::kable()

pub_tbl %>% 
  dplyr::filter(variation_name == exp_variants[3]) %>% 
  dplyr::select(!variation_name) %>% 
knitr::kable()
```

In @tbl-optimizely_results, `Conversions` for % metrics are unique conversions while for avg metrics they are total conversions. The `Conversion Rate` is therefore, total conversions per user for avg metrics and unique conversions per user for % metrics. `Improvement`, represents the change in conversion rate relative to the control.

<!-- | Metric                          | Conversions | Conversion Rate | Improvement | Stat-Sig | Conf Int.           | -->

<!-- |------------|------------|------------|------------|------------|------------| -->

<!-- | Project Exported (avg)          | 165,597     | 2.690           | +49.19%     | \>99%    | \[+46.12%;+69.50%\] | -->

<!-- | Project Exported (%)            | 23,060      | 37.46%          | +25.73%     | \>99%    | \[+23.14%;+28.06%\] | -->

<!-- | D0 Second Project Exported (%)  | 7,445       | 12.09%          | +46.15%     | \>99%    | \[+39.08%;+55.95%\] | -->

<!-- | Application Opened After D0 (%) | 19,366      | 31.46%          | +26.32%     | \>99%    | \[+23.75%;+29.88%\] | -->

<!-- : *Treatment-7 vs Control* {#tbl-treatment1_optimizely_results} -->

<!-- | Metric                          | Conversions | Conversion Rate | Improvement | Stat-Sig | Conf Int.           | -->

<!-- |------------|------------|------------|------------|------------|------------| -->

<!-- | Project Exported (avg)          | 187,691     | 3.058           | +69.60%     | \>99%    | \[+55.88%;+93.73%\] | -->

<!-- | Project Exported (%)            | 23,370      | 38.07%          | +27.80%     | \>99%    | \[+25.29%;+31.01%\] | -->

<!-- | D0 Second Project Exported (%)  | 7,601       | 12.38%          | +49.66%     | \>99%    | \[+42.32%;+55.95%\] | -->

<!-- | Application Opened After D0 (%) | 19,423      | 31.64%          | +27.07%     | \>99%    | \[+25.87%;+30.64%\] | -->

<!-- : *Treatment-14 vs Control* {#tbl-treatment2_optimizely_results} -->

## Retention Analysis

M1 retention is the primary business success metric in this experiment. To evaluate M1 unbounded retention[^1] for new users on Android we refer to the techniques proposed in [Experimentation: User Retention Testing](https://knowledge.mvdataserv.int.gdcorp.tools/posts/user-retention-testing.html). We will make use of visual inspection and bootstrap testing.

[^1]: Whether the user opened the app any time between day 30 and 59.

### Visual Inspection

```{r eval = TRUE, echo = FALSE}
retention_curve_data <- bq_ret_data %>%
  dplyr::select(variant_name, nday, pct_users_retained)

retention_diff_data <- retention_curve_data %>%
  tidyr::pivot_wider(names_from = variant_name, values_from = pct_users_retained) %>%
  dplyr::mutate(absolute_diff = !!dplyr::sym(exp_variants[2]) - !!dplyr::sym(exp_variants[1])) %>%
  dplyr::mutate(relative_diff = (!!dplyr::sym(exp_variants[2]) / !!dplyr::sym(exp_variants[1]) - 1) * 100) %>%
  dplyr::select(!dplyr::all_of(exp_variants)) %>%
  tidyr::pivot_longer(!nday, names_to = "diff_type", values_to = "diff_value")

  diff_data_abs <- dplyr::filter(retention_diff_data, diff_type == "absolute_diff")
  diff_data_rel <- dplyr::filter(retention_diff_data, diff_type == "relative_diff")
```

Any experiment with retention as a primary metric should use the `application opened after d0` metric. This allows us to use Optimizely's stats engine to test significance.

In the event that significance is detected, visual inspection of unbounded retention should follow in order to provide an indication of the nature of the retention, viz., quick convergence, potential convergence or uniform improvement. In this experiment we find that the treatment outperforms the control in percentage of users retained and that the relative difference remains fairly stable at an average difference of `r round(mean(diff_data_rel$diff_value), 2)`% (see @fig-retention_diff).

```{r eval = TRUE, echo = FALSE, warning = FALSE}
#| label: fig-retention_curves
#| fig-cap: "Retention Curves"
#| fig-pos: "h"

ggplot2::ggplot(retention_curve_data, ggplot2::aes(x = nday, y = pct_users_retained, group = variant_name)) +
    ggplot2::geom_line(ggplot2::aes(color = variant_name)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste0("nDay Retention Curves"),
                  x = 'days',
                  y = 'users retained (%)'
                  ) +
    ggplot2::theme(legend.position = "bottom")
```

@fig-retention_curves shows that the treatment groups outperform the control, with little difference between `treatment-7` and `treatment-14`. Through visual inspection we can be confident that there is an increase in D0 retention (confirmed in Optimizely with the `application opened after d0` event).

Taking the visual inspection a step further, @fig-retention_diff quantifies the differences between the curves for the `control` and `treatment-7`. We see a relatively stable, positive difference which would indicate improved retention as a result of the treatment. In the next section we make use of bootstrapping as a more rigorous quantitative approach to our assessment.

```{r eval = TRUE, echo = FALSE, warning = FALSE}
#| label: fig-retention_diff
#| fig-cap: "Differences in user retention over time"
#| fig-subcap:
#| - "Absolute Difference"
#| - "Relative Difference"
#| fig-pos: "h"
#| layout-nrow: 2

  ggplot2::ggplot(diff_data_abs, ggplot2::aes(x = nday, y = diff_value)) +
    ggplot2::geom_line(color = scales::hue_pal()(2)[2]) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste0("Retention Curve Absolute Difference"),
                  x = 'days',
                  y = 'absolute difference (ppt)'
    )

  ggplot2::ggplot(diff_data_rel, ggplot2::aes(x = nday, y = diff_value)) +
    ggplot2::geom_line(color = scales::hue_pal()(2)[1]) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste0("Retention Curve Relative Difference"),
                  x = 'days',
                  y = 'relative difference (%)'
    )
```

### Bootstrapping

Bootstrapping is a procedure that re-samples a single dataset, in this instance, the sample collected over the first 3 weeks of the experiment. This single sample is treated as one of many random samples that the experiment could have collected. We construct a sampling distribution from the simulated datasets created by drawing repeatedly from our actual data (`r b` times in this analysis).

The process works as follows:

1.  We assume that there is no difference between the control and the treatment. Our null hypothesis would be to say that they \[control and treatment\] are from the same distribution.

2.  With this assumption, we can simulate the difference we'd see from random sub-samples of the control and treatment users and create a probability distribution from this.

3.  We then compare this to the actual difference we observed in the experiment. If it's sufficiently unlikely that the observed difference falls within the sampling distribution, then we reject the null hypothesis.

```{r, eval = TRUE, echo = FALSE, warning = FALSE}
#| label: fig-retention_distro_box
#| fig-cap: "Observed Retention Distribution"
#| fig-pos: "h"
boxplot_data <- dplyr::filter(bootstrap_data_set1, name == "data") %>% tidyr::unnest(value)

ggplot2::ggplot(boxplot_data, ggplot2::aes_string(x = group_var, y = num_var)) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_minimal() +
    ggplot2::stat_summary(ggplot2::aes_string(y = num_var), fun = mean, geom = "point", shape = 20, size = 3, color = scales::hue_pal()(2)[1], fill = scales::hue_pal()(2)[1]) +
    ggplot2::labs(ylab = "Users Retained (%)",
                  xlab = "Variant",
                  title = "Observed Retention Distribution")
```

```{r echo = FALSE}
diff_mean <- dplyr::filter(bootstrap_data_set1, name == "diff_mean") %>% tidyr::unnest(value)
diff_mean <- round(diff_mean$value, 2)
```

```{r echo = FALSE}
asl_mean <- dplyr::filter(bootstrap_data_set1, name == "asl_mean") %>% tidyr::unnest(value)
asl_mean <- round(asl_mean$value, 2)
```

@fig-retention_distro_box illustrates the distribution of our collected sample for the control and `r exp_variants[2]`. We observe both an increase in the mean as well as a wider inter-quartile range in the treatment. The treatment mean is `r if(diff_mean<0){paste0(-diff_mean, " percentage points higher than the control.")} else {print0(diff_mean, " percentage points lower than the control.")}`

```{r, echo = FALSE}
#| label: tbl-t_results
#| tbl-cap: "T-test results"

dplyr::filter(bootstrap_data_set1, name == "t_results") %>% 
  tidyr::unnest(value) %>% 
  dplyr::select(!!dplyr::sym(exp_variants[1]) := estimate1, 
                !!dplyr::sym(exp_variants[2]) := estimate2, 
                diff = estimate, n1, n2, test_stat = statistic, p, conf.low, conf.high) %>% 
gt::gt() %>%
  gt::tab_header(
    title = "Two-sample T-test",
    subtitle = "alpha: 90%"
  ) %>% 
  gt::fmt_number(
    columns = 1:9,
    decimals = 2, 
    sep_mark = ","
  )
```

@fig-bootstrapped_distro shows the observed difference in the means of the `contol` and `r exp_variants[2]` against the bootstrapped distribution. The Achieved Significance Level (ASL)[^2] is at `r asl_mean` and we see `treatment-7` outperforming the `control`.

[^2]: Indicates the percentage of the distribution that is more than the observed value.

```{r, eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE}
#| label: fig-bootstrapped_distro
#| fig-cap: "Bootstrapped Distribution"
#| fig-pos: "h"

densityplot_data <- dplyr::filter(bootstrap_data_set1, name == "bootstrapped_data") %>% 
  tidyr::unnest(value)

t_test_pub <- dplyr::filter(bootstrap_data_set1, name == "t_results") %>% tidyr::unnest(value)

ggplot2::ggplot(densityplot_data, ggplot2::aes(x = boot_diff_mean)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), colour = "black", fill = "white") +
    ggplot2::theme_minimal() + 
    ggplot2::geom_density(alpha = 0.2, fill = "#00A4A7") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = diff_mean),
                        color = scales::hue_pal()(2)[1], linetype = "dashed", linewidth = 1) +
    ggplot2::geom_text(label = "Observed difference",
                       x = diff_mean,
                       y = 0.2,
                       angle = 90,
                       vjust = 1) +
    ggplot2::labs(x = "Difference in Means (ppt)",
                  y = "Probability Density",
                  title = density_plot_ttl,
                  subtitle = paste("ASL: ", asl_mean)
                  )
```

## Learnings

### Activation

@fig-activation shows the activation rates as measured by `d0 first project exported`. While we see higher activation rates of \~7ppt for the treatment groups relative to the control, there is little difference between `treatment-7` and `treatment-14`.

```{r, eval = TRUE, echo = FALSE}
activation_pie <- bq_act_data %>%
  dplyr::mutate(date = as.Date(h0)) %>%
  dplyr::filter(date <= end_alloc_dt) %>%
  dplyr::group_by(variant_name) %>%
  dplyr::mutate(activated_d0 = sum(!is.na(first_project_export_time)) / dplyr::n(),
                not_activated = 1 - activated_d0) %>%
  dplyr::ungroup() %>%
  dplyr::select(variant_name, activated_d0, not_activated) %>%
  dplyr::distinct() %>% 
  tidyr::pivot_longer(cols = c("activated_d0", "not_activated"), names_to = "type", values_to = "value") %>% 
  dplyr::mutate(value = round(value*100,2))
```

```{r, eval = TRUE, echo = FALSE}
activation_line <- bq_act_data %>%
  dplyr::mutate(date = as.Date(h0)) %>%
  dplyr::group_by(variant_name, date) %>%
  dplyr::mutate(activated_d0_first_pct = sum(!is.na(first_project_export_time)) / dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::select(variant_name, date, activated_d0_first_pct) %>%
  dplyr::distinct() %>%
  dplyr::filter(date <= end_alloc_dt)
```

```{r, eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE}
#| label: fig-activation
#| fig-cap: "Activation Rates"
#| fig-subcap:
#| - "Total activation rates"
#| - "Activation rate time series"
#| fig-pos: "h"
#| layout-nrow: 2
ggplot2::ggplot(activation_pie, ggplot2::aes(x = "", y = value, fill = type)) +
  ggplot2::geom_col(color = "black") +
  ggplot2::geom_text(ggplot2::aes(label = paste0(value, "%")),
            position = ggplot2::position_stack(vjust = 0.5)) +
  ggplot2::coord_polar(theta = "y") +
  ggplot2::scale_fill_brewer() +
  ggplot2::theme_void() +
  ggplot2::labs(title = "Total Activation Rates",
                subtitle = paste0(start_dt, " - ", end_alloc_dt)
                ) +
  ggplot2::facet_wrap(~variant_name)

ggplot2::ggplot(activation_line, ggplot2::aes(x = date, y = activated_d0_first_pct, color = variant_name))+
  ggplot2::geom_line()+
  ggplot2::theme_minimal()+
  ggplot2::labs(x = "Date",
                y = "Activation Rate (%)",
                title = "Activation Rates per Variant",
                subtitle = "D0 First Project Exported")
```

### Engagement

```{r eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE}
#| label: fig-weekly_cad
#| fig-cap: "Weekly average Canvas Active Days"
#| fig-pos: "h"
ggplot2::ggplot(bq_cad_data, ggplot2::aes(x = date, y = weekly_avg_cad, color = variant_name)) +
  ggplot2::geom_line() +
  ggplot2::theme_minimal() +
  ggplot2::labs(x= "Date",
                y = "Avg CAD",
                title = "Weekly Average Canvas Active Days")
```

```{r, echo = FALSE, warning = FALSE, message = FALSE}
ggplot2::ggplot(bq_pro_content_data, ggplot2::aes(x = element_type, y = total_taps)) +
  ggplot2::geom_col(ggplot2::aes(color = variant_name, fill = variant_name), position =   ggplot2::position_dodge(0.8), width = 0.7) +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "Pro Content Taps")
  # ggplot2::facet_wrap(~variant_name)
```

```{r eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE}
exp_start_dt <- start_dt

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
```

### Revenue

```{r echo = FALSE}
knitr::kable(revenue_tbl)
```

## Next Steps
