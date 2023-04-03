source_dir <- 'optimizely_export_20230109_20230317.csv'

reqd_metric_tbl <- tibble::tribble(
  ~ reqd_metric,                 ~reqd_type,
  'Project Exported',            'total conversions',
  'Project Exported',            'unique conversions',
  'D0 Second Project Exported',  'unique conversions',
  'Application Opened After D0', 'unique conversions') %>%
  tidyr::unite("metric_id", reqd_metric, reqd_type, sep = "-")

reqd_variation_name <- c('treatment-7')

get_optimizely_results <- function(source_dir, reqd_metrics, reqd_variations, to_publish = TRUE) {

  reqd_cols <- c('Variation Name', 'Metric Event Name', 'Metric Numerator Type', 'Metric Denominator Type', 'Numerator Value',	'Denominator Value',
                 'Metric Value', 'Improvement Value from Baseline',	'Statistical Significance', 'Confidence Interval - Low',	'Confidence Interval - High')

  raw_optimizely <- readr::read_csv(source_dir)

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

  if (to_publish) {
    pub_tbl <- clean_optimizely %>%
      dplyr::filter(variation_name %in% dplyr::all_of(reqd_variations)) %>%
      tidyr::unite("metric_id", metric, metric_numerator_type, sep = "-", remove = FALSE) %>%
      dplyr::right_join(reqd_metrics, by = "metric_id") %>%
      dplyr::mutate(numerator_value = format(numerator_value, big.mark = ',')) %>%
      dplyr::mutate(value = ifelse(metric_numerator_type == 'unique conversions', paste0(round(value * 100, 2), "%"), round(value, 2))) %>%
      dplyr::mutate(statsig = ifelse(statsig == 100, ">99%", paste0(statsig, "%"))) %>%
      dplyr::mutate(metric_id = stringr::str_replace_all(metric_id, "-total conversions", " (Avg)"),
                    metric_id = stringr::str_replace_all(metric_id, "-unique conversions", " (%)")
      ) %>%
      dplyr::mutate(conf_int = paste0("[", round(ci_low, 2),"%;", round(ci_high, 2),"%]"),
                    change_vs_baseline = paste0(round(change_vs_baseline, 2), "%")
      ) %>%
      dplyr::select(metric = metric_id,
                    conversions = numerator_value,
                    conversion_rate = value,
                    improvement = change_vs_baseline,
                    stat_sig = statsig,
                    conf_int)

    result <- pub_tbl
  } else {
    result <- clean_optimizely
  }

  result
}







