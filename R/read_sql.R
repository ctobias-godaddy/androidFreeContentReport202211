#' read sql file for the ftp conversions
#'
#' @param path path to script
#' @param start_dt min date as string Y-m-d
#' @param end_dt max date as string Y-m-d
#' @param platform string specifying ios or android
#' @param exp_name
#' @param effect_type
#' @param exp_variant1
#' @param exp_variant2
#' @param exp_variant_3
#'
#' @return tbl_df containing query name, query string and required bq data
#'
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
        # "SQL_EXP_VARIANTS" = paste0("['", exp_variant1, "',", "'", exp_variant2, "',", "'", exp_variant3, "']"),
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
