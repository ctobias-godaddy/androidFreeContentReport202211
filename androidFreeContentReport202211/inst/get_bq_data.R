# sql params
ret_script_path <- './inst/experiment_retention_script.sql'
act_script_path <- './inst/experiment_activation_script.sql'
cad_script_path <- './inst/experiment_cad_script.sql'
pro_content_script_path <- './inst/experiment_pro_content_taps_script.sql'
revenue_script_path <- './inst/experiment_revenue_script.sql'
proj_exports_path <- './inst/project_exports_script.sql'

platform <- 'android'
exp_name <- 'android-customer-202211-free-content'
exp_variants <- c('control', 'treatment-7', 'treatment-14')
effect_type <- 'observed_effect' # observed_effect or true_effect where true_effect only includes users in their first session (not just new users)

start_dt <- '2023-01-09' # experiment started
end_alloc_dt <- '2023-02-01' # stop allocating users to experiment
end_dt <- '2023-03-17' # results analysed at

source('./R/read_sql.R')

# Get data from BQ for retention testing
bq_tbl_ret <- read_sql(ret_script_path, exp_name, exp_variants, platform, start_dt, end_dt, effect_type)
bq_ret_data <- bq_tbl_ret$data[[1]]

# Get activation data from BQ
bq_tbl_act <- read_sql(act_script_path, exp_name, exp_variants, platform, start_dt, end_dt, effect_type)
bq_act_data <- bq_tbl_act$data[[1]]

# Get CAD data from BQ
bq_tbl_cad <- read_sql(cad_script_path, exp_name, exp_variants, platform, start_dt, end_dt, effect_type)
bq_cad_data <- bq_tbl_cad$data[[1]]

# Get pro content data from BQ
bq_tbl_pro_content <- read_sql(pro_content_script_path, exp_name, exp_variants, platform, start_dt, end_dt, effect_type)
bq_pro_content_data <- bq_tbl_pro_content$data[[1]]

# Get data for revenue calculations from BQ
bq_tbl_revenue <- read_sql(revenue_script_path, exp_name, exp_variants, platform, start_dt, end_dt, effect_type)
bq_revenue_data <- bq_tbl_revenue$data[[1]]

# Get new user exports from BQ
bq_tbl_exports <- read_sql(proj_exports_path, exp_name, exp_variants, platform, start_dt, end_dt, effect_type)
bq_exports_data <- bq_tbl_exports$data[[1]]

bq_data_list <- list(
  retention_data = bq_ret_data,
  activation_data = bq_act_data,
  cad_data = bq_cad_data,
  pro_content_data = bq_pro_content_data,
  revenue_data = bq_revenue_data,
  exports_data = bq_exports_data
)

bq_data_tbl <- tibble::enframe(bq_data_list)

saveRDS(bq_data_tbl, "./inst/extdata/bq_data_exports.rds")
