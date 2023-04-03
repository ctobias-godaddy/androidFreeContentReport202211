WITH
  config AS (
    SELECT
    SQL_EXP_NAME AS exp_name,
    [SQL_EXP_VARIANTS] AS exp_variants,
    SQL_START_DATE AS start_dt,
    SQL_END_DATE AS end_dt,
  ),

  proj_exp_tbl AS (
  SELECT
    user_id,
    DATE(timestamp) AS `date`
  FROM
    `over-data.over_android.project_exported`, config
  WHERE
    DATE(timestamp) BETWEEN DATE_SUB(DATE(start_dt), INTERVAL 12 MONTH) AND DATE_ADD(DATE(end_dt), INTERVAL 2 MONTH)
  ),

new_user_tbl AS (
  SELECT
    DATE(start_time) AS `date`,
    user_id
  FROM `over-data.users.sessions_android`, config
  WHERE
    DATE(start_time) BETWEEN DATE_SUB(DATE(start_dt), INTERVAL 12 MONTH) AND DATE_ADD(DATE(end_dt), INTERVAL 2 MONTH)
  AND session_number = 1
  ),

  new_user_exports AS (
  SELECT
    'new' AS user_type,
    new_user_tbl.date,
    COUNT(DISTINCT new_user_tbl.user_id) AS users,
    COUNT(DISTINCT exports.user_id) AS unq_exports,
    COUNT(exports.user_id) AS total_exports,
    COUNT(DISTINCT exports.user_id) / COUNT(DISTINCT new_user_tbl.user_id) AS exports_pct,
    COUNT(exports.user_id) / COUNT(DISTINCT new_user_tbl.user_id) AS exports_avg
  FROM
    new_user_tbl, config
  LEFT JOIN
   proj_exp_tbl AS exports ON new_user_tbl.user_id = exports.user_id
   AND new_user_tbl.date = exports.date
  GROUP BY
    date
  ),

  euser_tbl AS (
  SELECT
    variant_name,
    user_id,
    DATE(MIN(timestamp)) AS date
  FROM
    `over_android.experiment_participated`, config
  WHERE
    experiment_name = exp_name
    AND variant_name IN UNNEST(exp_variants)
  GROUP BY
    variant_name,
    user_id
  ),

  euser_exports AS (
  SELECT
    'experiment' AS user_type,
    euser_tbl.date,
    COUNT(DISTINCT euser_tbl.user_id) AS users,
    COUNT(DISTINCT exports.user_id) AS unq_exports,
    COUNT(exports.user_id) AS total_exports,
    COUNT(DISTINCT exports.user_id) / COUNT(DISTINCT euser_tbl.user_id) AS exports_pct,
    COUNT(exports.user_id) / COUNT(DISTINCT euser_tbl.user_id) AS exports_avg
  FROM
    euser_tbl, config
  LEFT JOIN
   proj_exp_tbl AS exports ON euser_tbl.user_id = exports.user_id
   AND euser_tbl.date = exports.date
  GROUP BY
    date
  )

  SELECT * FROM new_user_exports
  UNION ALL
  SELECT * FROM euser_exports
    ORDER BY
    user_type, date
