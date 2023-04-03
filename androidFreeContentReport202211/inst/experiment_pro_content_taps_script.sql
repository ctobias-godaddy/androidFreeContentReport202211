WITH
  config AS (
    SELECT
    SQL_EXP_NAME AS exp_name,
    [SQL_EXP_VARIANTS] AS exp_variants,
    SQL_START_DATE AS start_dt,
    SQL_END_DATE AS end_dt
  ),

  users AS (
  SELECT
    variant_name,
    user_id
  FROM
    `over-data.over_android.experiment_participated`, config
  WHERE
    experiment_name = exp_name
    AND variant_name IN UNNEST(exp_variants)
    AND timestamp BETWEEN timestamp(start_dt) AND timestamp(end_dt)
  ),

  elements AS (
  SELECT
    user_id,
    element_type,
    timestamp
  FROM
    `over_android.element_tapped`, config
  WHERE
    distribution_type = 'pro'
    AND DATE(timestamp) BETWEEN DATE(start_dt) AND DATE(end_dt)
  )

SELECT
  variant_name,
  element_type,
  COUNT(*) total_taps
FROM
  users u
INNER JOIN
  elements e
ON
  e.user_id = u.user_id
GROUP BY
  variant_name,
  element_type
ORDER BY
  element_type,
  variant_name