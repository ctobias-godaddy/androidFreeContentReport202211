WITH
  config AS (
    SELECT
    SQL_EXP_NAME AS exp_name,
    [SQL_EXP_VARIANTS] AS exp_variants,
    SQL_START_DATE AS start_dt,
    SQL_END_DATE AS end_dt
    ),

eusers AS (
  SELECT
    variant_name,
    e.user_id,
    MIN(timestamp) AS h0,
    DATE(MIN(timestamp)) AS d0
  FROM
    config,
    `over_android.experiment_participated` AS e
  WHERE
    experiment_name = exp_name
    AND variant_name IN UNNEST(exp_variants)
    AND timestamp BETWEEN timestamp(start_dt) AND timestamp(end_dt)
  GROUP BY
    variant_name,
    user_id
  ),

  cads AS (
  SELECT
    variant_name,
    u.user_id,
    DATE_TRUNC(DATE(start_time), week) AS date,
    COUNT(DISTINCT DATE(start_time)) cad
  FROM
    eusers u
  LEFT JOIN
    users.canvas_sessions_android c
  ON
    c.user_id = u.user_id
    AND start_time >= h0
  GROUP BY
    variant_name,
    user_id,
    date)

SELECT
  variant_name,
  date,
  SUM(cad) AS sum_cad,
  COUNT(*) AS users,
  SUM(cad) / COUNT(*) AS weekly_avg_cad
FROM
  cads
GROUP BY
  variant_name,
  date
ORDER BY
  variant_name,
  date
