WITH

  config AS (
    SELECT
    SQL_EXP_NAME AS exp_name,
    [SQL_EXP_VARIANTS] AS exp_variants,
    SQL_START_DATE AS start_dt,
    SQL_END_DATE AS end_dt,
    GENERATE_ARRAY(0, 180, 1) AS nday
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
  GROUP BY
    variant_name,
    user_id
  ),

  retention AS (
  SELECT
    u.variant_name,
    u.user_id,
    MAX(DATE_DIFF(DATE(start_time), d0, day)) AS max_nday
  FROM
    eusers AS u
  INNER JOIN
    users.sessions_android AS s
  ON
    s.user_id = u.user_id
  GROUP BY
    variant_name,
    user_id
  ),

  retained_days AS (
  SELECT
    variant_name,
    user_id,
    max_nday
  FROM
    retention, config
  INNER JOIN
    UNNEST(config.nday) AS nday
  ON
    max_nday = nday
  ),

  user_tbl AS (
    SELECT
      user_id,
      device_id,
      start_time h0,
      timestamp_add(start_time, interval 18 hour) h18
    FROM `over-data.users.sessions_android`, config
    WHERE session_number = 1
    AND DATE(start_time) >= start_dt
    ),

  proj_export_tbl AS (
    SELECT
        context_device_id AS device_id,
        project_id,
        `timestamp`
    FROM `over-data.over_android.project_exported`, config
    WHERE timestamp >= TIMESTAMP(start_dt)
  ),

  project_times AS (
    SELECT
      user_tbl.device_id,
      h0,
      h18,
      project_id,
      MIN(`timestamp`) AS time_exported
    FROM user_tbl
    LEFT JOIN proj_export_tbl ON user_tbl.device_id = proj_export_tbl.device_id
    AND proj_export_tbl.`timestamp` BETWEEN h0 AND h18
    GROUP BY device_id, h0, h18, project_id
    ),

  project_orders AS (
    SELECT *,
    ROW_NUMBER() OVER (PARTITION BY device_id ORDER BY time_exported) row_num
    FROM project_times
    ),

  activations AS (
    SELECT user_tbl.*,
      p1.time_exported AS first_project_export_time,
      p2.time_exported AS second_project_export_time
    FROM user_tbl
    LEFT JOIN project_orders p1 ON user_tbl.device_id = p1.device_id AND p1.row_num = 1
    LEFT JOIN project_orders p2 ON user_tbl.device_id = p2.device_id AND p2.row_num = 2
)

  SELECT
    variant_name,
    user_id,
    max_nday,
    h0,
    first_project_export_time,
    second_project_export_time
  FROM retained_days
  LEFT JOIN activations USING(user_id)

