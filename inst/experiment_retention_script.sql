WITH
  config AS (
    SELECT
    SQL_EXP_NAME AS exp_name,
    [SQL_EXP_VARIANTS] AS exp_variants,
    SQL_PLATFORM AS platform,
    SQL_EFFECT_TYPE AS effect_type, -- observed or true effect
    SQL_START_DATE AS start_dt, -- dates reqd for true effect
    SQL_END_DATE AS end_dt,
    GENERATE_ARRAY(0, 180, 1) AS nday,
    1 AS nday_out_min, -- chart from nday
    DATE_DIFF(SQL_END_DATE, SQL_START_DATE, day) AS nday_out_max -- chart to nday
    -- 60 AS nday_out_max
  ),

  eusers_observed_android AS (
  SELECT
    variant_name,
    e.user_id,
    MIN(timestamp) AS h0,
    DATE(MIN(timestamp)) AS d0,
    'android' AS platform,
    'observed_effect' AS type
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

  eusers_observed_ios AS (
  SELECT
    variant_name,
    e.user_id,
    MIN(timestamp) AS h0,
    DATE(MIN(timestamp)) AS d0,
    'ios' AS platform,
    'observed_effect' AS type
  FROM
    config,
    `events.experiment_participated` AS e
  WHERE
    experiment_name = exp_name
    AND variant_name IN UNNEST(exp_variants)
  GROUP BY
    variant_name,
    user_id
  ),

  eusers_true_android AS (
    SELECT
      variant_name,
      e.user_id,
      MIN(start_time) AS h0,
      DATE(MIN(start_time)) AS d0,
      'android' AS platform,
      'true_effect' AS type
    FROM
      config,
      `over_android.experiment_participated` AS e
    INNER JOIN `users.sessions_android` AS s
      ON e.user_id = s.user_id
      AND session_number = 1
      AND DATE(start_time) BETWEEN start_dt AND end_dt
    WHERE
      experiment_name = exp_name
      AND variant_name IN UNNEST(exp_variants)
    GROUP BY
      variant_name,
      user_id
  ),

  eusers_true_ios AS (
    SELECT
      variant_name,
      e.user_id,
      MIN(start_time) AS h0,
      DATE(MIN(start_time)) AS d0,
      'ios' AS platform,
      'true_effect' AS type
    FROM
      config,
      `events.experiment_participated` AS e
    INNER JOIN `users.sessions_ios` AS s
      ON e.user_id = s.user_id
      AND session_number = 1
      AND DATE(start_time) BETWEEN start_dt AND end_dt
    WHERE
      experiment_name = exp_name
      AND variant_name IN UNNEST(exp_variants)
    GROUP BY
      variant_name,
      user_id
  ),

  eusers AS(
    SELECT
      eusers_observed_android.*
    FROM
      config, eusers_observed_android
    WHERE
      eusers_observed_android.platform = config.platform
      AND eusers_observed_android.type = config.effect_type
    UNION ALL
    SELECT
      eusers_observed_ios.*
    FROM
      config, eusers_observed_ios
    WHERE
      eusers_observed_ios.platform = config.platform
      AND eusers_observed_ios.type = config.effect_type
    UNION ALL
    SELECT
      eusers_true_android.*
    FROM
      config, eusers_true_android
    WHERE
      eusers_true_android.platform = config.platform
      AND eusers_true_android.type = config.effect_type
    UNION ALL
    SELECT
      eusers_true_ios.*
    FROM
      config, eusers_true_ios
    WHERE
      eusers_true_ios.platform = config.platform
      AND eusers_true_ios.type = config.effect_type
  ),

    retention_android AS (
  SELECT
    u.variant_name,
    u.user_id,
    MAX(DATE_DIFF(DATE(start_time), d0, day)) AS max_nday,
    'android' AS platform
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

   retention_ios AS (
  SELECT
    u.variant_name,
    u.user_id,
    MAX(DATE_DIFF(DATE(start_time), d0, day)) AS max_nday,
    'ios' AS platform
  FROM
    eusers AS u
  INNER JOIN
    users.sessions_ios AS s
  ON
    s.user_id = u.user_id
  GROUP BY
    variant_name,
    user_id
  ),

  retention AS (
  SELECT
    retention_android.*
  FROM
    retention_android, config
  WHERE
    retention_android.platform = config.platform
  UNION ALL
  SELECT
    retention_ios.*
  FROM
    retention_ios, config
  WHERE
    retention_ios.platform = config.platform
  ),

    retained_days AS (
  SELECT
    variant_name,
    user_id,
    nday
  FROM
    retention, config
  INNER JOIN
    UNNEST(config.nday) AS nday
  ON
    max_nday = nday
  ),

  totals AS (
  SELECT
    variant_name,
    nday,
    COUNT(DISTINCT user_id) AS total_users
  FROM
    retained_days
  GROUP BY
    variant_name,
    nday
  ),

   retention_pct AS (
  SELECT
    variant_name,
    nday,
    total_users,
    SUM(total_users) OVER (PARTITION BY variant_name ORDER BY nday DESC) AS total_users_retained,
    SUM(total_users) OVER (PARTITION BY variant_name) AS total_users_in_variant,
    SUM(total_users) OVER (PARTITION BY variant_name ORDER BY nday DESC) /
      SUM(total_users) OVER (PARTITION BY variant_name) * 100 AS pct_users_retained
  FROM
    totals
  ORDER BY
    nday,
    variant_name
  ),

  retention_curves AS (
  SELECT
    rp.*
  FROM
    retention_pct AS rp, config
  WHERE
    rp.nday BETWEEN nday_out_min AND nday_out_max
  ORDER BY
    rp.nday,
    variant_name
  )

SELECT * FROM retention_curves
