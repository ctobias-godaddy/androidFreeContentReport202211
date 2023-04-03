WITH
  config AS (
    SELECT
    SQL_EXP_NAME AS exp_name,
    [SQL_EXP_VARIANTS] AS exp_variants,
    SQL_START_DATE AS start_dt,
    SQL_END_DATE AS end_dt,
  ),

  ----------- get actuals - these are used to derive assumptions in the pre period to extrapolate estimated values for the experiment period

  -- we want actuals for new users
  new_user_tbl AS (
  SELECT
    DATE(start_time) AS `date`,
    user_id
  FROM `over-data.users.sessions_android`, config
  WHERE DATE(start_time) BETWEEN DATE_SUB(DATE(start_dt), INTERVAL 1 month) AND DATE(end_dt)
  AND session_number = 1
  ),

  -- get actual new user subscriptions purchased
  actual_subs AS (
  SELECT
    DATE(TIMESTAMP_TRUNC(timestamp, DAY)) AS date,
    COUNT(DISTINCT user_id) AS actual_monthly_subs,
  FROM
    new_user_tbl, config
  INNER JOIN
    events.subscription_purchased USING(user_id)
  WHERE
    subscription_provider = 'PLAY_STORE'
    AND product_id IN
      ('app.over.editor.subscription.overpro.annual.9.99_and_69_99',
      'app.over.editor.subscription.overpro.monthly.9.99_and_69_99')
    AND DATE(timestamp) BETWEEN DATE_SUB(DATE(start_dt), INTERVAL 1 month) AND DATE(end_dt)
  GROUP BY
    date
  ),

  -- get actual new user conversions from free to paid
  actual_convs AS (
  SELECT
    DATE_SUB(DATE(TIMESTAMP_TRUNC(timestamp, DAY)), INTERVAL 6 day) AS date,
    COUNT(DISTINCT user_id) AS actual_annual_conv,
  FROM
    new_user_tbl, config
  INNER JOIN
    events.free_trial_converted_to_paid_subscriber USING(user_id)
  WHERE
    subscription_provider = 'PLAY_STORE'
    AND product_id IN
      ('app.over.editor.subscription.overpro.annual.9.99_and_69_99',
      'app.over.editor.subscription.overpro.monthly.9.99_and_69_99')
    AND DATE(timestamp) BETWEEN DATE_SUB(DATE(start_dt), INTERVAL 1 month) AND DATE(end_dt)
  GROUP BY
    date
  ),

------------------------------------ experiment data

  -- get experiment participants
  eusers AS (
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

  -- get subscriptions purchased by experiment participants
  experiment_subs AS (
  SELECT
    DATE(TIMESTAMP_TRUNC(timestamp, DAY)) AS date,
    COUNT(DISTINCT user_id) AS exp_monthly_subs,
  FROM
    eusers, config
  INNER JOIN
    events.subscription_purchased USING(user_id)
  WHERE
    subscription_provider = 'PLAY_STORE'
    AND product_id IN
      ('app.over.editor.subscription.overpro.annual.9.99_and_69_99',
      'app.over.editor.subscription.overpro.monthly.9.99_and_69_99')
    AND DATE(timestamp) BETWEEN DATE(start_dt) AND DATE(end_dt)
  GROUP BY
    date
  ),

  -- get experiment conversions
  experiment_convs AS (
  SELECT
    DATE_SUB(DATE(TIMESTAMP_TRUNC(timestamp, DAY)), INTERVAL 6 day) AS date,
    COUNT(DISTINCT user_id) AS exp_annual_conv,
  FROM
    eusers, config
  INNER JOIN
    events.free_trial_converted_to_paid_subscriber USING(user_id)
  WHERE
    subscription_provider = 'PLAY_STORE'
    AND product_id IN
      ('app.over.editor.subscription.overpro.annual.9.99_and_69_99',
      'app.over.editor.subscription.overpro.monthly.9.99_and_69_99')
    AND DATE(timestamp) BETWEEN DATE_SUB(DATE(start_dt), INTERVAL 1 month) AND DATE(end_dt)
  GROUP BY
    date
  ),

----- active subs
google_transactions AS (
    SELECT
      order_id,
      price_amount_micros/1000000 AS price_amount,
      price_currency_code
    FROM `over-data.over_core_replica.google_play_transaction`
),

subscriptions AS (
  SELECT DISTINCT
    user_id,
    subscription_states.original_transaction_id,
    subscription_states.latest_transaction_id,
    subscription_states.product_id,
    subscription_states.state_start,
    subscription_states.expiry_date,
    google_transactions.price_amount,
    google_transactions.price_currency_code,
    subscription_types.months AS months_per_billing_interval,
    fx_rates.rate,
    google_transactions.price_amount/fx_rates.rate AS price_usd,
    (google_transactions.price_amount/fx_rates.rate)/subscription_types.months AS monthly_usd_revenue
  FROM `over-data.users.subscription_states_view` subscription_states
  INNER JOIN eusers USING(user_id)
  INNER JOIN google_transactions
    ON subscription_states.latest_transaction_id = google_transactions.order_id
  INNER JOIN `over-data.users.subscription_types` subscription_types
    ON subscription_states.product_id = subscription_types.product_code
  INNER JOIN `over-data.itunes_connect.fx_rates` fx_rates
    ON DATE(subscription_states.state_start) = fx_rates.date
      AND google_transactions.price_currency_code = fx_rates.to_currency
  WHERE subscription_states.subscription_provider IN ('PLAY_STORE')
    AND state = 'SUBSCRIBED'
),

duplicates_removed AS (
SELECT
  dim_date.date,
  subscriptions.product_id,
  subscriptions.price_currency_code AS billing_currency,
  COUNT(DISTINCT subscriptions.latest_transaction_id) AS active_subscriptions,
  monthly_usd_revenue AS monthly_recurring_revenue_usd_per_subscription
FROM subscriptions
INNER JOIN `over-data.utils.dim_date` dim_date
ON TIMESTAMP(dim_date.date) BETWEEN subscriptions.state_start AND subscriptions.expiry_date
WHERE dim_date.date < CURRENT_DATE()
GROUP BY
  dim_date.date,
  subscriptions.product_id,
  subscriptions.price_currency_code,
  monthly_usd_revenue
),

exp_active_subs AS(
  SELECT
 date,
 product_id,
 billing_currency,
 active_subscriptions,
 monthly_recurring_revenue_usd_per_subscription * active_subscriptions AS monthly_recurring_revenue_usd
FROM duplicates_removed
),
------------------------------ MRR data

  mrr_monthly_tbl AS (
  SELECT
    date,
    SUM(active_subscriptions) AS subs_monthly,
    SUM(monthly_recurring_revenue_usd) AS mrr_monthly
  FROM
    `over-data.users.active_subscriptions_android`,
    config
  WHERE
    product_id = 'app.over.editor.subscription.overpro.monthly.9.99_and_69_99'
    AND date BETWEEN DATE_SUB(DATE(start_dt), INTERVAL 1 month) AND DATE(end_dt)
  GROUP BY
    date
  ORDER BY
    date DESC
),

  mrr_annual_tbl AS (
  SELECT
    date,
    SUM(active_subscriptions) AS subs_annual,
    SUM(monthly_recurring_revenue_usd) AS mrr_annual
  FROM
    `over-data.users.active_subscriptions_android`,
    config
  WHERE
    product_id = 'app.over.editor.subscription.overpro.annual.9.99_and_69_99'
    AND date BETWEEN DATE_SUB(DATE(start_dt), INTERVAL 1 month) AND DATE(end_dt)
  GROUP BY
    date
  ORDER BY
    date DESC
  ),

  exp_mrr_monthly_tbl AS (
  SELECT
    date,
    SUM(active_subscriptions) AS exp_subs_monthly,
    SUM(monthly_recurring_revenue_usd) AS exp_mrr_monthly
  FROM
    exp_active_subs,
    config
  WHERE
    product_id = 'app.over.editor.subscription.overpro.monthly.9.99_and_69_99'
    AND date BETWEEN DATE_SUB(DATE(start_dt), INTERVAL 1 month) AND DATE(end_dt)
  GROUP BY
    date
  ORDER BY
    date DESC
  ),

  exp_mrr_annual_tbl AS (
  SELECT
    date,
    SUM(active_subscriptions) AS exp_subs_annual,
    SUM(monthly_recurring_revenue_usd) AS exp_mrr_annual
  FROM
    exp_active_subs,
    config
  WHERE
    product_id = 'app.over.editor.subscription.overpro.annual.9.99_and_69_99'
    AND date BETWEEN DATE_SUB(DATE(start_dt), INTERVAL 1 month) AND DATE(end_dt)
  GROUP BY
    date
  ORDER BY
    date DESC
  ),

  mrr_tbl AS (
  SELECT *
  FROM mrr_monthly_tbl
  LEFT JOIN mrr_annual_tbl USING(date)
  LEFT JOIN exp_mrr_monthly_tbl USING(date)
  LEFT JOIN exp_mrr_annual_tbl USING(date)
  ),

-------------------------------------------------------

  trials AS (
  SELECT
    start.user_id AS start_id,
    conv.user_id conversion_id,
    bill.user_id bill_id,
    canc.user_id cancel_id,
    bill_success.user_id AS bill_success_id,
    DATE(TIMESTAMP_TRUNC(start.timestamp,day)) AS date,
    start.product_id
  FROM
    events.free_trial_started start, config
  LEFT JOIN
    events.free_trial_converted_to_paid_subscriber conv
  ON
    conv.user_id = start.user_id
  LEFT JOIN
    events.billing_error_occurred bill
  ON
    bill.user_id = start.user_id
  LEFT JOIN
    events.free_trial_cancelled canc
  ON
    canc.user_id = start.user_id
  LEFT JOIN
    events.billing_error_resolved bill_success
  ON
    bill_success.user_id = start.user_id
  WHERE
    start._PARTITIONTIME BETWEEN TIMESTAMP(DATE_SUB(DATE(start_dt), INTERVAL 1 MONTH)) AND TIMESTAMP(end_dt)
    AND start.subscription_provider = 'PLAY_STORE'
    AND start.timestamp >= TIMESTAMP(DATE_SUB(DATE(start_dt), INTERVAL 1 MONTH))
    AND (conv.timestamp > TIMESTAMP(DATE_SUB(DATE(start_dt), INTERVAL 1 MONTH)) OR conv.timestamp IS NULL)
    AND (bill.timestamp > TIMESTAMP(DATE_SUB(DATE(start_dt), INTERVAL 1 MONTH)) OR bill.timestamp IS NULL)
    AND (canc.timestamp > TIMESTAMP(DATE_SUB(DATE(start_dt), INTERVAL 1 MONTH)) OR canc.timestamp IS NULL)
    AND (bill_success.timestamp > TIMESTAMP(DATE_SUB(DATE(start_dt), INTERVAL 1 MONTH)) OR bill_success.timestamp IS NULL)
  ),

trial_conversions AS(
SELECT
  date,
  COUNT(DISTINCT conversion_id)/COUNT(DISTINCT start_id) AS conversion_rate,
  (COUNT(DISTINCT bill_id)-COUNT(DISTINCT bill_success_id))/COUNT(DISTINCT start_id) AS billing_error_not_yet_successful_rate,
  COUNT(DISTINCT cancel_id)/COUNT(DISTINCT start_id) AS cancellation_rate,
  COUNT(DISTINCT bill_success_id)/COUNT(DISTINCT start_id) AS successful_billing_error_rate
FROM
  trials, config
 GROUP BY
   date
),

--------------------------------------------

  -- count daily new users
  dnu_tbl AS (
  SELECT
    date,
    COUNT(DISTINCT user_id) AS new_users,
  FROM new_user_tbl
  GROUP BY date
  ),

  sub_tbl AS(
  SELECT
    dnu_tbl.*,
    actual_subs.actual_monthly_subs,
    actual_convs.actual_annual_conv,
    experiment_subs.exp_monthly_subs,
    experiment_convs.exp_annual_conv,
    mrr_tbl.subs_monthly,
    mrr_tbl.mrr_monthly,
    mrr_tbl.subs_annual,
    mrr_tbl.mrr_annual,
    mrr_tbl.exp_subs_monthly,
    mrr_tbl.exp_mrr_monthly,
    mrr_tbl.exp_subs_annual,
    mrr_tbl.exp_mrr_annual,
    trial_conversions.conversion_rate
  FROM
    dnu_tbl, config
  LEFT JOIN actual_subs USING(date)
  LEFT JOIN actual_convs USING(date)
  LEFT JOIN experiment_subs USING(date)
  LEFT JOIN experiment_convs USING(date)
  LEFT JOIN mrr_tbl USING(date)
  LEFT JOIN trial_conversions USING(date)
  )

SELECT * FROM sub_tbl ORDER BY date
