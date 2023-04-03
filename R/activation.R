ret_act_df <- bq_req_act_data %>%
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
  dplyr::mutate(users_retained = cumsum(users)
                ) %>%
  dplyr::arrange(variant_name, max_nday) %>%
  dplyr::mutate(
                retained_pct = users_retained / total_users_per_variant,
                activated_pct = activations / total_activations_per_variant
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
                # users_activated,
                activated_pct
                )

activation_line <- bq_req_act_data %>%
  dplyr::mutate(date = as.Date(h0)) %>%
  dplyr::group_by(variant_name, date) %>%
  dplyr::mutate(activated_d0_first_pct = sum(!is.na(first_project_export_time)) / dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::select(variant_name, date, activated_d0_first_pct) %>%
  dplyr::distinct() %>%
  dplyr::filter(date <= '2023-02-01')

ggplot2::ggplot(activation_line, ggplot2::aes(x = date, y = activated_d0_first_pct, color = variant_name))+
  ggplot2::geom_line()

activation_pie <- bq_req_act_data %>%
  dplyr::mutate(date = as.Date(h0)) %>%
  dplyr::filter(date <= '2023-02-01') %>%
  dplyr::group_by(variant_name) %>%
  dplyr::mutate(activated_d0 = sum(!is.na(first_project_export_time)) / dplyr::n(),
                not_activated = 1 - activated_d0) %>%
  dplyr::ungroup() %>%
  dplyr::select(variant_name, activated_d0, not_activated) %>%
  dplyr::distinct() %>%
  tidyr::pivot_longer(cols = c("activated_d0", "not_activated"), names_to = "type", values_to = "value") %>%
  dplyr::mutate(value = round(value*100,2))

ggplot2::ggplot(activation_pie, ggplot2::aes(x = "", y = value, fill = type)) +
  ggplot2::geom_col(color = "black") +
  ggplot2::geom_text(ggplot2::aes(label = value),
            position = ggplot2::position_stack(vjust = 0.5)) +
  ggplot2::coord_polar(theta = "y") +
  ggplot2::scale_fill_brewer() +
  ggplot2::theme_void() +
  ggplot2::facet_wrap(~variant_name)

scatter_data <- dplyr::filter(ret_act_df, variant_name %in% c('control', 'treatment-7'),
                              nday>0)

ggplot2::ggplot(scatter_data, ggplot2::aes(x = activated_pct, y = retained_pct, color = variant_name))+
  ggplot2::geom_point()


