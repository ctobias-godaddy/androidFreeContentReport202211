plot_bootstrapped_distro <- function(densityplot_data, diff_mean, plot_ttl) {
  density_plot <- ggplot2::ggplot(densityplot_data, ggplot2::aes(x = boot_diff_mean)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), colour = "black", fill = "white") +
    ggplot2::geom_density(alpha = 0.2, fill = "#FF6666") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = diff_mean),
                        color = "blue", linetype = "dashed", linewidth = 1) +
    ggplot2::geom_text(label = "Observed difference",
                       x = diff_mean,
                       y = 0.3,
                       angle = 90,
                       vjust = 1) +
    ggplot2::labs(x = "Difference in Means (ppt)",
                  y = "Probability Density",
                  title = plot_ttl)

  density_plot
}

plot_box <- function(boxplot_data) {
  box_plot <- ggplot2::ggplot(boxplot_data, ggplot2::aes_string(x = group_var, y = num_var)) +
    ggplot2::geom_boxplot() +
    ggplot2::stat_summary(ggplot2::aes_string(y = num_var), fun = mean, geom = "point", shape = 20, size = 3, color = "red", fill = "red") +
    ggplot2::labs(ylab = "Users Retained (%)",
                  xlab = "Variant",
                  title = "Observed Retention Distribution")

  box_plot
}

plot_retention_curves <- function(retention_curve_data) {

  ret_curve <- ggplot2::ggplot(retention_curve_data, ggplot2::aes(x = nday, y = pct_users_retained, group = variant_name)) +
    ggplot2::geom_line(ggplot2::aes(color = variant_name)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste0("nDay Retention Curves"),
                  x = 'days',
                  y = 'users retained (%)'
                  ) +
    ggplot2::theme(legend.position = "bottom")

  ret_curve
}

plot_diff_data <- function(diff_data) {

  diff_data_abs <- dplyr::filter(diff_data, diff_type == "absolute_diff")

  diff_curve_abs <- ggplot2::ggplot(diff_data_abs, ggplot2::aes(x = nday, y = diff_value)) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste0("Retention Curve Absolute Difference"),
                  x = 'days',
                  y = 'absolute difference (ppt)'
                  )

  diff_data_rel <- dplyr::filter(diff_data, diff_type == "relative_diff")

  diff_curve_rel <- ggplot2::ggplot(diff_data_rel, ggplot2::aes(x = nday, y = diff_value)) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste0("Retention Curve Relative Difference"),
                  x = 'days',
                  y = 'relative difference (%)'
    )

  plot_tbl <- tibble::enframe(list(
    abs_plot = diff_curve_abs,
    rel_plot = diff_curve_rel)
    )

  plot_tbl
}
