#
#
# pmder_figs <- function() {return(list(
#     star_differences =
#         spectral_data |>
#         dplyr::mutate(
#             sLF1 = 2 * (
#                 (LF1 * 1e-3 * Ed_keV) - LsF1) / ((LF1 * 1e-3 * Ed_keV) + LsF1),
#             sLF2 = 2 * (
#                 (LF2 * 1e-3 * Ed_keV) - LsF2) / ((LF2 * 1e-3 * Ed_keV) + LsF2),
#             sLF4 = 2 * (
#                 (LF4 * 1e-3 * Ed_keV) - LsF4) / ((LF4 * 1e-3 * Ed_keV) + LsF4),
#             sLF8 = 2 * (
#                 (LF8 * 1e-3 * Ed_keV) - LsF8) / ((LF8 * 1e-3 * Ed_keV) + LsF8),
#             sRF1 = 2 * (
#                 (RF1 * 1e-3 * Ed_keV) - RsF1) / ((RF1 * 1e-3 * Ed_keV) + RsF1),
#             sRF2 = 2 * (
#                 (RF2 * 1e-3 * Ed_keV) - RsF2) / ((RF2 * 1e-3 * Ed_keV) + RsF2),
#             sRF4 = 2 * (
#                 (RF4 * 1e-3 * Ed_keV) - RsF4) / ((RF4 * 1e-3 * Ed_keV) + RsF4),
#             sRF8 = 2 * (
#                 (RF8 * 1e-3 * Ed_keV) - RsF8) / ((RF8 * 1e-3 * Ed_keV) + RsF8)
#         ) |>
#         dplyr::select(
#             sLF1,
#             sLF2,
#             sLF4,
#             sLF8,
#             sRF1,
#             sRF2,
#             sRF4,
#             sRF8
#         ) |>
#         tidyr::pivot_longer(
#             cols = c("sLF1", "sLF2", "sLF4","sLF8", "sRF1","sRF2", "sRF4","sRF8"),
#             names_to = "tally",
#             values_to = "RelativeDifference"
#         ) |>
#         dplyr::filter(RelativeDifference <= 0.5) |>
#         dplyr::filter(!is.nan(RelativeDifference)) |>
#         ggplot2::ggplot() +
#         ggplot2::geom_histogram(
#             mapping = ggplot2::aes(
#                 x = RelativeDifference,
#                 y = ggplot2::after_stat(density)
#             ),
#             binwidth = 0.01,
#             na.rm = TRUE
#         ) +
#         ggplot2::xlab("Relative Difference") +
#         ggplot2::scale_x_continuous(limits = c(0,0.5), expand = c(0,0)) +
#         ggplot2::ylab(NULL) +
#         ggplot2::scale_y_continuous(expand = c(0,0)) +
#         ggplot2::ggtitle(
#             "Distribution of Differences",
#             subtitle = "Particle and Radiant Energy Tallies"
#         ) +
#         ggplot2::theme_bw(),
#     parity_differences =
#         spectral_data |>
#         dplyr::mutate(
#             sLF1 = 2 * (LF1 - RF1) / (LF1 + RsF1),
#             sLF2 = 2 * (LF2 - RF2) / (LF2 + RsF2),
#             sLF4 = 2 * (LF4 - RF4) / (LF4 + RsF4),
#             sLF8 = 2 * (LF8 - RF8) / (LF8 + RsF8)
#         ) |>
#         dplyr::select(
#             sLF1,
#             sLF2,
#             sLF4,
#             sLF8,
#         ) |>
#         tidyr::pivot_longer(
#             cols = c("sLF1", "sLF2", "sLF4","sLF8"),
#             names_to = "tally",
#             values_to = "RelativeDifference"
#         ) |>
#         ggplot2::ggplot(ggplot2::aes(
#             x = RelativeDifference,
#             y = ggplot2::after_stat(density)
#         )) +
#         ggplot2::geom_histogram(
#             binwidth = 0.01, na.rm = TRUE
#         ) +
#         ggplot2::xlab("Relative Difference") +
#         ggplot2::scale_x_continuous(limits = c(-2,2), expand = c(0,0)) +
#         ggplot2::ylab(NULL) +
#         ggplot2::scale_y_continuous(expand = c(0,0)) +
#         ggplot2::ggtitle("Differences between Left and Right Tallies") +
#         ggplot2::theme_bw(),
#     prob_reach =
#         summary_data |>
#         ggplot2::ggplot() +
#         ggplot2::geom_line(
#             mapping = aes(x = Es_keV, y = PrReach, color = as.factor(y_cm)),
#             linewidth = 1
#         ) +
#         ggplot2::facet_grid(
#             cols = vars(contents),
#             labeller = as_labeller(c("m"="Scrap Metal", "f"="Foodstuff"))
#         ) +
#         scale_x_continuous(limits = c(0,2000)) +
#         xlab("Source Energy (keV)") +
#         scale_y_continuous(
#             trans = "log10",
#             limits = c(1e-9, 1e-1),
#             n.breaks = 10,
#             labels = scales::trans_format("log10", scales::math_format(10^.x))
#         ) +
#         ylab("Probability (Log10 Scale)") +
#         scale_color_hue(
#             name = "Distance (cm): ",
#             breaks = c(0, -250, -500, -750, -1000, -1250, -1500),
#             labels = c("0     ", "250 ", "500 ", "750 ", "1000", "1250", "1500")
#         ) +
#         guides(color = guide_legend(nrow = 1)) +
#         ggtitle("Probability of Photon Reaching Detector") +
#         theme_bw()  +
#         theme(
#             legend.position = "top",
#             legend.title = element_text(size=7),
#             legend.text = element_text(size=7),
#             strip.background = element_rect(fill = "white")
#         ),
#     interior_atten_coeff =
#         interior_atten_coeff |>
#         ggplot() +
#         geom_line(mapping = aes(x = 1000 * E_MeV, y = mu_cm, color = Material)) +
#         scale_x_continuous(limits = c(0,2000)) +
#         xlab("Source Energy (keV)") +
#         scale_y_continuous(
#             trans = "log10",
#             limits = c(1e-2, 1e5),
#             n.breaks = 10,
#             labels = scales::trans_format("log10", scales::math_format(10^.x))
#         ) +
#         ylab("Attenuation Coefficienty (1 / cm)") +
#         ggtitle("Attenuation Coefficients of Truck Cargo") +
#         theme_bw(),
#     # Probability of Detection
#     prob_det =
#         summary_data |>
#         ggplot2::ggplot() +
#         ggplot2::geom_line(
#             mapping = ggplot2::aes(
#                 x = Es_keV,
#                 y = PrDet,
#                 color = as.factor(y_cm)
#             ),
#             linewidth = 1
#         ) +
#         fggplot2::acet_grid(
#             cols = ggplot2::vars(contents),
#             labeller = ggplot2::as_labeller(c(
#                 "m"="Scrap Metal",
#                 "f"="Foodstuff"
#             ))
#         ) +
#         ggplot2::scale_x_continuous(limits = c(0,2000)) +
#         ggplot2::xlab("Source Energy (keV)") +
#         ggplot2::scale_y_continuous(
#             trans = "log10",
#             limits = c(1e-9, 1e-1),
#             n.breaks = 10,
#             labels = scales::trans_format("log10", scales::math_format(10^.x))
#         ) +
#         ggplot2::ylab("Probability (Log10 Scale)") +
#         ggplot2::scale_color_hue(
#             name = "Distance (cm): ",
#             breaks = c(0, -250, -500, -750, -1000, -1250, -1500),
#             labels = c("0     ", "250 ", "500 ", "750 ", "1000", "1250", "1500")
#         ) +
#         ggplot2::guides(color = ggplot2::guide_legend(nrow = 1)) +
#         ggplot2::ggtitle("Probability of Photon Detection") +
#         ggplot2::theme_bw() +
#         ggplot2::theme(
#             legend.position = "top",
#             legend.title = ggplot2::element_text(size=7),
#             legend.text = ggplot2::element_text(size=7),
#             strip.background = ggplot2::element_rect(fill = "white")
#         ),
#     source_spatial =
#         #####count_rate_data_discrete |>
#         ggplot2::ggplot() +
#         ggplot2::geom_line(mapping = ggplot2::aes(
#             x = y_cm,
#             y = count_rate,
#             color = source
#         )) +
#         ggplot2::facet_grid(
#             cols = ggplot2::vars(contents),
#             labeller = ggplot2::as_labeller(c(
#                 "m"="Scrap Metal",
#                 "f"="Foodstuff"
#             ))
#         ) +
#         ggplot2::scale_x_continuous(
#             limits = c(-1500, 1500)
#         ) +
#         ggplot2::xlab("Distance (cm)") +
#         ggplot2::scale_y_continuous(
#             trans = "log10",
#             limits = c(1e-7, 1e-1),
#             n.breaks = 7,
#             labels = scales::trans_format("log10", scales::math_format(10^.x))
#         ) +
#         ggplot2::ylab("Count Rate (cps / Bq)") +
#         ggplot2::scale_color_hue(name = "Source") +
#         ggplot2::ggtitle("Count Rate of Sources Approaching Detector") +
#         ggplot2::theme(
#             legend.position = "right",
#             legend.title = ggplot2::element_text(size=7),
#             legend.text = ggplot2::element_text(size=7)
#         ) +
#         ggplot2::theme_bw() +
#         ggplot2::theme(
#             strip.background = ggplot2::element_rect(fill = "white")
#         ),
#     intrinsic_efficiency =
#         #####intrinsic_data |>
#         ggplot2::ggplot(
#             data = _,
#             ggplot2::aes(x = E_keV, y = value, color = Type)) +
#         ggplot2::geom_point(shape = "o") +
#         ggplot2::xlab("Energy (keV)") +
#         ggplot2::scale_x_continuous(limits = c(0,2000)) +
#         ggplot2::ylab("Intrinsic Efficiency") +
#         ggplot2::scale_y_continuous(limits = c(0,1)) +
#         ggplot2::scale_color_hue(
#             name = "Type",
#             breaks = c("all", "closest", "isotropic"),
#             labels = c("Truck; y = all", "Truck; y=0 cm", "Bare Isotropic")) +
#         ggplot2::ggtitle("Measured Intrinsic Efficiency") +
#         ggplot2::theme_bw(),
#     lm_diagnostics = ggpubr::ggarrange(
#         ggpubr::ggarrange(
#             linear_model |>
#                 broom::augment() |>
#                 ggplot2::ggplot() +
#                 ggplot2::geom_histogram(
#                     mapping = ggplot2::aes(
#                         x = .std.resid,
#                         y = ggplot2::after_stat(density)
#                     ),
#                     bins = bin_number(
#                         broom::augment(linear_model)$.std.resid
#                         )
#                 ) +
#                 ggplot2::geom_line(
#                     data = dplyr::mutate(
#                         data.frame(abscissa = -80:40 / 20),
#                         ordinate = dnorm(abscissa)),
#                     mapping = aes(x = abscissa, y = ordinate)) +
#                 ggplot2::xlab("Relative Residuals") +
#                 ggplot2::ylab("") +
#                 ggplot2::ggtitle(
#                     "Distribution of Residuals",
#                     subtitle = "Line indicates standard normal distribution") +
#                 ggplot2::theme_bw(),
#             linear_model |>
#                 broom::augment() |>
#                 ggplot2::ggplot(ggplot2::aes(sample = .resid)) +
#                 ggplot2::geom_qq() +
#                 ggplot2::geom_qq_line() +
#                 ggplot2::xlab("Quantiles of Standard Normal") +
#                 ggplot2::ylab("Quantiles of Residuals") +
#                 ggplot2::ggtitle(
#                     "Quantile-Quantile Plot of Residuals",
#                     subtitle = "Line indicates normal distribution") +
#                 ggplot2::theme_bw(),
#             ncol = 2
#         ),
#         linear_model |>
#             broom::augment() |>
#             ggplot2::ggplot() +
#             ggplot2::geom_point(mapping = ggplot2::aes(
#                 x = .fitted,
#                 y = .resid,
#                 color = trans_kev,
#                 alpha = trans_cm
#             )) +
#             ggplot2::facet_grid(
#                 rows = ggplot2::vars(contents),
#                 labeller = ggplot2::as_labeller(c(
#                     "m"="Scrap Metal",
#                     "f"="Foodstuff"
#                 ))
#             ) +
#             ggplot2::xlab("Fitted Values of Ln(Efficiency)") +
#             ggplot2::ylab("Residuals of Ln(Efficiency)") +
#             ggplot2::ggtitle("Residuals vs. Fitted Values of Linear Model") +
#             ggplot2::scale_color_continuous(
#                 name = "Ln(Source\n\ \ \ Energy)"
#             ) +
#             ggplot2::scale_alpha_continuous(name = "Ln(Distance)") +
#             ggplot2::theme_bw() +
#             ggplot2::theme(
#                 legend.position = "top",
#                 legend.title = ggplot2::element_text(size=7),
#                 legend.text = ggplot2::element_text(size=7),
#                 strip.background = ggplot2::element_rect(fill = "white")
#             ),
#         nrow = 2
#     ) |>
#         ggpubr::annotate_figure(top = "Diagnostic Plots for Linear Model"),
#     lm_comparison =
#         summary_data |>
#         predict_efficiency(method = "lm") |>
#         ggplot2::ggplot() +
#         ggplot2::geom_line(
#             mapping = ggplot2::aes(
#                 x = Es_keV,
#                 y = efficiency,
#                 color = as.factor(y_cm)
#             ),
#             linewidth = 1
#         ) +
#         ggplot2::geom_point(
#             mapping = ggplot2::aes(
#                 x = Es_keV,
#                 y = PrDet,
#                 color = as.factor(y_cm)
#             ),
#             size = 1
#         ) +
#         ggplot2::facet_grid(
#             cols = ggplot2::vars(contents),
#             labeller = ggplot2::as_labeller(c(
#                 "m"="Scrap Metal",
#                 "f"="Foodstuff"
#             ))
#         ) +
#         ggplot2::scale_x_continuous(limits = c(0,2000)) +
#         ggplot2::xlab("Source Energy (keV)") +
#         ggplot2::scale_y_continuous(
#             trans = "log10",
#             limits = c(1e-9, 1e-1),
#             n.breaks = 10,
#             labels = scales::trans_format("log10", scales::math_format(10^.x))
#         ) +
#         ggplot2::ylab("Probability (Log10 Scale)") +
#         ggplot2::scale_color_hue(
#             name = "Distance (cm): ",
#             breaks = c(0, -250, -500, -750, -1000, -1250, -1500),
#             labels = c(
#                 "0     ",
#                 "250 ",
#                 "500 ",
#                 "750 ",
#                 "1000",
#                 "1250",
#                 "1500"
#             )
#         ) +
#         ggplot2::guides(color = ggplot2::guide_legend(nrow = 1)) +
#         ggplot2::ggtitle(
#             "Probability of Photon Detection",
#             subtitle = "Points denote MCNP values, lines denote fitted values.") +
#         ggplot2::theme_bw() +
#         ggplot2::theme(
#             legend.position = "top",
#             legend.title = ggplot2::element_text(size=7),
#             legend.text = ggplot2::element_text(size=7),
#             strip.background = ggplot2::element_rect(fill = "white")
#         ),
#     lm_contour =
#         rbind(
#             dplyr::mutate(data.frame(
#                 y_cm = rep(-50:50 * 10, times = 151),
#                 Es_keV = rep(50:200 * 10, each = 101)),
#                 contents = "m"
#             ),
#             dplyr::mutate(data.frame(
#                 y_cm = rep(-50:50 * 10, times = 151),
#                 Es_keV = rep(50:200 * 10, each = 101)),
#                 contents = "f"
#             )
#         ) |>
#         predict_efficiency(method = "lm") |>
#         ggplot2::ggplot(
#             ggplot2::aes(x = Es_keV, y = y_cm, z = 100*(efficiency))
#         ) +
#         ggplot2::facet_grid(
#             rows = ggplot2::vars(contents),
#             labeller = ggplot2::as_labeller(c(
#                 "m"="Scrap Metal",
#                 "f"="Foodstuff"
#             ))
#         ) +
#         ggplot2::geom_contour_filled(bins = 9) +
#         ggplot2::scale_fill_viridis_d(
#             name = "Efficiency (%)",
#             option = "F",
#             begin = 1,
#             end = 0) +
#         ggplot2::scale_x_continuous(expand = c(0,0)) +
#         ggplot2::xlab("Source Energy (keV)") +
#         ggplot2::scale_y_continuous(expand = c(0.,0.)) +
#         ggplot2::ylab("Truck Position (cm)") +
#         ggplot2::ggtitle(
#             "Detection Efficiency for Different\nSource Energies and Positions"
#         ) +
#         ggplot2::theme_bw() +
#         ggplot2::theme(
#             legend.position = "right",
#             legend.title = ggplot2::element_text(size=14),
#             legend.text = ggplot2::element_text(size=12),
#             strip.background = ggplot2::element_rect(fill = "white"),
#             panel.spacing = ggplot2::unit(1, "lines")
#         ),
#     lm_net_counts =
#         ######count_rate_data_lm |>
#         ggplot() +
#         ggplot2::geom_line(mapping = ggplot2::aes(
#             x = y_cm,
#             y = net_counts_cumulative,
#             color = source)) +
#         ggplot2::facet_grid(
#             cols = ggplot2::vars(contents),
#             labeller = ggplot2::as_labeller(c(
#                 "m"="Scrap Metal",
#                 "f"="Foodstuff"
#             ))
#         ) +
#         ggplot2::scale_color_hue(name = "Source") +
#         ggplot2::xlab("Distance (cm)") +
#         ggplot2::ylab("Net Counts (counts / Bq)") +
#         ggplot2::ggtitle("Net Counts of Source Along Trajectory") +
#         ggplot2::theme_bw() +
#         ggplot2::theme(
#             strip.background = ggplot2::element_rect(fill = "white")
#         ),
# ))}
#
# pmder_figs()
#
#     # Linear Model Diagnostics
#
#     # Linear Model Comparison
#
#     # Linear Model Contour Plot
#
#     # Linear model net counts
#
#     # Linear model time derivative
#     lm_time_derivative =
#         count_rate_data_lm |>
#         ggplot() +
#         geom_line(mapping = aes(
#             x = y_cm,
#             y = count_rate_derivative,
#             color = source)) +
#         facet_grid(
#             cols = vars(contents),
#             labeller = as_labeller(c("m"="Scrap Metal", "f"="Foodstuff"))) +
#         scale_color_hue(name = "Source") +
#         xlab("Distance (cm)") +
#         ylab("Count Rate Derivative (cps / Bq / s)") +
#         ggtitle("Count Rate Derivative for\nSources Along Trajectory") +
#         theme_bw() +
#         theme(
#             strip.background = element_rect(fill = "white"),
#             legend.position = "top"),
#     # Earth Model Diagnostics
#     earth_diagnostics = ggarrange(
#         ggarrange(
#             earth_model_aug |>
#                 ggplot() +
#                 geom_histogram(
#                     mapping = aes(x = .std.resid, y = after_stat(density)),
#                     bins = bin_number(earth_model_aug$.std.resid)) +
#                 geom_line(
#                     data = mutate(
#                         data.frame(abscissa = -100:50 / 20),
#                         ordinate = dnorm(abscissa)),
#                     mapping = aes(x = abscissa, y = ordinate)) +
#                 xlab("Relative Residuals") +
#                 ylab("") +
#                 ggtitle(
#                     "Distribution of Residuals",
#                     subtitle = "Line indicates standard normal distribution") +
#                 theme_bw(),
#             earth_model_aug |>
#                 ggplot(aes(sample = .resid)) +
#                 geom_qq() +
#                 geom_qq_line() +
#                 xlab("Quantiles of Standard Normal") +
#                 ylab("Quantiles of Residuals") +
#                 ggtitle(
#                     "Quantile-Quantile Plot of Residuals",
#                     subtitle = "Line indicates normal distribution") +
#                 theme_bw(),
#             ncol = 2
#         ),
#         earth_model_aug |>
#             ggplot() +
#             geom_point(mapping = aes(
#                 x = .fitted,
#                 y = .resid,
#                 color = Es_keV,
#                 alpha = y_cm
#             )) +
#             facet_grid(
#                 rows = vars(contents),
#                 labeller = as_labeller(c("m"="Scrap Metal", "f"="Foodstuff"))
#             ) +
#             xlab("Fitted Values of Ln(Efficiency)") +
#             ylab("Residuals of Ln(Efficiency)") +
#             ggtitle("Residuals vs. Fitted Values") +
#             scale_color_continuous(name = "Source\n\ \ \ Energy") +
#             scale_alpha_continuous(name = "Distance") +
#             theme_bw() +
#             theme(
#                 legend.position = "top",
#                 legend.title = element_text(size=7),
#                 legend.text = element_text(size=7),
#                 strip.background = element_rect(fill = "white")
#             ),
#         nrow = 2
#     ) |>
#         annotate_figure(top = "Diagnostic Plots for MARS Model"),
#     # Earth Comparison
#     earth_comparison = earth_comparison,
#     # Earth Model Contour Plot
#     earth_contour = rbind(
#         mutate(data.frame(
#             y_cm = rep(-50:50 * 10, times = 151),
#             Es_keV = rep(50:200 * 10, each = 101)), contents = "m"),
#         mutate(data.frame(
#             y_cm = rep(-50:50 * 10, times = 151),
#             Es_keV = rep(50:200 * 10, each = 101)), contents = "f")
#     ) |> predict_efficiency(method = "earth") |> ggplot(aes(x = Es_keV, y = y_cm, z = 100*(efficiency))) +
#         facet_grid(
#             rows = vars(contents),
#             labeller = as_labeller(c("m"="Scrap Metal", "f"="Foodstuff"))
#         ) +
#         geom_contour_filled(bins = 9) +
#         scale_fill_viridis_d(
#             name = "Efficiency (%)",
#             option = "F",
#             begin = 1,
#             end = 0) +
#         scale_x_continuous(expand = c(0,0)) +
#         xlab("Source Energy (keV)") +
#         scale_y_continuous(expand = c(0.,0.)) +
#         ylab("Truck Position (cm)") +
#         ggtitle(
#             "Detection Efficiency for Different\nSource Energies and Positions") +
#         theme_bw() +
#         theme(
#             legend.position = "right",
#             legend.title = element_text(size=14),
#             legend.text = element_text(size=12),
#             strip.background = element_rect(fill = "white"),
#             panel.spacing = unit(1, "lines")
#         ),
#     earth_count_rate =
#         count_rate_data_earth |>
#         ggplot() +
#         # geom_linerange(mapping = aes(
#         #     x = y_cm,
#         #     ymin = =u_count_per_decay,
#         #     ymax = u_count_per_decay,
#         #     color = source)) +
#         geom_line(mapping = aes(
#             x = y_cm,
#             y = count_rate,
#             color = source)) +
#         facet_grid(
#             cols = vars(contents),
#             labeller = as_labeller(c("m"="Scrap Metal", "f"="Foodstuff"))) +
#         # geom_point(
#         #     data = count_rate_data_discrete,
#         #     mapping = aes(x = y_cm, y = count_per_decay, color = source)) +
#         # geom_pointrange(
#         #     data = count_rate_data_discrete,
#         #     mapping = aes(
#         #         x = y_cm,
#         #         y = count_per_decay,
#         #         ymin = count_per_decay - u_count_per_decay,
#         #         ymax = count_per_decay + u_count_per_decay,
#         #         color = source),
#     #     size = 0.5) +
#     scale_x_continuous(
#         limits = c(-1500, 1500)
#     ) +
#         xlab("Distance (cm)") +
#         # scale_y_continuous(
#         #     trans = "log10",
#         #     limits = c(1e-5, 1e0),
#         #     n.breaks = 7,
#         #     labels = scales::trans_format("log10", scales::math_format(10^.x))) +
#         ylab("Count Rate (cps / Bq)") +
#         scale_color_hue(name = "Source") +
#         ggtitle("Count Rate Sources Approaching Detector") +
#         theme_bw() +
#         theme(
#             legend.position = "top",
#             legend.title = element_text(size=7),
#             legend.text = element_text(size=7),
#             strip.background = element_rect(fill = "white")
#         )
# )
#
#
# save(figures, file = "figures.rda")
