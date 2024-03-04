# #' Figures related to research
# #'
# #' Code for creating figures used in this research project
# #'
# #'
#
# # library(tidyverse)
# # library(ggpubr)

# figures <- list(
#     # Star differences
#     star_differences =
#         spectral_data |>
#         mutate(
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
#         select(
#             sLF1,
#             sLF2,
#             sLF4,
#             sLF8,
#             sRF1,
#             sRF2,
#             sRF4,
#             sRF8
#         ) |>
#         pivot_longer(
#             cols = c("sLF1", "sLF2", "sLF4","sLF8", "sRF1","sRF2", "sRF4","sRF8"),
#             names_to = "tally",
#             values_to = "RelativeDifference"
#         ) |>
#         filter(RelativeDifference <= 0.5) |>
#         filter(!is.nan(RelativeDifference)) |>
#         ggplot() +
#         geom_histogram(
#             mapping = aes(x = RelativeDifference, y = after_stat(density)),
#             binwidth = 0.01,
#             na.rm = TRUE
#         ) +
#         xlab("Relative Difference") +
#         scale_x_continuous(limits = c(0,0.5), expand = c(0,0)) +
#         ylab(NULL) +
#         scale_y_continuous(expand = c(0,0)) +
#         ggtitle(
#             "Distribution of Differences",
#             subtitle = "Particle and Radiant Energy Tallies"
#         ) +
#         theme_bw(),
#     # Parity Differences
#     parity_differences =
#         spectral_data |>
#         mutate(
#             sLF1 = 2 * (LF1 - RF1) / (LF1 + RsF1),
#             sLF2 = 2 * (LF2 - RF2) / (LF2 + RsF2),
#             sLF4 = 2 * (LF4 - RF4) / (LF4 + RsF4),
#             sLF8 = 2 * (LF8 - RF8) / (LF8 + RsF8)
#         ) |>
#         select(
#             sLF1,
#             sLF2,
#             sLF4,
#             sLF8,
#         ) |>
#         pivot_longer(
#             cols = c("sLF1", "sLF2", "sLF4","sLF8"),
#             names_to = "tally",
#             values_to = "RelativeDifference"
#         ) |>
#         ggplot(aes(x = RelativeDifference, y = after_stat(density))) +
#         geom_histogram(
#             binwidth = 0.01, na.rm = TRUE
#         ) +
#         xlab("Relative Difference") +
#         scale_x_continuous(limits = c(-2,2), expand = c(0,0)) +
#         ylab(NULL) +
#         scale_y_continuous(expand = c(0,0)) +
#         ggtitle("Differences between Left and Right Tallies") +
#         theme_bw(),
#     # Probability of Reaching Detector
#     prob_reach =
#         summary_data |>
#         ggplot() +
#         geom_line(
#             mapping = aes(x = Es_keV, y = PrReach, color = as.factor(y_cm)),
#             linewidth = 1
#         ) +
#         facet_grid(
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
#     # Interior Attenuation Coefficients
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
#         ggplot() +
#         geom_line(
#             mapping = aes(x = Es_keV, y = PrDet, color = as.factor(y_cm)),
#             linewidth = 1
#         ) +
#         facet_grid(
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
#         ggtitle("Probability of Photon Detection") +
#         theme_bw() +
#         theme(
#             legend.position = "top",
#             legend.title = element_text(size=7),
#             legend.text = element_text(size=7),
#             strip.background = element_rect(fill = "white")
#         ),
#     # Spatial Behavior of sources
#     source_spatial =
#         count_rate_data_discrete |>
#         ggplot() +
#         geom_line(mapping = aes(x = y_cm, y = count_rate, color = source)) +
#         facet_grid(
#             cols = vars(contents),
#             labeller = as_labeller(c("m"="Scrap Metal", "f"="Foodstuff"))
#         ) +
#         scale_x_continuous(
#             limits = c(-1500, 1500)
#         ) +
#         xlab("Distance (cm)") +
#         scale_y_continuous(
#             trans = "log10",
#             limits = c(1e-7, 1e-1),
#             n.breaks = 7,
#             labels = scales::trans_format("log10", scales::math_format(10^.x))
#         ) +
#         ylab("Count Rate (cps / Bq)") +
#         scale_color_hue(name = "Source") +
#         ggtitle("Count Rate of Sources Approaching Detector") +
#         theme(
#             legend.position = "right",
#             legend.title = element_text(size=7),
#             legend.text = element_text(size=7)
#         ) +
#         theme_bw() +
#         theme(strip.background = element_rect(fill = "white")),
#     # Intrinsic efficiency
#     intrinsic_efficiency =
#         intrinsic_data |> ggplot(
#         data = _,
#         aes(x = E_keV, y = value, color = Type)) +
#         geom_point(shape = "o") +
#         xlab("Energy (keV)") +
#         scale_x_continuous(limits = c(0,2000)) +
#         ylab("Intrinsic Efficiency") +
#         scale_y_continuous(limits = c(0,1)) +
#         scale_color_hue(
#             name = "Type",
#             breaks = c("all", "closest", "isotropic"),
#             labels = c("Truck; y = all", "Truck; y=0 cm", "Bare Isotropic")) +
#         ggtitle("Measured Intrinsic Efficiency") +
#         theme_bw(),
#     # Linear Model Diagnostics
#     lm_diagnostics = ggarrange(
#         ggarrange(
#             linear_model_aug |>
#                 ggplot() +
#                 geom_histogram(
#                     mapping = aes(x = .std.resid, y = after_stat(density)),
#                     bins = bin_number(linear_model_aug$.std.resid)) +
#                 geom_line(
#                     data = mutate(
#                         data.frame(abscissa = -80:40 / 20),
#                         ordinate = dnorm(abscissa)),
#                     mapping = aes(x = abscissa, y = ordinate)) +
#                 xlab("Relative Residuals") +
#                 ylab("") +
#                 ggtitle(
#                     "Distribution of Residuals",
#                     subtitle = "Line indicates standard normal distribution") +
#                 theme_bw(),
#             linear_model_aug |>
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
#         linear_model_aug |>
#             ggplot() +
#             geom_point(mapping = aes(
#                 x = .fitted,
#                 y = .resid,
#                 color = trans_kev,
#                 alpha = trans_cm
#             )) +
#             facet_grid(
#                 rows = vars(contents),
#                 labeller = as_labeller(c("m"="Scrap Metal", "f"="Foodstuff"))
#             ) +
#             xlab("Fitted Values of Ln(Efficiency)") +
#             ylab("Residuals of Ln(Efficiency)") +
#             ggtitle("Residuals vs. Fitted Values of Linear Model") +
#             scale_color_continuous(name = "Ln(Source\n\ \ \ Energy)") +
#             scale_alpha_continuous(name = "Ln(Distance)") +
#             theme_bw() +
#             theme(
#                 legend.position = "top",
#                 legend.title = element_text(size=7),
#                 legend.text = element_text(size=7),
#                 strip.background = element_rect(fill = "white")
#             ),
#         nrow = 2
#     ) |>
#         annotate_figure(top = "Diagnostic Plots for Linear Model"),
#     # Linear Model Comparison
#     lm_comparison =
#         summary_data |>
#         predict_efficiency(method = "lm") |>
#         ggplot() +
#         geom_line(
#             mapping = aes(x = Es_keV, y = efficiency, color = as.factor(y_cm)),
#             linewidth = 1
#         ) +
#         geom_point(
#             mapping = aes(x = Es_keV, y = PrDet, color = as.factor(y_cm)),
#             size = 1
#         ) +
#         facet_grid(
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
#             labels = c("0     ", "250 ", "500 ", "750 ", "1000", "1250", "1500")) +
#         guides(color = guide_legend(nrow = 1)) +
#         ggtitle(
#             "Probability of Photon Detection",
#             subtitle = "Points denote MCNP values, lines denote fitted values.") +
#         theme_bw() +
#         theme(
#             legend.position = "top",
#             legend.title = element_text(size=7),
#             legend.text = element_text(size=7),
#             strip.background = element_rect(fill = "white")
#         ),
#     # Linear Model Contour Plot
#     lm_contour = rbind(
#         mutate(data.frame(
#             y_cm = rep(-50:50 * 10, times = 151),
#             Es_keV = rep(50:200 * 10, each = 101)), contents = "m"),
#         mutate(data.frame(
#             y_cm = rep(-50:50 * 10, times = 151),
#             Es_keV = rep(50:200 * 10, each = 101)), contents = "f")
#     ) |> predict_efficiency(method = "lm") |> ggplot(aes(x = Es_keV, y = y_cm, z = 100*(efficiency))) +
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
#     # Linear model net counts
#     lm_net_counts =
#         count_rate_data_lm |>
#         ggplot() +
#         geom_line(mapping = aes(
#             x = y_cm,
#             y = net_counts_cumulative,
#             color = source)) +
#         facet_grid(
#             cols = vars(contents),
#             labeller = as_labeller(c("m"="Scrap Metal", "f"="Foodstuff"))) +
#         scale_color_hue(name = "Source") +
#         xlab("Distance (cm)") +
#         ylab("Net Counts (counts / Bq)") +
#         ggtitle("Net Counts of Source Along Trajectory") +
#         theme_bw() +
#         theme(
#             strip.background = element_rect(fill = "white")),
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
