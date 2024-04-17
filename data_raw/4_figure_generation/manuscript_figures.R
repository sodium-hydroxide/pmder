
# Results ----

color_style = "F"




#----





lm_comparison <-
    summary_data |>
    dplyr::mutate(y_cm = y_m * 100) |>
    predict_efficiency(method = "lm") |>
    ggplot2::ggplot() +
    ggplot2::geom_line(
        mapping = ggplot2::aes(
            x = Es_keV,
            y = efficiency,
            color = as.factor(y_m)
        ),
        linewidth = 1
    ) +
    ggplot2::geom_point(
        mapping = ggplot2::aes(
            x = Es_keV,
            y = PrDet,
            color = as.factor(y_m)
        ),
        size = 1
    ) +
    ggplot2::facet_grid(
        cols = ggplot2::vars(contents),
        labeller = ggplot2::as_labeller(c(
            "m"="Scrap Metal",
            "f"="Foodstuff"
        ))
    ) +
    ggplot2::scale_x_continuous(limits = c(0,2000)) +
    ggplot2::xlab("Source Energy (keV)") +
    ggplot2::scale_y_continuous(
        trans = "log10",
        limits = c(1e-9, 1e-1),
        n.breaks = 10,
        labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    ggplot2::ylab("Probability (Log10 Scale)") +
    ggplot2::scale_colour_viridis_d(
        name = "Position (m): ",
        breaks = c(
            0, -2.50, -5.00, -7.50, -10.00, -12.50, -15.00
        ),
        labels = c(
            "0.0", "2.5", "5.0", "7.5 ", "10.0",
            "12.5", "15.0"
        ),
        direction = -1,
        option ="F"
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 1)) +
    ggplot2::ggtitle(
        "Probability of Photon Detection",
        subtitle = "Points denote MCNP values, lines denote fitted values.") +
    ggplot2::theme_bw() +
    ggplot2::theme(
        legend.position = "top",
        legend.title = ggplot2::element_text(size=7),
        legend.text = ggplot2::element_text(size=7),
        strip.background = ggplot2::element_rect(fill = "white")
    )


ggplot2::ggsave(
    "img/lm_comparison.png",
    plot = lm_comparison,
    width = 7,
    height = 5,
    units = "in",
    dpi = 1200
)



count_rate_derivative <-
    rbind(
        dplyr::mutate(
            predict_count_rate(
                data.frame(
                    y_cm = rep(
                        pracma::linspace(-300, 300, n = 1000),
                        times = 2
                    ),
                    contents = rep(c("m","f"), each = 1000)
                ),
                photon_energy_keV = 661.66,
                yield = 0.851,
                speed_cm_s = 447,
                count_rate_derivative = TRUE,
                method = "lm"
            ),
            source = "Cs-137"
        ),
        dplyr::mutate(
            predict_count_rate(
                data.frame(
                    y_cm = rep(
                        pracma::linspace(-300, 300, n = 1000),
                        times = 2
                    ),
                    contents = rep(c("m","f"), each = 1000)
                ),
                photon_energy_keV = c(1173, 1332),
                yield = c(0.998, 0.999),
                speed_cm_s = 447,
                count_rate_derivative = TRUE,
                method = "lm"
            ),
            source = "Co-60"
        ),
        dplyr::mutate(
            predict_count_rate(
                data.frame(
                    y_cm = rep(
                        pracma::linspace(-300, 300, n = 1000),
                        times = 2
                    ),
                    contents = rep(c("m","f"), each = 1000)
                ),
                photon_energy_keV = 510.9,
                yield = 2,
                speed_cm_s = 447,
                count_rate_derivative = TRUE,
                method = "lm"
            ),
            source = "Beta-Plus"
        )
    ) |>
    dplyr::mutate(
        source = as.factor(source),
        y_m = y_cm / 100
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_line(mapping = ggplot2::aes(
        x = y_m,
        y = count_rate_derivative,
        linetype = source)) +
    ggplot2::facet_grid(
        cols = ggplot2::vars(contents),
        labeller = ggplot2::as_labeller(c(
            "m"="Scrap Metal",
            "f"="Foodstuff"
        ))
    ) +
    ggplot2::scale_linetype_discrete(name = "Source") +
    ggplot2::scale_x_continuous(
        name = "Distance (m)",
        limits = c(-3,3.1),
        expand = c(0,0)
    ) +
    ggplot2::scale_y_continuous(
        name = "dr/dt (cps/s/Bq)",
        limits = c(-0.06,0.06),
        expand = c(0,0)
    ) +
    ggplot2::ggtitle(
        "Time Derivative of Count Rate Along Trajectory"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
        strip.background = ggplot2::element_rect(fill = "white")
    )

ggplot2::ggsave(
    "img/count_rate_derivative.png",
    plot = count_rate_derivative,
    width = 7,
    height = 5,
    units = "in",
    dpi = 1200
)


lm_log_derivative <-
    predict_count_rate(
        data.frame(
            y_cm = pracma::linspace(-300, 300, n = 1000),
            contents = rep("m", each = 1000)
        ),
        photon_energy_keV = 661.66,
        yield = 0.851,
        speed_cm_s = 447,
        count_rate_derivative = TRUE,
        method = "lm"
    ) |>
    dplyr::mutate(
        y_m = y_cm / 100,
        log_deriv = count_rate_derivative / count_rate
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_line(mapping = ggplot2::aes(
        x = y_m,
        y = log_deriv
    )) +
    ggplot2::scale_x_continuous(
        name = "Distance (m)",
        limits = c(-3,3.1),
        expand = c(0,0)
    ) +
    ggplot2::scale_y_continuous(
        name = "d ln(r)/dt (1/s)",
        limits = c(-6,6),
        expand = c(0,0)
    ) +
    ggplot2::ggtitle(
        "Logarithmic Time Derivative of Count Rate Along Trajectory"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
        strip.background = ggplot2::element_rect(fill = "white")
    )

earth_contour <-
    rbind(
        dplyr::mutate(data.frame(
            y_cm = rep(-50:50 * 10, times = 151),
            Es_keV = rep(50:200 * 10, each = 101)), contents = "m"),
        dplyr::mutate(data.frame(
            y_cm = rep(-50:50 * 10, times = 151),
            Es_keV = rep(50:200 * 10, each = 101)), contents = "f")
    ) |>
    predict_efficiency(method = "earth") |>
    dplyr::mutate(y_m = y_cm / 100) |>
    ggplot2::ggplot(
        ggplot2::aes(x = Es_keV, y = y_m, z = 100*(efficiency))
    ) +
    ggplot2::facet_grid(
        rows = ggplot2::vars(contents),
        labeller = ggplot2::as_labeller(c(
            "m"="Scrap Metal",
            "f"="Foodstuff"
        ))
    ) +
    ggplot2::geom_contour_filled(bins = 7) +
    ggplot2::scale_fill_viridis_d(
        name = "Efficiency (%)",
        option = color_style,
        begin = 1,
        end = 0.5) +
    ggplot2::scale_x_continuous(
        name = "Source Energy (keV)", expand = c(0,0)
    ) +
    ggplot2::scale_y_continuous(
        name = "Truck Position (m)", expand = c(0.,0.)
    ) +
    ggplot2::ggtitle(
        "Detection Efficiency for Different\nSource Energies and Positions") +
    ggplot2::theme_bw() +
    ggplot2::theme(
        legend.position = "right",
        legend.title = ggplot2::element_text(size=14),
        legend.text = ggplot2::element_text(size=12),
        strip.background = ggplot2::element_rect(fill = "white"),
        panel.spacing = ggplot2::unit(1, "lines")
    )


ggplot2::ggsave(
    "img/earth_contour.png",
    plot = earth_contour,
    width = 7,
    height = 5,
    units = "in",
    dpi = 1200
)
