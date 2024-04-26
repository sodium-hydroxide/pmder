devtools::load_all()
library(ggplot2)



energy_scales <- data.frame(
    energy = c( 177.5, 350, 511, 661.6, 1173, 2000),
    shape = c(15, 15, 16, 16, 17, 17),
    line = c(
        "solid", "longdash", "solid", "longdash", "solid", "longdash"
    )
)

pr_det_pos <-
    summary_data |>
    dplyr::filter(PrDet > 0) |>
    dplyr::filter(Es_keV %in% energy_scales$energy) |>
    ggplot(aes(
        x = sqrt(y_m^2 + 1.79),
        y = PrDet,
        color = as.factor(Es_keV),
        ymin = PrDet - uPrDet,
        ymax = PrDet + uPrDet
    )) +
    facet_grid(
        cols = vars(contents),
        labeller = as_labeller(
            c("m"="Scrap Metal", "f"="Foodstuff")
        )
    ) +
    geom_point(size = 1.5) +
    geom_line(linewidth = 0.5) +
    geom_errorbar() +
    scale_y_continuous(
        name = "Pr{Detection}",
        trans = "log10",
        limits = c(8e-9, 1.8e-2),
        expand = c(0,0),
        n.breaks = 9,
        labels = scales::trans_format(
            "log10",
            scales::math_format(10^.x)
        )
    ) +
    scale_x_continuous(
        trans = "log10",#scales::compose_trans("log10", "reverse"),
        name = "Distance (m)",
        limits = c(1.2,17),#c(17, 1.2),
        expand = c(0,0),
        n.breaks = 8
    ) +
    scale_color_viridis_d(
        name = latex2exp::TeX(
            "$E_{\\gamma, source}\\ (keV)$"
        ),
        breaks = rev(energy_scales$energy),
        direction = -1,
        begin = 0.1,
        end = 0.8,
        option = "G"
    ) +
    guides(color = guide_legend(ncol = 1)) +
    theme_bw() +
    theme(
        legend.position = "right",
        legend.title = element_text(size=9),
        legend.text = element_text(size=8),
        strip.background = element_rect(
            fill = "white"
        )
    ) +
    ggtitle("Absolute Detection Efficiency")


ggsave(
    paste(
        getwd(),
        "/",
        "data_raw/4_figure_generation/gg/",
        "fig-results_pr_det_pos_color.png",
        sep = ""
    ),
    plot = ,
    width = 7,
    height = 5,
    units = "in",
    dpi = 1200
)


linear_model$aug |>
    mutate(y_m = round(sqrt(exp(2 * log.r_m.) - 1.795^2), digits = 1)) |>
    filter(y_m %in%  c(0, 1.0, 2, 5, 10, 15)) |>
    ggplot() +
    geom_point(aes(
        x = exp(log.Es_keV.),
        y = exp(log.PrDet.),
        color = as.factor(y_m)
    ), size = 0.5) +
    geom_line(aes(
        x = exp(log.Es_keV.),
        y = exp(.fitted),
        color = as.factor(y_m)
    ), linewidth = 1) +
    facet_grid(
        cols = vars(contents),
        labeller = as_labeller(
            c("m"="Scrap Metal", "f"="Foodstuff")
        )
    ) +
    scale_y_continuous(
        name = "Pr{Detection}",
        trans = "log10",
        limits = c(8e-8, 1.8e-2),
        expand = c(0,0),
        n.breaks = 7,
        labels = scales::trans_format(
            "log10",
            scales::math_format(10^.x)
        )
    ) +
    scale_x_continuous(
        trans = "log10",
        name = latex2exp::TeX(
            "$E_{\\gamma, source}\\ (keV)$"
        ),
        limits = c(300,2000),
        #expand = c(0,0),
        n.breaks = 7
    ) +
    scale_colour_viridis_d(
        name = latex2exp::TeX(
            "$y\\ (m)$"
        ),
        breaks = c(0, 1.0, 2, 5, 10, 15),
        direction = 1,
        begin = 0.1,
        end = 0.8,
        option = "G"
    ) +
    theme_bw() +
    theme(
        legend.position = "right",
        legend.title = element_text(size=9),
        legend.text = element_text(size=8),
        strip.background = element_rect(
            fill = "white"
        )
    ) +
    ggtitle(
        "Absolute Detection Efficiency",
        subtitle = "Points Indicate MCNP Values\nLines Indicate Model Values"
    )


mars_model$aug |>
    mutate(y_m = round(sqrt(exp(2 * log.r_m.) - 1.795^2), digits = 1)) |>
    filter(y_m %in%  c(0, 1.0, 2, 5, 10, 15)) |>
    ggplot() +
    geom_point(aes(
        x = exp(log.Es_keV.),
        y = exp(log.PrDet.),
        color = as.factor(y_m)
    ), size = 0.5) +
    geom_line(aes(
        x = exp(log.Es_keV.),
        y = exp(.fitted),
        color = as.factor(y_m)
    ), linewidth = 1) +
    facet_grid(
        cols = vars(contents),
        labeller = as_labeller(
            c("m"="Scrap Metal", "f"="Foodstuff")
        )
    ) +
    scale_y_continuous(
        name = "Pr{Detection}",
        trans = "log10",
        limits = c(8e-8, 1.8e-2),
        expand = c(0,0),
        n.breaks = 7,
        labels = scales::trans_format(
            "log10",
            scales::math_format(10^.x)
        )
    ) +
    scale_x_continuous(
        trans = "log10",
        name = latex2exp::TeX(
            "$E_{\\gamma, source}\\ (keV)$"
        ),
        #limits = c(300,2000),
        #expand = c(0,0),
        n.breaks = 7
    ) +
    scale_colour_viridis_d(
        name = latex2exp::TeX(
            "$y\\ (m)$"
        ),
        breaks = c(0, 1.0, 2, 5, 10, 15),
        direction = 1,
        begin = 0.1,
        end = 0.8,
        option = "G"
    ) +
    theme_bw() +
    theme(
        legend.position = "right",
        legend.title = element_text(size=9),
        legend.text = element_text(size=8),
        strip.background = element_rect(
            fill = "white"
        )
    ) +
    ggtitle(
        "Absolute Detection Efficiency",
        subtitle = "Points Indicate MCNP Values\nLines Indicate Model Values"
    )


