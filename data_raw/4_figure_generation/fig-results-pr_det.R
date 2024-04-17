devtools::load_all()
library(ggplot2)

pr_det_en <-
    summary_data |>
    dplyr::filter(PrDet > 0) |>
    ggplot(aes(
        x = Es_keV,
        y = PrDet,
        color = as.factor(y_m)
    )) +
    facet_grid(
        cols = vars(contents),
        labeller = as_labeller(
            c("m"="Scrap Metal", "f"="Foodstuff")
        )
    ) +
    geom_point(size = 1) +
    geom_line(linewidth = 0.5) +
    scale_y_continuous(
        name = "Pr{Detection}",
        trans = "log10",
        limits = c(8e-11, 1e-1),
        expand = c(0,0),
        n.breaks = 10,
        labels = scales::trans_format(
            "log10",
            scales::math_format(10^.x)
        )
    ) +
    scale_x_continuous(
        trans = "log10",
        name = "Source Energy (keV)",
        limits = c(20,2090),
        expand = c(0,0),
        labels = (
            \(x) parse(
                text=gsub("e", " %*% 10^", scales::scientific_format()(x))
            )
        )
    ) +
    scale_colour_viridis_d(
        name = "Position (m): ",
        breaks = c(
            0, -2.50,
            -5.00, -10.00, -15.00
        ),
        labels = c(
            "0.0",
            "2.5", "5.0", "10.0", "15.0"
        ),
        direction = -1,
        begin = 0.1,
        end = 0.8,
        option = "G"
    ) +
    guides(color = guide_legend(nrow = 1)) +
    theme_bw() +
    theme(
        legend.position = "top",
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        strip.background = element_rect(
            fill = "white"
        )
    )




pr_det_pos <-
    summary_data |>
    dplyr::filter(PrDet > 0) |>
    dplyr::filter(
        (Es_keV %in% c(110, 200, 511, 1022, 1173, 2000)) |
            (Es_keV > 650 & Es_keV < 700)
    ) |>
    ggplot(aes(
        x = sqrt(y_m^2 + 1.79),
        y = PrDet,
        shape = as.factor(Es_keV),
        linetype = as.factor(Es_keV)
    )) +
    facet_grid(
        cols = vars(contents),
        labeller = as_labeller(
            c("m"="Scrap Metal", "f"="Foodstuff")
        )
    ) +
    geom_point(size = 1.5) +
    geom_line(linewidth = 0.5) +
    scale_y_continuous(
        name = "Pr{Detection}",
        trans = "log10",
        limits = c(8e-9, 1.8e-2),
        expand = c(0,0),
        n.breaks = 10,
        labels = scales::trans_format(
            "log10",
            scales::math_format(10^.x)
        )
    ) +
    scale_x_continuous(
        trans = "log10",
        name = "Distance (m)",
        #limits = c(20,2090),
        expand = c(0,0),
        # labels = (
        #     \(x) parse(
        #         text=gsub("e", " %*% 10^", scales::scientific_format()(x))
        #     )
        # ),
        n.breaks = 7
    ) +
    # scale_colour_viridis_d(
    #     name = "Source Energy (keV): ",
    #     breaks = c(110, 200, 511, 661.6, 1022, 1173, 2000),
    #     direction = -1,
    #     begin = 0.1,
    #     end = 0.8,
    #     option = "G"
    # ) +
    scale_shape_manual(
        name = latex2exp::TeX("$E_s\ (keV)$"),#"Source Energy (keV): ",
        breaks = rev(c(110, 200, 511, 661.6, 1022, 1173, 2000)),
        values = rev(c(15, 15, 16, 16, 17, 17, 1))
    ) +
    scale_linetype_manual(
        name = latex2exp::TeX("$E_s\ (keV)$"),#"Source Energy (keV): ",
        breaks = rev(c(110, 200, 511, 661.6, 1022, 1173, 2000)),
        values = rev(c(
            "solid", "longdash", "solid", "longdash", "solid", "longdash",
            "solid"
        ))
    ) +
    guides(color = guide_legend(ncol = 2)) +
    theme_bw() +
    theme(
        legend.position = "right",
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        strip.background = element_rect(
            fill = "white"
        )
    )


ggsave(
    paste(
        wd(),
        "/",
        "data_raw/4_figure_generation/",
        "fig-.png"
    ),
    plot = ,
    width = 7,
    height = 5,
    units = "in",
    dpi = 1200
)
