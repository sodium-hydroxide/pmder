devtools::load_all()
library(ggplot2)

position_scales <- dplyr::mutate(data.frame(
    pos = -1 * c(0, 1, 2, 5, 10, 15),
    shape = c(15, 15, 16, 16, 17, 17),
    line = c(
        "solid", "longdash", "solid", "longdash", "solid", "longdash"
    )),
    label = as.character(abs(pos)))

pr_det_en <-
    summary_data |>
    dplyr::filter(PrDet > 1e-9) |>
    dplyr::filter(y_m %in% position_scales$pos) |>
    ggplot(aes(
        x = Es_keV,
        y = PrDet,
        shape = as.factor(y_m),
        linetype = as.factor(y_m)
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
        limits = c(40,2090),
        expand = c(0,0),
        n.breaks = 7
    ) +
    scale_shape_manual(
        name = latex2exp::TeX(
            "$y\\ (m)$"
        ),
        breaks = (position_scales$pos),
        values = (position_scales$shape),
        labels = position_scales$label
    ) +
    scale_linetype_manual(
        name = latex2exp::TeX(
            "$y\\ (m)$"
        ),
        breaks = (position_scales$pos),
        values = (position_scales$line),
        labels = position_scales$label
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
    ggtitle("Absolute Detection Efficiency")


# Add color if wanted
pr_det_en <-
    pr_det_en +
    scale_colour_viridis_d(
        name = latex2exp::TeX(
            "$y\\ (m)$"
        ),
        breaks = position_scales$pos,
        labels = position_scales$label,
        direction = -1,
        begin = 0.1,
        end = 0.8,
        option = "G"
    )

ggsave(
    paste(
        getwd(),
        "/",
        "data_raw/4_figure_generation/gg/",
        "fig-results_pr_det_en.png",
        sep = ""
    ),
    plot = ,
    width = 7,
    height = 5,
    units = "in",
    dpi = 1200
)
