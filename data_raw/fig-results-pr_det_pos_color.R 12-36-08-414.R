devtools::load_all()
library(ggplot2)



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


