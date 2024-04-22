devtools::load_all()
library(dplyr)
library(ggplot2)


mars_contour <-
    rbind(
        mutate(
            data.frame(
                y_m = rep(pracma::linspace(-5, 5, n = 101), times = 151),
                Es_keV = rep(pracma::linspace(20, 2000, n = 151), each = 101)
            ),
            contents = "m"
        ),
        mutate(
            data.frame(
                y_m = rep(pracma::linspace(-5, 5, n = 101), times = 151),
                Es_keV = rep(pracma::linspace(20, 2000, n = 151), each = 101)
            ),
            contents = "f"
        )
    ) |>
    predict_efficiency(method = "mars") |>
    mutate(contents = as.factor(contents)) |>
    ggplot(
        aes(x = Es_keV, y = y_m, z = 100*(efficiency))
    ) +
    facet_grid(
        rows = vars(contents),
        labeller = as_labeller(c(
            "m"="Scrap Metal",
            "f"="Foodstuff"
        ))
    ) +
    geom_contour_filled(bins = 10) +
    scale_fill_viridis_d(
        name = "Efficiency (%)",
        option = "F",
        begin = 1,
        end = 0.5
    ) +
    scale_x_continuous(
        name = "Source Energy (keV)",
        expand = c(0,0)
    ) +
    scale_y_continuous(
        name = "Truck Position (m)",
        expand = c(0.,0.)
    ) +
    ggtitle(
        "Detection Efficiency for Different\nSource Energies and Positions") +
    theme_bw() +
    theme(
        legend.position = "right",
        legend.title = element_text(size=14),
        legend.text = element_text(size=12),
        strip.background = element_rect(fill = "white"),
        panel.spacing = unit(1, "lines")
    )


ggsave(
    paste(
        getwd(),
        "/",
        "data_raw/4_figure_generation/gg/",
        "fig-discussion-mars_contour.png",
        sep = ""
    ),
    plot = mars_contour,
    width = 7,
    height = 5,
    units = "in",
    dpi = 1200
)
