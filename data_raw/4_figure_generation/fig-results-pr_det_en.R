devtools::load_all()
library(ggplot2)

color_style <- "F"

pr_det_en <-
    summary_data |>
    dplyr::filter(PrDet <= 0) #|>
    ggplot2::ggplot(ggplot2::aes(
        x = Es_keV,
        y = PrDet,
        color = as.factor(y_m)
    )) +
    ggplot2::geom_point(size = 1) +
    ggplot2::geom_line(linewidth = 0.5) +
    #ggplot2::geom_line(mapping = ggplot2::aes(linetype = as.factor(y_m)), linewidth = 0.5) +
    ggplot2::geom_errorbar(
        mapping = ggplot2::aes(
            ymin = PrDet - uPrDet,
            ymax = PrDet + uPrDet
        ),
        width = 20
    ) +
    ggplot2::facet_grid(
        cols = ggplot2::vars(contents),
        labeller = ggplot2::as_labeller(
            c("m"="Scrap Metal", "f"="Foodstuff")
        )
    ) +
    ggplot2::scale_x_continuous(
        name = "Source Energy (keV)",
        #limits = c(0,2090),
        expand = c(0,0)
    ) +
    ggplot2::scale_y_continuous(
        name = "Probability (Log10 Scale)",
        #trans = "log10",
        #limits = c(1e-9, 1e-1),
        n.breaks = 10,
        #labels = scales::trans_format(
        #    "log10",
        #    scales::math_format(10^.x)
        #)
    ) +
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
        option = color_style
    ) +
    # ggplot2::scale_linetype_discrete(
    #     name = "Position (m): ",
    #     breaks = c(
    #         0, -2.50, -5.00, -7.50, -10.00, -12.50, -15.00
    #     ),
    #     labels = c(
    #         "0.0", "2.5", "5.0", "7.5 ", "10.0",
    #         "12.5", "15.0"
    #     )
    # ) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 1)) +
    #ggplot2::ggtitle("Probability of Photon Detection") +
    ggplot2::theme_bw() +
    ggplot2::theme(
        legend.position = "top",
        legend.title = ggplot2::element_text(size=7),
        legend.text = ggplot2::element_text(size=7),
        strip.background = ggplot2::element_rect(
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
