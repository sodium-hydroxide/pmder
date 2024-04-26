devtools::load_all()
library(ggplot2)


pr_det_pos <-
    summary_data |>
    dplyr::filter(PrDet > 0) |>
    dplyr::filter(Es_keV %in% c( 177.5, 350, 661.6, 1173, 2000)) |>
    dplyr::mutate(energy_label = Vectorize(function(.Es_keV, .y_m) {ifelse(
        .y_m == -15,
        paste("E = ", as.character(.Es_keV), " keV", sep = ""),
        NA
    )})(Es_keV, y_m)) |>
    ggplot(aes(
        x = sqrt(y_m^2 + 1.795),
        y = PrDet,
        linetype = as.factor(Es_keV),
        label = energy_label
    )) +
    facet_grid(
        rows = vars(contents),
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
        n.breaks = 9,
        labels = scales::trans_format(
            "log10",
            scales::math_format(10^.x)
        )
    ) +
    scale_x_continuous(
        trans = "log10",
        name = "Distance (m)",
        limits = c(1.2, 26),
        expand = c(0,0),
        n.breaks = 7
    ) +
    scale_linetype_manual(guide="none",values=rep(1,6)) +
    ggrepel::geom_text_repel(
        min.segment.length = 0, point.padding = 0, force = 0.1,
        size = 3,
        nudge_x = 0.175, nudge_y = 0., angle = 0, segment.size = 0.2,
        seed = 110
    ) +
    guides(color = guide_legend(ncol = 2)) +
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





# Save Images ----

file_name <- paste(
    getwd(),
    "/data_raw/4_figure_generation/gg/fig-results_pr_det_pos",
    sep=""
)

ggsave(
    paste(file_name, ".eps", sep=""),
    plot=pr_det_pos,
    device="eps",
    width=6,
    height=8,
    units="in",
    dpi=300,
    bg='transparent'
)
ggsave(
    paste(file_name, ".png", sep=""),
    plot=pr_det_pos,
    device="png",
    width=6,
    height=8,
    units="in",
    dpi=300,
    bg='transparent'
)
ggsave(
    paste(file_name, ".tiff", sep=""),
    plot=pr_det_pos,
    device="tiff",
    width=6,
    height= 8,
    units="in",
    dpi=300,
    bg='transparent'
)
