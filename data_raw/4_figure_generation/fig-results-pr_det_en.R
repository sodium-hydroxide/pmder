devtools::load_all()
library(ggplot2)

# Non Color Plot ----
pr_det_en <-
    summary_data |>
    dplyr::filter(PrDet >= 1e-8) |>
    dplyr::filter(y_m %in% c(0, -2, -5, -10, -15)) |>
    dplyr::mutate(position_label = Vectorize(function(.Es_keV, .y_m) {ifelse(
        .Es_keV == 2000,
        paste("y = ", as.character(.y_m), " m", sep = ""),
        NA
    )})(Es_keV, y_m)) |>
    ggplot(aes(
        x=Es_keV,
        y=PrDet,
        linetype=as.factor(y_m),
        label=as.factor(position_label)
    )) +
    facet_grid(
        rows=vars(contents),
        labeller=as_labeller(c("m"="Scrap Metal", "f"="Foodstuff"))
    ) +
    geom_point(size=1.) +
    geom_line(linewidth=0.5) +
    scale_y_continuous(
        name="Pr{Detection}",
        trans="log10",
        limits=c(8e-9, 2e-2),
        expand=c(0,0),
        n.breaks=7,
        labels=scales::trans_format("log10",scales::math_format(10^.x))
    ) +
    scale_x_continuous(
        trans="log10",
        name=latex2exp::TeX("$E_{\\gamma, source}\\ (keV)$"),
        limits=c(40,2999),
        expand=c(0,0),
        n.breaks=9
    ) +
    scale_linetype_manual(guide="none",values=rep(1,6)) +
    ggrepel::geom_text_repel(
        min.segment.length = 0, point.padding = 0, force = 0,
        size = 3,
        nudge_x = 0.1, nudge_y = -0.2, angle = -60, segment.size = 0.2
    ) +
    theme_bw() +
    theme(
        legend.position="right",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        strip.background=element_rect(
            fill="white"
        )
    ) +
    ggtitle("Absolute Detection Efficiency")


# Color Plot ----

# Save Images ----

file_name <- paste(
    getwd(),
    "/data_raw/4_figure_generation/gg/fig-results_pr_det_en",
    sep=""
)

ggsave(
    paste(file_name, ".eps", sep=""),
    plot=pr_det_en,
    device="eps",
    width=6,
    height=8,
    units="in",
    dpi=300,
    bg='transparent'
)
ggsave(
    paste(file_name, ".png", sep=""),
    plot=pr_det_en,
    device="png",
    width=6,
    height=8,
    units="in",
    dpi=300,
    bg='transparent'
)
ggsave(
    paste(file_name, ".tiff", sep=""),
    plot=pr_det_en,
    device="tiff",
    width=6,
    height= 8,
    units="in",
    dpi=300,
    bg='transparent'
)
