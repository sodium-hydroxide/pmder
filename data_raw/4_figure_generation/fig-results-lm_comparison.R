devtools::load_all()
library(ggplot2)

lm_comparison <- ggpubr::ggarrange(
    linear_model$aug |>
        dplyr::mutate(
            Es_keV = round(exp(log.Es_keV.), digits = 2),
            r_m = exp(log.r_m.)
        ) |>
        dplyr::filter(Es_keV %in% c(350, 661.6, 1173, 2000)) |>
        dplyr::mutate(Es_keV = as.factor(Es_keV)) |>
        ggplot()+
        geom_line(
            mapping = aes(
                x = r_m,
                y = (.resid),
                linetype = Es_keV
            )
        ) +
        geom_hline(yintercept = 0, alpha=0.5) +
        facet_grid(
            cols = vars(contents),
            labeller = as_labeller(
                c("m"="Scrap Metal", "f"="Foodstuff")
            )
        ) +
        scale_x_continuous(
            name = "Distance (m)",
            transform = "log10",
            expand=c(0,0),
            n.breaks = 7
        ) +
        scale_y_continuous(
            name = "Residuals",
            expand = c(0,0)
        )+
        scale_linetype_manual(
            name = latex2exp::TeX("$E_s\\ (keV)$"),
            values=c(1,2,3,4)
        ) +
        ggtitle("Residuals vs. Distance of Linear Model") +
        theme(
            legend.position = "right",
            legend.title = element_text(size=9),
            legend.text = element_text(size=8),
            strip.background = element_rect(fill = "white")
        ) +
        theme_bw(),
    linear_model$aug |>
        ggplot() +
        geom_histogram(
            aes(x = .resid, y = after_stat(density)),
            bins = ( \(x)
                     ceiling(0.5 * diff(range(x))
                             * pracma::nthroot(length(x), 3)/ IQR(x))
            )(linear_model$aug$.resid),
            linewidth=0.5,
            color="black",
            fill="grey",
            alpha=0.0
        ) +
        geom_line(
            data = data.frame(
                abscissa = -200:200/100,
                ordinate = dnorm(-200:200/100)
            ),
            mapping = aes(x=abscissa, y=ordinate),
            linewidth=1
        ) +
        scale_x_continuous(
            name = "Studentized Residual",
            expand=c(0,0)
        ) +
        scale_y_continuous(
            name="",
            expand=c(0,0),
            limits=c(0,1.4),
            n.breaks = 7
        )+
        ggtitle(
            "Distribution of Residuals from Linear Model",
            subtitle = "Solid Line Indicates Standard Normal Distribution"
        )+
        theme_bw(),
    nrow=2
)

# Save Images ----

file_name <- paste(
    getwd(),
    "/data_raw/4_figure_generation/gg/fig-results_lm_comparison",
    sep=""
)

ggsave(
    paste(file_name, ".eps", sep=""),
    plot=lm_comparison,
    device="eps",
    width=6,
    height=8,
    units="in",
    dpi=300,
    bg='transparent'
)
ggsave(
    paste(file_name, ".png", sep=""),
    plot=lm_comparison,
    device="png",
    width=6,
    height=8,
    units="in",
    dpi=300,
    bg='transparent'
)
ggsave(
    paste(file_name, ".tiff", sep=""),
    plot=lm_comparison,
    device="tiff",
    width=6,
    height= 8,
    units="in",
    dpi=300,
    bg='transparent'
)

