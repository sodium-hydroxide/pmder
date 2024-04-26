devtools::load_all()
library(ggplot2)
library(dplyr)

speed <- 4.46

cs137_data <-
    data.frame(
        y_m = rep(
            pracma::linspace(-5, 5, n = 1000),
            times = 2
        ),
        contents = rep(c("m","f"), each = 1000)
    ) |>
    predict_count_rate(
        input_data = _,
        photon_energy_keV = 661.66,
        yield = 0.851,
        speed_m_s = speed,
        count_rate_derivative = TRUE,
        method = "lm"
    ) |>
    mutate(
        source = "Cs-137"
    )

co60_data <-
    data.frame(
        y_m = rep(
            pracma::linspace(-5, 5, n = 1000),
            times = 2
        ),
        contents = rep(c("m","f"), each = 1000)
    ) |>
    predict_count_rate(
        input_data = _,
        photon_energy_keV = c(1173, 1332),
        yield = c(0.998, 0.999),
        speed_m_s = speed,
        count_rate_derivative = TRUE,
        method = "lm"
    ) |>
    mutate(
        source = "Co-60"
    )

betaplus_data <-
    data.frame(
        y_m = rep(
            pracma::linspace(-5, 5, n = 1000),
            times = 2
        ),
        contents = rep(c("m","f"), each = 1000)
    ) |>
    predict_count_rate(
        input_data = _,
        photon_energy_keV = 510.9,
        yield = 2,
        speed_m_s = speed,
        count_rate_derivative = TRUE,
        method = "lm"
    ) |>
    mutate(
        source = "Beta-plus"
    )

count_rate_derivative <-
    rbind(cs137_data, co60_data, betaplus_data) |>
    mutate(source = as.factor(source)) |>
    ggplot(aes(
        x = y_m,
        y = count_rate_derivative,
        linetype = source
    )) +
    facet_grid(
        rows = vars(contents),
        labeller = as_labeller(c(
            "m"="Scrap Metal",
            "f"="Foodstuff"
        ))
    ) +
    geom_line() +
    scale_linetype_discrete(
        name = "Source",
        breaks = c(
            "Co-60",
            "Beta-plus",
            "Cs-137"
        ),
        labels = c(
            latex2exp::TeX("$^{60}Co$"),
            latex2exp::TeX("$\\beta^{+}$"),
            latex2exp::TeX("$^{137}Cs$")
        )
    ) +
    scale_x_continuous(
        name = "Position (m)",
        limits = c(-5,5.9),
        expand = c(0,0)
    ) +
    scale_y_continuous(
        name = latex2exp::TeX(
            "$d\\hat{r}_n / dt\\ \\ (cps\\ Bq^{-1}\\ s^{-1})$"
        ),
        #limits = c(-0.035,0.035),
        expand = c(0,0)
    ) +
    ggtitle(
        "Time Derivative of Count Rate Along Trajectory",
        subtitle = (paste(
            "Constant speed of",
            speed,
            "m/s"
        ))
    ) +
    theme_bw() +
    theme(
        strip.background = element_rect(fill = "white")
    )

# Save Images ----

file_name <- paste(
    getwd(),
    "/data_raw/4_figure_generation/gg/fig-results_count_rate_derivative",
    sep=""
)

ggsave(
    paste(file_name, ".eps", sep=""),
    plot=count_rate_derivative,
    device="eps",
    width=5,
    height=5,
    units="in",
    dpi=300,
    bg='transparent'
)
ggsave(
    paste(file_name, ".png", sep=""),
    plot=count_rate_derivative,
    device="png",
    width=5,
    height=5,
    units="in",
    dpi=300,
    bg='transparent'
)
ggsave(
    paste(file_name, ".tiff", sep=""),
    plot=count_rate_derivative,
    device="tiff",
    width=5,
    height= 8,
    units="in",
    dpi=300,
    bg='transparent'
)
