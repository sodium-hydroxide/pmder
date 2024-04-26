library(ggplot2)

pvt_atten <-
    data.frame(
        E_MeV=c(
            1.0E-03, 1.5E-03, 2.0E-03, 3.0E-03, 4.0E-03, 5.0E-03, 6.0E-03,
            8.0E-03, 1.0E-02, 1.5E-02, 2.0E-02, 3.0E-02, 4.0E-02, 5.0E-02,
            6.0E-02, 8.0E-02, 1.0E-01, 1.5E-01, 2.0E-01, 3.0E-01, 4.0E-01,
            5.0E-01, 6.0E-01, 8.0E-01, 1.0E+00, 1.2E+00, 1.5E+00, 2.0E+00,
            3.0E+00, 4.0E+00, 5.0E+00, 6.0E+00, 8.0E+00, 1.0E+01, 1.5E+01,
            2.0E+01),
        mum=c(
            2.024E+03, 6.409E+02, 2.770E+02, 8.270E+01, 3.461E+01, 1.753E+01,
            1.005E+01, 4.220E+00, 2.204E+00, 7.705E-01, 4.358E-01, 2.647E-01,
            2.194E-01, 1.997E-01, 1.881E-01, 1.736E-01, 1.635E-01, 1.458E-01,
            1.331E-01, 1.155E-01, 1.034E-01, 9.443E-02, 8.732E-02, 7.668E-02,
            6.894E-02, 6.166E-02, 5.611E-02, 4.810E-02, 3.848E-02, 3.282E-02,
            2.907E-02, 2.641E-02, 2.290E-02, 2.069E-02, 1.770E-02, 1.624E-02)#,
        # muenm=c(
        #     2.022E+03, 6.397E+02, 2.760E+02, 8.203E+01, 3.407E+01, 1.707E+01,
        #     9.650E+00, 3.883E+00, 1.903E+00, 5.158E-01, 2.059E-01, 6.210E-02,
        #     3.256E-02, 2.424E-02, 2.180E-02, 2.172E-02, 2.310E-02, 2.650E-02,
        #     2.876E-02, 3.110E-02, 3.197E-02, 3.218E-02, 3.204E-02, 3.128E-02,
        #     3.027E-02, 2.894E-02, 2.766E-02, 2.542E-02, 2.214E-02, 1.992E-02,
        #     1.835E-02, 1.718E-02, 1.558E-02, 1.455E-02, 1.309E-02, 1.236E-02)
    ) |>
    dplyr::mutate(
        E_keV=E_MeV * 1000,
        mu=mum * 2.250000
    ) |>
    dplyr::filter(E_keV <= 2000) |>
    dplyr::select(-mum, -E_MeV) |>
    ggplot() +
    geom_line(
        mapping=aes(
            x=E_keV,
            y=mu
        )
    ) +
    scale_x_continuous(
        name="Photon Energy (keV)",
        transform="log10",
        limits=c(1,2250),
        expand=c(0,0),
        n.breaks=15
    ) +
    scale_y_continuous(
        name="Interaction Coefficient (1/cm)",
        transform="log10",
        limits=c(1e-1,1e4),
        expand=c(0,0),
        n.breaks=7,
        labels=scales::trans_format(
            "log10",
            scales::math_format(10^.x)
        )
    ) +
    scale_linetype_discrete(
        name="",
        breaks=c("mu", "muen"),
        labels=c(
            "Attenuation Coefficient",
            "Energy Absorption Coefficient"
        )
    ) +
    ggtitle(
        "Attenuation Coefficient in PVT",
        subtitle=latex2exp::TeX("$\\rho=2.25\\ g\\ cm^{-3}$")
    ) +
    theme_bw() +
    theme(
        legend.position="bottom",
        strip.background=element_rect(
            fill="white"
        )
    )


# Save Images ----

file_name <- paste(
    getwd(),
    "/data_raw/4_figure_generation/gg/fig-methods_pvt_atten",
    sep=""
)

ggsave(
    paste(file_name, ".eps", sep=""),
    plot=pvt_atten,
    device="eps",
    width=5,
    height=5,
    units="in",
    dpi=300,
    bg='transparent'
)
ggsave(
    paste(file_name, ".png", sep=""),
    plot=pvt_atten,
    device="png",
    width=5,
    height=5,
    units="in",
    dpi=300,
    bg='transparent'
)
ggsave(
    paste(file_name, ".tiff", sep=""),
    plot=pvt_atten,
    device="tiff",
    width=5,
    height= 5,
    units="in",
    dpi=300,
    bg='transparent'
)

