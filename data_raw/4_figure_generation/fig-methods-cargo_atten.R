library(ggplot2)

cargo_atten <-
    data.frame(
        E_MeV = c(
            0.001, 0.001, 0.0015, 0.0015, 0.002, 0.002, 0.003,
            0.003, 0.004, 0.004, 0.005, 0.005, 0.006, 0.006, 0.008,
            0.008, 0.01, 0.01, 0.015, 0.015, 0.02, 0.02, 0.03,
            0.03, 0.04, 0.04, 0.05, 0.05, 0.06, 0.06, 0.08, 0.08,
            0.1, 0.1, 0.15, 0.15, 0.2, 0.2, 0.3, 0.3, 0.4, 0.4,
            0.5, 0.5, 0.6, 0.6, 0.8, 0.8, 1, 1, 1.25, 1.25, 1.5,
            1.5, 2, 2, 0.00103542, 0.00103542, 0.0010721,
            0.0010721, 0.00114237, 0.00114237, 0.001305, 0.001305,
            0.0018389, 0.0018389, 0.0021455, 0.0021455, 0.002472,
            0.002472, 0.0028224, 0.0028224, 0.0032029, 0.0032029,
            0.0036074, 0.0036074, 0.0040381, 0.0040381, 0.0059892,
            0.0059892, 0.006539, 0.006539, 0.007112, 0.007112,
            0.0010098, 0.0010098, 0.0010197, 0.0010197, 0.00103119,
            0.00103119, 0.0010428, 0.0010428, 0.00111565,
            0.00111565, 0.0011936, 0.0011936, 0.0096586, 0.0096586,
            0.00100404, 0.00100404, 0.0010081, 0.0010081,
            0.0083328, 0.0083328
        ),
        Material = as.factor(c(
            "Foodstuff", "Scrap Metal", "Foodstuff", "Scrap Metal",
            "Foodstuff", "Scrap Metal", "Foodstuff", "Scrap Metal",
            "Foodstuff", "Scrap Metal", "Foodstuff", "Scrap Metal",
            "Foodstuff", "Scrap Metal", "Foodstuff", "Scrap Metal",
            "Foodstuff", "Scrap Metal", "Foodstuff", "Scrap Metal",
            "Foodstuff", "Scrap Metal", "Foodstuff", "Scrap Metal",
            "Foodstuff", "Scrap Metal", "Foodstuff", "Scrap Metal",
            "Foodstuff", "Scrap Metal", "Foodstuff", "Scrap Metal",
            "Foodstuff", "Scrap Metal", "Foodstuff", "Scrap Metal",
            "Foodstuff", "Scrap Metal", "Foodstuff", "Scrap Metal",
            "Foodstuff", "Scrap Metal", "Foodstuff", "Scrap Metal",
            "Foodstuff", "Scrap Metal", "Foodstuff", "Scrap Metal",
            "Foodstuff", "Scrap Metal", "Foodstuff", "Scrap Metal",
            "Foodstuff", "Scrap Metal", "Foodstuff", "Scrap Metal",
            "Foodstuff", "Scrap Metal", "Foodstuff", "Scrap Metal",
            "Foodstuff", "Scrap Metal", "Foodstuff", "Scrap Metal",
            "Foodstuff", "Scrap Metal", "Foodstuff", "Scrap Metal",
            "Foodstuff", "Scrap Metal", "Foodstuff", "Scrap Metal",
            "Foodstuff", "Scrap Metal", "Foodstuff", "Scrap Metal",
            "Foodstuff", "Scrap Metal", "Foodstuff", "Scrap Metal",
            "Foodstuff", "Scrap Metal", "Foodstuff", "Scrap Metal",
            "Foodstuff", "Scrap Metal", "Foodstuff", "Scrap Metal",
            "Foodstuff", "Scrap Metal", "Foodstuff", "Scrap Metal",
            "Foodstuff", "Scrap Metal", "Foodstuff", "Scrap Metal",
            "Foodstuff", "Scrap Metal", "Foodstuff", "Scrap Metal",
            "Foodstuff", "Scrap Metal", "Foodstuff", "Scrap Metal"
        )),
        mu_cm = c(
            1757.68436640017, 2934.59565593209, 597.874324343362,
            77779.5680915577, 263.108550789681, 530.287994665669,
            84.5054946879393, 181.830484934846, 37.1220965378754,
            83.6717753906472, 19.2197556576055, 45.5492706074377,
            11.1570209359588, 55.0095948045152, 4.72794645499967,
            93.9551568497537, 2.44495610222524, 55.36754658186,
            0.777923655094276, 18.4862663753955, 0.381716299925961,
            8.30864892289049, 0.181077907891391, 2.6427526232671,
            0.131059027348193, 1.17346808582831, 0.111614499000265,
            0.633922804299104, 0.101618931142086,
            0.390914604933908, 0.0909529727995325,
            0.194149477428227, 0.0846674408966, 0.121994677801205,
            0.0747520801127955, 0.0652874576855549,
            0.068071066640188, 0.0488804954173088,
            0.0589577579301638, 0.037004704101759,
            0.0527497703673374, 0.0317147378349514,
            0.0481457980847012, 0.0284123901805244,
            0.0445143905348407, 0.0260261805077695,
            0.0390948726690168, 0.0226407832172357,
            0.0351473037254325, 0.020265280624407,
            0.0314266415241883, 0.0180871482695365,
            0.0286000914495581, 0.0165077959345551,
            0.0245495234540621, 0.0144119178202005,
            1675.42769686071, 8234.86099037987, 1593.84685051627,
            13725.6265344653, 1427.22359121216, 24244.6040367249,
            1050.32189195335, 48589.2932524994, 370.970083028677,
            25422.4473039222, 238.33069706415, 479.810874666379,
            179.753201675378, 365.878828154937, 116.344920246784,
            243.702399723152, 74.7286800470196, 161.930769231225,
            56.4060302671592, 122.207813855207, 36.5551992251371,
            82.2193079584069, 11.2440984709526, 64.1337662312931,
            9.42251446428816, 59.7548879209116, 7.58878571054552,
            133.117012881307, 1734.90822210026, 4399.70795767898,
            1711.95674852177, 5881.67575720039, 1685.25063191402,
            7601.65656694799, 1658.41473528191, 9339.60062275038,
            1489.20923270671, 20244.7879151883, 1308.50750861473,
            31913.4131649554, 2.83594222861884, 61.8446224719225,
            1748.29501711735, 3538.38803327637, 1738.85918590739,
            4330.71854587743, 4.34755035121363, 92.557470141899
        )
    )|>
    ggplot2::ggplot() +
    ggplot2::geom_line(
        mapping = ggplot2::aes(
            x = 1000 * E_MeV,
            y = mu_cm,
            linetype = Material
        )
    ) +
    ggplot2::scale_x_continuous(
        name = "Energy (keV)",
        trans = "log10",
        limits = c(1,2000),
        expand = c(0,0),
        labels = scales::trans_format(
            "log10",
            scales::math_format(10^.x)
        )
    ) +
    ggplot2::scale_y_continuous(
        name = "Attenuation Coefficienty (1 / cm)",
        trans = "log10",
        limits = c(1e-2, 1e5),
        n.breaks = 10,
        labels = scales::trans_format(
            "log10",
            scales::math_format(10^.x)
        )
    ) +
    ggplot2::ggtitle("Attenuation Coefficients of Truck Cargo") +
    ggplot2::theme_bw() +
    ggplot2::theme(
        legend.position = "bottom",
        strip.background = ggplot2::element_rect(
            fill = "white"
        )
    )


# Save Images ----

file_name <- paste(
    getwd(),
    "/data_raw/4_figure_generation/gg/fig-methods_cargo_atten",
    sep=""
)

ggsave(
    paste(file_name, ".eps", sep=""),
    plot=cargo_atten,
    device="eps",
    width=5,
    height=5,
    units="in",
    dpi=300,
    bg='transparent'
)
ggsave(
    paste(file_name, ".png", sep=""),
    plot=cargo_atten,
    device="png",
    width=5,
    height=5,
    units="in",
    dpi=300,
    bg='transparent'
)
ggsave(
    paste(file_name, ".tiff", sep=""),
    plot=cargo_atten,
    device="tiff",
    width=5,
    height= 5,
    units="in",
    dpi=300,
    bg='transparent'
)

