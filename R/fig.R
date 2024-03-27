#' Return figures used for pmder data visualization
#'
#' @param fig_name string, Name of the figure.
#' @param color_style string, color to be passed to scale_colour_viridis
#'
#' @export
#'
#' @examples fig("prob_det_energy") will return the efficiency as a function of the energy
#' @examples fig("") will return a list of potential figures
fig <- function(fig_name, color_style = "plasma") {

    if (fig_name == "pvt_atten_coeff") {
        plt <-
            data.frame(
                E_MeV = c(
                    1.0E-03, 1.5E-03, 2.0E-03, 3.0E-03, 4.0E-03, 5.0E-03, 6.0E-03,
                    8.0E-03, 1.0E-02, 1.5E-02, 2.0E-02, 3.0E-02, 4.0E-02, 5.0E-02,
                    6.0E-02, 8.0E-02, 1.0E-01, 1.5E-01, 2.0E-01, 3.0E-01, 4.0E-01,
                    5.0E-01, 6.0E-01, 8.0E-01, 1.0E+00, 1.2E+00, 1.5E+00, 2.0E+00,
                    3.0E+00, 4.0E+00, 5.0E+00, 6.0E+00, 8.0E+00, 1.0E+01, 1.5E+01,
                    2.0E+01),
                mum = c(
                    2.024E+03, 6.409E+02, 2.770E+02, 8.270E+01, 3.461E+01, 1.753E+01,
                    1.005E+01, 4.220E+00, 2.204E+00, 7.705E-01, 4.358E-01, 2.647E-01,
                    2.194E-01, 1.997E-01, 1.881E-01, 1.736E-01, 1.635E-01, 1.458E-01,
                    1.331E-01, 1.155E-01, 1.034E-01, 9.443E-02, 8.732E-02, 7.668E-02,
                    6.894E-02, 6.166E-02, 5.611E-02, 4.810E-02, 3.848E-02, 3.282E-02,
                    2.907E-02, 2.641E-02, 2.290E-02, 2.069E-02, 1.770E-02, 1.624E-02),
                muenm = c(
                    2.022E+03, 6.397E+02, 2.760E+02, 8.203E+01, 3.407E+01, 1.707E+01,
                    9.650E+00, 3.883E+00, 1.903E+00, 5.158E-01, 2.059E-01, 6.210E-02,
                    3.256E-02, 2.424E-02, 2.180E-02, 2.172E-02, 2.310E-02, 2.650E-02,
                    2.876E-02, 3.110E-02, 3.197E-02, 3.218E-02, 3.204E-02, 3.128E-02,
                    3.027E-02, 2.894E-02, 2.766E-02, 2.542E-02, 2.214E-02, 1.992E-02,
                    1.835E-02, 1.718E-02, 1.558E-02, 1.455E-02, 1.309E-02, 1.236E-02)
            ) |>
            dplyr::mutate(
                E_keV = E_MeV * 1000,
                mu = mum * 2.250000,
                muen = muenm * 2.250000
            ) |>
            dplyr::filter(E_keV <= 2000) |>
            dplyr::select(-mum,-muenm, -E_MeV) |>
            tidyr::pivot_longer(
                cols = c("mu", "muen"),
                names_to = "type",
                values_to = "mu"
            ) |>
            dplyr::mutate(type = as.factor(type)) |>
            ggplot2::ggplot() +
            ggplot2::geom_line(
                mapping = ggplot2::aes(
                    x = E_keV,
                    y = mu,
                    linetype = type
                )
            ) +
            ggplot2::scale_x_continuous(
                name = "Photon Energy (keV)",
                transform = "log10",
                limits = c(1,2150),
                expand = c(0,0),
                n.breaks = 15
            ) +
            ggplot2::scale_y_continuous(
                name = "Interaction Coefficient (1/cm)",
                transform = "log10",
                limits = c(1e-2,1e4),
                expand = c(0,0),
                n.breaks = 7,
                labels = scales::trans_format(
                    "log10",
                    scales::math_format(10^.x)
                )
            ) +
            ggplot2::scale_linetype_discrete(
                name = "",
                breaks = c("mu", "muen"),
                labels = c(
                    "Linear Attenuation Coefficient",
                    "Energy Absorption Coefficient"
                )
            ) +
            ggplot2::ggtitle(
                "Interaction Coefficients in PVT"
            ) +
            ggplot2::theme_bw() +
            ggplot2::theme(
                legend.position = "bottom",
                strip.background = ggplot2::element_rect(
                    fill = "white"
                )
            )
    }

    if (fig_name == "interior_atten_coeff") {
        plt <-
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
    }

    if (fig_name == "histories_needed") {
        history_number <- Vectorize(function(
        energy,
        truckPosition) {

            if (abs(truckPosition) < 300) {
                if ((energy < 0.5) | (energy > 1.3)) {
                    historiesRequired <- 1e8
                }
                else {
                    historiesRequired <- 4e8
                }
            }
            else if (abs(truckPosition) < 800) {
                if (energy < 0.6) {
                    historiesRequired <- 1e9
                }
                else {
                    historiesRequired <- 1e8
                }
            }
            else if(abs(truckPosition) < 1300) {
                if (energy < 0.6) {
                    historiesRequired <- 3e9
                }
                else if (energy < 1.2) {
                    historiesRequired <- 3e8
                }
                else {
                    historiesRequired <- 1e8
                }
            }
            else {
                if (energy < 1.0) {
                    historiesRequired <- 2.5e9
                }
                else {
                    historiesRequired <- 3e9
                }
            }
            return(historiesRequired)

            return(bit64::integer64(historiesRequired))
        })

        plt <-
            data.frame(
                energy = rep(10:2000,times = 4),
                position = rep(c(0,500, 1000, 1500), each = 1991),
                type = as.factor(rep(
                    c(
                        "0 cm ≤ |y| < 300 cm",
                        "300 cm ≤ |y| < 800 cm",
                        "800 cm ≤ |y| < 1300 cm",
                        "1300 cm ≤ |y| < 2000 cm"
                    ),
                    each = 1991
                ))
            ) |>
            dplyr::mutate(
                number = history_number(energy / 1000, position)
            ) |>
            ggplot2::ggplot() +
            ggplot2::geom_line(
                mapping = ggplot2::aes(
                    x = energy,
                    y = number
                )
            ) +
            ggplot2::facet_wrap(
                ~ type, nrow = 2
            ) +
            ggplot2::scale_x_continuous(
                name = "Energy (keV)",
                limits = c(1,2090),
                expand = c(0,0)
            ) +
            ggplot2::scale_y_continuous(
                name = "Histories Needed",
                transform = "log10",
                limits = c(5e7, 5e9),
                expand = c(0,0),
                n.breaks = 5,
                labels = btools::scientific_label
            ) +
            ggplot2::ggtitle("Particle Histories Needed for MCNP") +
            ggplot2::theme_bw() +
            ggplot2::theme(
                strip.background = ggplot2::element_rect(
                    fill = "white"
                )
            )
    }

    else if (fig_name == "star_differences") {

        plt <-
            spectral_data |>
            dplyr::mutate(
                sLF1 =
                    2 *
                    ((LF1 * 1e-3 * Ed_keV) - LsF1) /
                    ((LF1 * 1e-3 * Ed_keV) + LsF1),
                sLF2 =
                    2 *
                    ((LF2 * 1e-3 * Ed_keV) - LsF2) /
                    ((LF2 * 1e-3 * Ed_keV) + LsF2),
                sLF4 =
                    2 *
                    ((LF4 * 1e-3 * Ed_keV) - LsF4) /
                    ((LF4 * 1e-3 * Ed_keV) + LsF4),
                sLF8 =
                    2 *
                    ((LF8 * 1e-3 * Ed_keV) - LsF8) /
                    ((LF8 * 1e-3 * Ed_keV) + LsF8),
                sRF1 =
                    2 *
                    ((RF1 * 1e-3 * Ed_keV) - RsF1) /
                    ((RF1 * 1e-3 * Ed_keV) + RsF1),
                sRF2 =
                    2 *
                    ((RF2 * 1e-3 * Ed_keV) - RsF2) /
                    ((RF2 * 1e-3 * Ed_keV) + RsF2),
                sRF4 =
                    2 *
                    ((RF4 * 1e-3 * Ed_keV) - RsF4) /
                    ((RF4 * 1e-3 * Ed_keV) + RsF4),
                sRF8 =
                    2 *
                    ((RF8 * 1e-3 * Ed_keV) - RsF8) /
                    ((RF8 * 1e-3 * Ed_keV) + RsF8)
            ) |>
            dplyr::select(
                sLF1,
                sLF2,
                sLF4,
                sLF8,
                sRF1,
                sRF2,
                sRF4,
                sRF8
            ) |>
            tidyr::pivot_longer(
                cols = c(
                    "sLF1", "sLF2", "sLF4","sLF8", "sRF1","sRF2", "sRF4","sRF8"
                ),
                names_to = "tally",
                values_to = "RelativeDifference"
            ) |>
            dplyr::filter(RelativeDifference <= 0.5) |>
            dplyr::filter(!is.nan(RelativeDifference)) |>
            ggplot2::ggplot() +
            ggplot2::geom_histogram(
                mapping = ggplot2::aes(
                    x = RelativeDifference,
                    y = ggplot2::after_stat(density)
                ),
                binwidth = 0.01,
                na.rm = TRUE
            ) +
            ggplot2::scale_x_continuous(
                name = "Relative Difference",
                limits = c(0,0.5), expand = c(0,0)
            ) +
            ggplot2::scale_y_continuous(
                name = NULL,
                expand = c(0,0)
            ) +
            ggplot2::ggtitle(
                "Distribution of Differences",
                subtitle = "Particle and Radiant Energy Tallies"
            ) +
            ggplot2::theme_bw()
    }

    else if (fig_name == "parity_differences") {

        plt <-
            spectral_data |>
            dplyr::mutate(
                sLF1 = 2 * (LF1 - RF1) / (LF1 + RsF1),
                sLF2 = 2 * (LF2 - RF2) / (LF2 + RsF2),
                sLF4 = 2 * (LF4 - RF4) / (LF4 + RsF4),
                sLF8 = 2 * (LF8 - RF8) / (LF8 + RsF8)
            ) |>
            dplyr::select(
                sLF1,
                sLF2,
                sLF4,
                sLF8,
            ) |>
            tidyr::pivot_longer(
                cols = c("sLF1", "sLF2", "sLF4","sLF8"),
                names_to = "tally",
                values_to = "RelativeDifference"
            ) |>
            ggplot2::ggplot(ggplot2::aes(
                x = RelativeDifference,
                y = ggplot2::after_stat(density)
            )) +
            ggplot2::geom_histogram(
                binwidth = 0.01, na.rm = TRUE
            ) +
            ggplot2::scale_x_continuous(
                name = "Relative Difference",
                limits = c(-2,2), expand = c(0,0)
            ) +
            ggplot2::scale_y_continuous(
                name = NULL,
                expand = c(0,0)
            ) +
            ggplot2::ggtitle("Differences between Left and Right Tallies") +
            ggplot2::theme_bw()
    }

    else if (fig_name == "prob_reach") {

        plt <-
            summary_data |>
            ggplot2::ggplot(ggplot2::aes(
                x = Es_keV,
                y = PrReach,
                color = as.factor(y_cm)
            )) +
            ggplot2::geom_point(size = 1) +
            ggplot2::geom_line(linewidth = 0.5) +
            ggplot2::geom_errorbar(
                mapping = ggplot2::aes(
                    ymin = PrReach - uPrReach,
                    ymax = PrReach + uPrReach
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
                limits = c(0,2090),
                expand = c(0,0)
            ) +
            ggplot2::scale_y_continuous(
                name = "Probability (Log10 Scale)",
                trans = "log10",
                limits = c(1e-9, 1e-1),
                n.breaks = 10,
                labels = scales::trans_format(
                    "log10",
                    scales::math_format(10^.x)
                )
            ) +
            ggplot2::scale_colour_viridis_d(
                name = "Distance (cm): ",
                breaks = c(
                    0, -250, -500, -750, -1000, -1250, -1500
                ),
                labels = c(
                    "0     ", "250 ", "500 ", "750 ", "1000",
                    "1250", "1500"
                ),
                direction = -1,
                option = color_style
            ) +
            ggplot2::guides(color = ggplot2::guide_legend(nrow = 1)) +
            ggplot2::ggtitle(
                "Probability of Photon Reaching Detector"
            ) +
            ggplot2::theme_bw() +
            ggplot2::theme(
                legend.position = "top",
                legend.title = ggplot2::element_text(size=7),
                legend.text = ggplot2::element_text(size=7),
                strip.background = ggplot2::element_rect(
                    fill = "white"
                )
            )
    }

    else if (fig_name == "prob_det_energy") {
        plt <-
            summary_data |>
            ggplot2::ggplot(ggplot2::aes(
                x = Es_keV,
                y = PrDet,
                color = as.factor(y_cm)
            )) +
            ggplot2::geom_point(size = 1) +
            ggplot2::geom_line(linewidth = 0.5) +
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
                limits = c(0,2090),
                expand = c(0,0)
            ) +
            ggplot2::scale_y_continuous(
                name = "Probability (Log10 Scale)",
                trans = "log10",
                limits = c(1e-9, 1e-1),
                n.breaks = 10,
                labels = scales::trans_format(
                    "log10",
                    scales::math_format(10^.x)
                )
            ) +
            ggplot2::scale_colour_viridis_d(
                name = "Distance (cm): ",
                breaks = c(
                    0, -250, -500, -750, -1000, -1250, -1500
                ),
                labels = c(
                    "0     ", "250 ", "500 ", "750 ", "1000",
                    "1250", "1500"
                ),
                direction = -1,
                option = color_style
            ) +
            ggplot2::guides(color = ggplot2::guide_legend(nrow = 1)) +
            ggplot2::ggtitle(
                "Probability of Photon Detection"
            ) +
            ggplot2::theme_bw() +
            ggplot2::theme(
                legend.position = "top",
                legend.title = ggplot2::element_text(size=7),
                legend.text = ggplot2::element_text(size=7),
                strip.background = ggplot2::element_rect(
                    fill = "white"
                )
            )
    }

    else if (fig_name == "prob_det_position") {
        plt <-
            summary_data |>
            dplyr::mutate(y_cm = sqrt((y_cm ^ 2) + (179.5 ^2))) |>
            ggplot2::ggplot(ggplot2::aes(
                x = y_cm,
                y = PrDet,
                color = as.factor(Es_keV)
            )) +
            ggplot2::geom_point(size = 1) +
            ggplot2::geom_line(linewidth = 0.5) +
            ggplot2::geom_errorbar(
                mapping = ggplot2::aes(
                    ymin = PrDet - uPrDet,
                    ymax = PrDet + uPrDet
                ),
                width = 150
            ) +
            ggplot2::facet_grid(
                cols = ggplot2::vars(contents),
                labeller = ggplot2::as_labeller(
                    c("m"="Scrap Metal", "f"="Foodstuff")
                )
            ) +
            ggplot2::scale_x_continuous(
                name = "Distance (cm) (Log10 Scale)",
                trans = "log10",
                limits = c(175, 1525),
                n.breaks = 7
            ) +
            ggplot2::scale_y_continuous(
                name = "Probability (Log10 Scale)",
                trans = "log10",
                limits = c(1e-9, 1e-1),
                n.breaks = 10,
                labels = scales::trans_format(
                    "log10",
                    scales::math_format(10^.x)
                ),
                expand = c(0,0)
            ) +
            ggplot2::scale_colour_viridis_d(
                name = "Source Energy (keV): ",
                breaks = c(
                    200, 500, 1000, 1500, 2000
                ),
                labels = c(
                    "200 ", "500 ", "1000", "1500", "2000"
                ),
                direction = -1,
                option = color_style
            ) +
            ggplot2::guides(color = ggplot2::guide_legend(nrow = 1)) +
            ggplot2::ggtitle(
                "Probability of Photon Detection"
            ) +
            ggplot2::theme_bw() +
            ggplot2::theme(
                legend.position = "top",
                legend.title = ggplot2::element_text(size=7),
                legend.text = ggplot2::element_text(size=7),
                strip.background = ggplot2::element_rect(
                    fill = "white"
                )
            )
    }

    else if (fig_name == "lm_diagnostics_dist") {
        plt <-
            linear_model |>
            broom::augment() |>
            ggplot2::ggplot() +
            ggplot2::geom_histogram(
                mapping = ggplot2::aes(
                    x = .std.resid,
                    y = ggplot2::after_stat(density)
                ),
                bins = btools::bin_number(
                    broom::augment(linear_model)$.std.resid
                ),
                alpha = 0.2,
                col = "black",
                fill = "grey"
            ) +
            ggplot2::geom_line(
                data = dplyr::mutate(
                    data.frame(abscissa = -100:50 / 20),
                    ordinate = dnorm(abscissa)),
                mapping = ggplot2::aes(x = abscissa, y = ordinate)) +
            ggplot2::xlab("Relative Residuals") +
            ggplot2::scale_x_continuous(expand = c(0,0)) +
            ggplot2::ylab("") +
            ggplot2::scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
            ggplot2::ggtitle(
                "Distribution of Residuals",
                subtitle = "Line indicates standard normal distribution") +
            ggplot2::theme_bw()

    }

    else if (fig_name == "lm_diagnostics_qq") {
        plt <-
            linear_model |>
            broom::augment() |>
            ggplot2::ggplot(ggplot2::aes(sample = .resid)) +
            ggplot2::geom_qq() +
            ggplot2::geom_qq_line() +
            ggplot2::xlab("Quantiles of Standard Normal") +
            ggplot2::ylab("Quantiles of Residuals") +
            ggplot2::ggtitle(
                "Quantile-Quantile Plot of Residuals",
                subtitle = "Line indicates normal distribution") +
            ggplot2::theme_bw()
    }

    else if (fig_name == "lm_diagnostics_fit_resid") {
        plt <-
            linear_model |>
            broom::augment() |>
            ggplot2::ggplot() +
            ggplot2::geom_point(mapping = ggplot2::aes(
                x = .fitted,
                y = .resid,
                color = trans_kev,
                alpha = trans_cm
            )) +
            ggplot2::facet_grid(
                rows = ggplot2::vars(contents),
                labeller = ggplot2::as_labeller(c(
                    "m"="Scrap Metal",
                    "f"="Foodstuff"
                ))
            ) +
            ggplot2::xlab("Fitted Values of Ln(Efficiency)") +
            ggplot2::ylab("Residuals of Ln(Efficiency)") +
            ggplot2::ggtitle("Residuals vs. Fitted Values of Linear Model") +
            ggplot2::scale_color_continuous(
                name = "Ln(Source\n\ \ \ Energy)"
            ) +
            ggplot2::scale_alpha_continuous(name = "Ln(Distance)") +
            ggplot2::theme_bw() +
            ggplot2::theme(
                legend.position = "top",
                legend.title = ggplot2::element_text(size=7),
                legend.text = ggplot2::element_text(size=7),
                strip.background = ggplot2::element_rect(fill = "white")
            )
    }

    else if (fig_name == "lm_comparison") {
        plt <-
            summary_data |>
            predict_efficiency(method = "lm") |>
            ggplot2::ggplot() +
            ggplot2::geom_line(
                mapping = ggplot2::aes(
                    x = Es_keV,
                    y = efficiency,
                    color = as.factor(y_cm)
                ),
                linewidth = 1
            ) +
            ggplot2::geom_point(
                mapping = ggplot2::aes(
                    x = Es_keV,
                    y = PrDet,
                    color = as.factor(y_cm)
                ),
                size = 1
            ) +
            ggplot2::facet_grid(
                cols = ggplot2::vars(contents),
                labeller = ggplot2::as_labeller(c(
                    "m"="Scrap Metal",
                    "f"="Foodstuff"
                ))
            ) +
            ggplot2::scale_x_continuous(limits = c(0,2000)) +
            ggplot2::xlab("Source Energy (keV)") +
            ggplot2::scale_y_continuous(
                trans = "log10",
                limits = c(1e-9, 1e-1),
                n.breaks = 10,
                labels = scales::trans_format("log10", scales::math_format(10^.x))
            ) +
            ggplot2::ylab("Probability (Log10 Scale)") +
            ggplot2::scale_colour_viridis_d(
                name = "Distance (cm): ",
                breaks = c(
                    0, -250, -500, -750, -1000, -1250, -1500
                ),
                labels = c(
                    "0     ", "250 ", "500 ", "750 ", "1000",
                    "1250", "1500"
                ),
                direction = -1,
                option = color_style
            ) +
            ggplot2::guides(color = ggplot2::guide_legend(nrow = 1)) +
            ggplot2::ggtitle(
                "Probability of Photon Detection",
                subtitle = "Points denote MCNP values, lines denote fitted values.") +
            ggplot2::theme_bw() +
            ggplot2::theme(
                legend.position = "top",
                legend.title = ggplot2::element_text(size=7),
                legend.text = ggplot2::element_text(size=7),
                strip.background = ggplot2::element_rect(fill = "white")
            )
    }

    else if (fig_name == "lm_contour") {
        plt <-
            rbind(
                dplyr::mutate(data.frame(
                    y_cm = rep(-50:50 * 10, times = 151),
                    Es_keV = rep(50:200 * 10, each = 101)),
                    contents = "m"
                ),
                dplyr::mutate(data.frame(
                    y_cm = rep(-50:50 * 10, times = 151),
                    Es_keV = rep(50:200 * 10, each = 101)),
                    contents = "f"
                )
            ) |>
            predict_efficiency(method = "lm") |>
            ggplot2::ggplot(
                ggplot2::aes(x = Es_keV, y = y_cm, z = 100*(efficiency))
            ) +
            ggplot2::facet_grid(
                rows = ggplot2::vars(contents),
                labeller = ggplot2::as_labeller(c(
                    "m"="Scrap Metal",
                    "f"="Foodstuff"
                ))
            ) +
            ggplot2::geom_contour_filled(bins = 9) +
            ggplot2::scale_fill_viridis_d(
                name = "Efficiency (%)",
                option = "F",
                begin = 1,
                end = 0) +
            ggplot2::scale_x_continuous(expand = c(0,0)) +
            ggplot2::xlab("Source Energy (keV)") +
            ggplot2::scale_y_continuous(expand = c(0.,0.)) +
            ggplot2::ylab("Truck Position (cm)") +
            ggplot2::ggtitle(
                "Detection Efficiency for Different\nSource Energies and Positions"
            ) +
            ggplot2::theme_bw() +
            ggplot2::theme(
                legend.position = "right",
                legend.title = ggplot2::element_text(size=14),
                legend.text = ggplot2::element_text(size=12),
                strip.background = ggplot2::element_rect(fill = "white"),
                panel.spacing = ggplot2::unit(1, "lines")
            )
    }

    else if (fig_name == "lm_net_counts") {
        stop("Not done yet")
    }

    else if (fig_name == "lm_derivative") {
        plt <-
            rbind(
                dplyr::mutate(
                    predict_count_rate(
                        data.frame(
                            y_cm = rep(
                                pracma::linspace(-300, 300, n = 1000),
                                times = 2
                            ),
                            contents = rep(c("m","f"), each = 1000)
                        ),
                        photon_energy_keV = 661.66,
                        yield = 0.851,
                        speed_cm_s = 447,
                        count_rate_derivative = TRUE,
                        method = "lm"
                    ),
                    source = "Cs-137"
                ),
                dplyr::mutate(
                    predict_count_rate(
                        data.frame(
                            y_cm = rep(
                                pracma::linspace(-300, 300, n = 1000),
                                times = 2
                            ),
                            contents = rep(c("m","f"), each = 1000)
                        ),
                        photon_energy_keV = c(1173, 1332),
                        yield = c(0.998, 0.999),
                        speed_cm_s = 447,
                        count_rate_derivative = TRUE,
                        method = "lm"
                    ),
                    source = "Co-60"
                ),
                dplyr::mutate(
                    predict_count_rate(
                        data.frame(
                            y_cm = rep(
                                pracma::linspace(-300, 300, n = 1000),
                                times = 2
                            ),
                            contents = rep(c("m","f"), each = 1000)
                        ),
                        photon_energy_keV = 510.9,
                        yield = 2,
                        speed_cm_s = 447,
                        count_rate_derivative = TRUE,
                        method = "lm"
                    ),
                    source = "Beta-Plus"
                )
            ) |>
            dplyr::mutate(
                source = as.factor(source),
                y_m = y_cm / 100
            ) |>
            ggplot2::ggplot() +
            ggplot2::geom_line(mapping = ggplot2::aes(
                x = y_m,
                y = count_rate_derivative,
                linetype = source)) +
            ggplot2::facet_grid(
                cols = ggplot2::vars(contents),
                labeller = ggplot2::as_labeller(c(
                    "m"="Scrap Metal",
                    "f"="Foodstuff"
                ))
            ) +
            ggplot2::scale_linetype_discrete(name = "Source") +
            ggplot2::scale_x_continuous(
                name = "Distance (m)",
                limits = c(-3,3.1),
                expand = c(0,0)
            ) +
            ggplot2::scale_y_continuous(
                name = "dr/dt (cps/s/Bq)",
                limits = c(-0.06,0.06),
                expand = c(0,0)
            ) +
            ggplot2::ggtitle(
                "Time Derivative of Count Rate Along Trajectory"
            ) +
            ggplot2::theme_bw() +
            ggplot2::theme(
                strip.background = ggplot2::element_rect(fill = "white")
            )
    }

    else if (fig_name == "lm_log_derivative") {
        plt <-
            predict_count_rate(
                data.frame(
                    y_cm = pracma::linspace(-300, 300, n = 1000),
                    contents = rep("m", each = 1000)
                ),
                photon_energy_keV = 661.66,
                yield = 0.851,
                speed_cm_s = 447,
                count_rate_derivative = TRUE,
                method = "lm"
            ) |>
            dplyr::mutate(
                y_m = y_cm / 100,
                log_deriv = count_rate_derivative / count_rate
            ) |>
            ggplot2::ggplot() +
            ggplot2::geom_line(mapping = ggplot2::aes(
                x = y_m,
                y = log_deriv
            )) +
            ggplot2::scale_x_continuous(
                name = "Distance (m)",
                limits = c(-3,3.1),
                expand = c(0,0)
            ) +
            ggplot2::scale_y_continuous(
                name = "d ln(r)/dt (1/s)",
                limits = c(-6,6),
                expand = c(0,0)
            ) +
            ggplot2::ggtitle(
                "Logarithmic Time Derivative of Count Rate Along Trajectory"
            ) +
            ggplot2::theme_bw() +
            ggplot2::theme(
                strip.background = ggplot2::element_rect(fill = "white")
            )
    }

    else if (fig_name == "earth_explanation") {
        stop("Not done yet")
    }

    else if (fig_name == "earth_diagnostics_dist") {
        stop("Not done yet")
    }

    else if (fig_name == "earth_diagnostics_qq") {
        stop("Not done yet")
    }

    else if (fig_name == "earth_diagnostics_fit_resid") {
        stop("Not done yet")
    }

    else if (fig_name == "earth_comparison") {
        stop("Not done yet")
    }

    else if (fig_name == "earth_contour") {
        plt <-
            rbind(
                dplyr::mutate(data.frame(
                    y_cm = rep(-50:50 * 10, times = 151),
                    Es_keV = rep(50:200 * 10, each = 101)), contents = "m"),
                dplyr::mutate(data.frame(
                    y_cm = rep(-50:50 * 10, times = 151),
                    Es_keV = rep(50:200 * 10, each = 101)), contents = "f")
            ) |>
            predict_efficiency(method = "earth") |>
            ggplot2::ggplot(
                ggplot2::aes(x = Es_keV, y = y_cm, z = 100*(efficiency))
            ) +
            ggplot2::facet_grid(
                rows = ggplot2::vars(contents),
                labeller = ggplot2::as_labeller(c(
                    "m"="Scrap Metal",
                    "f"="Foodstuff"
                ))
            ) +
            ggplot2::geom_contour_filled(bins = 7) +
            ggplot2::scale_fill_viridis_d(
                name = "Efficiency (%)",
                option = "F",
                begin = 1,
                end = 0) +
            ggplot2::scale_x_continuous(
                name = "Source Energy (keV)", expand = c(0,0)
            ) +
            ggplot2::scale_y_continuous(
                name = "Truck Position (cm)", expand = c(0.,0.)
            ) +
            ggplot2::ggtitle(
                "Detection Efficiency for Different\nSource Energies and Positions") +
            ggplot2::theme_bw() +
            ggplot2::theme(
                legend.position = "right",
                legend.title = ggplot2::element_text(size=14),
                legend.text = ggplot2::element_text(size=12),
                strip.background = ggplot2::element_rect(fill = "white"),
                panel.spacing = ggplot2::unit(1, "lines")
            )
    }

    else {
        stop(paste(
            "Potential figures are:",
            "pvt_atten_coeff",
            "interior_atten_coeff",
            "histories_needed",
            "star_differences",
            "parity_differences",
            "prob_reach",
            "prob_det_energy",
            "prob_det_position",
            "lm_diagnostics_dist",
            "lm_diagnostics_qq",
            "lm_diagnostics_fit_resid",
            "lm_comparison",
            "lm_contour",
            "lm_derivative",
            "lm_log_derivative",
            sep = "\n"
        ))
    }


    return(plt)
}
