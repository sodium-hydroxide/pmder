library(ggplot2)
library(dplyr)
library(tidyr)
#
# Atom attenuation coefficients ----
attenuation_coefficients <-
    rbind(
        data.frame(
            E_MeV = c(
                1.00000e-03, 1.50000e-03, 2.00000e-03, 3.00000e-03, 4.00000e-03,
                5.00000e-03, 6.00000e-03, 8.00000e-03, 1.00000e-02, 1.50000e-02,
                2.00000e-02, 3.00000e-02, 4.00000e-02, 5.00000e-02, 6.00000e-02,
                8.00000e-02, 1.00000e-01, 1.50000e-01, 2.00000e-01, 3.00000e-01,
                4.00000e-01, 5.00000e-01, 6.00000e-01, 8.00000e-01, 1.00000e+00,
                1.25000e+00, 1.50000e+00, 2.00000e+00, 3.00000e+00, 4.00000e+00,
                5.00000e+00, 6.00000e+00, 8.00000e+00, 1.00000e+01, 1.50000e+01,
                2.00000e+01
            ),
            mu_cm2_g = c(
                7.217e+00, 2.148e+00, 1.059e+00, 5.612e-01, 4.546e-01, 4.193e-01,
                4.042e-01, 3.914e-01, 3.854e-01, 3.764e-01, 3.695e-01, 3.570e-01,
                3.458e-01, 3.355e-01, 3.260e-01, 3.091e-01, 2.944e-01, 2.651e-01,
                2.429e-01, 2.112e-01, 1.893e-01, 1.729e-01, 1.599e-01, 1.405e-01,
                1.263e-01, 1.129e-01, 1.027e-01, 8.769e-02, 6.921e-02, 5.806e-02,
                5.049e-02, 4.498e-02, 3.746e-02, 3.254e-02, 2.539e-02, 2.153e-02
            ),
            atom = "H"
        ),
        data.frame(
            E_MeV = c(
                1.00000e-03, 1.50000e-03, 2.00000e-03, 3.00000e-03, 4.00000e-03,
                5.00000e-03, 6.00000e-03, 8.00000e-03, 1.00000e-02, 1.50000e-02,
                2.00000e-02, 3.00000e-02, 4.00000e-02, 5.00000e-02, 6.00000e-02,
                8.00000e-02, 1.00000e-01, 1.50000e-01, 2.00000e-01, 3.00000e-01,
                4.00000e-01, 5.00000e-01, 6.00000e-01, 8.00000e-01, 1.00000e+00,
                1.25000e+00, 1.50000e+00, 2.00000e+00, 3.00000e+00, 4.00000e+00,
                5.00000e+00, 6.00000e+00, 8.00000e+00, 1.00000e+01, 1.50000e+01,
                2.00000e+01
            ),
            mu_cm2_g = c(
                2.211e+03, 7.002e+02, 3.026e+02, 9.033e+01, 3.778e+01, 1.912e+01,
                1.095e+01, 4.576e+00, 2.373e+00, 8.071e-01, 4.420e-01, 2.562e-01,
                2.076e-01, 1.871e-01, 1.753e-01, 1.610e-01, 1.514e-01, 1.347e-01,
                1.229e-01, 1.066e-01, 9.546e-02, 8.715e-02, 8.058e-02, 7.076e-02,
                6.361e-02, 5.690e-02, 5.179e-02, 4.442e-02, 3.562e-02, 3.047e-02,
                2.708e-02, 2.469e-02, 2.154e-02, 1.959e-02, 1.698e-02, 1.575e-02
            ),
            atom = "C"
        ),
        data.frame(
            E_MeV = c(
                1.00000e-03, 1.50000e-03, 2.00000e-03, 3.00000e-03, 4.00000e-03,
                5.00000e-03, 6.00000e-03, 8.00000e-03, 1.00000e-02, 1.50000e-02,
                2.00000e-02, 3.00000e-02, 4.00000e-02, 5.00000e-02, 6.00000e-02,
                8.00000e-02, 1.00000e-01, 1.50000e-01, 2.00000e-01, 3.00000e-01,
                4.00000e-01, 5.00000e-01, 6.00000e-01, 8.00000e-01, 1.00000e+00,
                1.25000e+00, 1.50000e+00, 2.00000e+00, 3.00000e+00, 4.00000e+00,
                5.00000e+00, 6.00000e+00, 8.00000e+00, 1.00000e+01, 1.50000e+01,
                2.00000e+01
            ),
            mu_cm2_g = c(
                3.311e+03, 1.083e+03, 4.769e+02, 1.456e+02, 6.166e+01, 3.144e+01,
                1.809e+01, 7.562e+00, 3.879e+00, 1.236e+00, 6.178e-01, 3.066e-01,
                2.288e-01, 1.980e-01, 1.817e-01, 1.639e-01, 1.529e-01, 1.353e-01,
                1.233e-01, 1.068e-01, 9.557e-02, 8.719e-02, 8.063e-02, 7.081e-02,
                6.364e-02, 5.693e-02, 5.180e-02, 4.450e-02, 3.579e-02, 3.073e-02,
                2.742e-02, 2.511e-02, 2.209e-02, 2.024e-02, 1.782e-02, 1.673e-02
            ),
            atom = "N"
        ),
        data.frame(
            E_MeV = c(
                1.00000e-03, 1.50000e-03, 2.00000e-03, 3.00000e-03, 4.00000e-03,
                5.00000e-03, 6.00000e-03, 8.00000e-03, 1.00000e-02, 1.50000e-02,
                2.00000e-02, 3.00000e-02, 4.00000e-02, 5.00000e-02, 6.00000e-02,
                8.00000e-02, 1.00000e-01, 1.50000e-01, 2.00000e-01, 3.00000e-01,
                4.00000e-01, 5.00000e-01, 6.00000e-01, 8.00000e-01, 1.00000e+00,
                1.25000e+00, 1.50000e+00, 2.00000e+00, 3.00000e+00, 4.00000e+00,
                5.00000e+00, 6.00000e+00, 8.00000e+00, 1.00000e+01, 1.50000e+01,
                2.00000e+01
            ),
            mu_cm2_g = c(
                4.590e+03, 1.549e+03, 6.949e+02, 2.171e+02, 9.315e+01, 4.790e+01,
                2.770e+01, 1.163e+01, 5.952e+00, 1.836e+00, 8.651e-01, 3.779e-01,
                2.585e-01, 2.132e-01, 1.907e-01, 1.678e-01, 1.551e-01, 1.361e-01,
                1.237e-01, 1.070e-01, 9.566e-02, 8.729e-02, 8.070e-02, 7.087e-02,
                6.372e-02, 5.697e-02, 5.185e-02, 4.459e-02, 3.597e-02, 3.100e-02,
                2.777e-02, 2.552e-02, 2.263e-02, 2.089e-02, 1.866e-02, 1.770e-02
            ),
            atom = "O"
        ),
        data.frame(
            E_MeV = c(
                1.00000e-03, 1.03542e-03, 1.07210e-03, 1.07210e-03, 1.50000e-03,
                2.00000e-03, 3.00000e-03, 4.00000e-03, 5.00000e-03, 6.00000e-03,
                8.00000e-03, 1.00000e-02, 1.50000e-02, 2.00000e-02, 3.00000e-02,
                4.00000e-02, 5.00000e-02, 6.00000e-02, 8.00000e-02, 1.00000e-01,
                1.50000e-01, 2.00000e-01, 3.00000e-01, 4.00000e-01, 5.00000e-01,
                6.00000e-01, 8.00000e-01, 1.00000e+00, 1.25000e+00, 1.50000e+00,
                2.00000e+00, 3.00000e+00, 4.00000e+00, 5.00000e+00, 6.00000e+00,
                8.00000e+00, 1.00000e+01, 1.50000e+01, 2.00000e+01
            ),
            mu_cm2_g = c(
                6.542e+02, 5.960e+02, 5.429e+02, 6.435e+03, 3.194e+03, 1.521e+03,
                5.070e+02, 2.261e+02, 1.194e+02, 7.030e+01, 3.018e+01, 1.557e+01,
                4.694e+00, 2.057e+00, 7.197e-01, 3.969e-01, 2.804e-01, 2.268e-01,
                1.796e-01, 1.585e-01, 1.335e-01, 1.199e-01, 1.029e-01, 9.185e-02,
                8.372e-02, 7.736e-02, 6.788e-02, 6.100e-02, 5.454e-02, 4.968e-02,
                4.282e-02, 3.487e-02, 3.037e-02, 2.753e-02, 2.559e-02, 2.319e-02,
                2.181e-02, 2.023e-02, 1.970e-02
            ),
            atom = "Na"
        ),
        data.frame(
            E_MeV = c(
                1.00000e-03, 1.14237e-03, 1.30500e-03, 1.30500e-03, 1.50000e-03,
                2.00000e-03, 3.00000e-03, 4.00000e-03, 5.00000e-03, 6.00000e-03,
                8.00000e-03, 1.00000e-02, 1.50000e-02, 2.00000e-02, 3.00000e-02,
                4.00000e-02, 5.00000e-02, 6.00000e-02, 8.00000e-02, 1.00000e-01,
                1.50000e-01, 2.00000e-01, 3.00000e-01, 4.00000e-01, 5.00000e-01,
                6.00000e-01, 8.00000e-01, 1.00000e+00, 1.25000e+00, 1.50000e+00,
                2.00000e+00, 3.00000e+00, 4.00000e+00, 5.00000e+00, 6.00000e+00,
                8.00000e+00, 1.00000e+01, 1.50000e+01, 2.00000e+01
            ),
            mu_cm2_g = c(
                9.225e+02, 6.474e+02, 4.530e+02, 5.444e+03, 4.004e+03, 1.932e+03,
                6.585e+02, 2.974e+02, 1.583e+02, 9.381e+01, 4.061e+01, 2.105e+01,
                6.358e+00, 2.763e+00, 9.306e-01, 4.881e-01, 3.292e-01, 2.570e-01,
                1.951e-01, 1.686e-01, 1.394e-01, 1.245e-01, 1.065e-01, 9.492e-02,
                8.647e-02, 7.988e-02, 7.008e-02, 6.296e-02, 5.629e-02, 5.129e-02,
                4.426e-02, 3.613e-02, 3.159e-02, 2.873e-02, 2.681e-02, 2.445e-02,
                2.313e-02, 2.168e-02, 2.127e-02
            ),
            atom = "Mg"
        ),
        data.frame(
            E_MeV = c(
                1.00000e-03, 1.50000e-03, 1.83890e-03, 1.83890e-03, 2.00000e-03,
                3.00000e-03, 4.00000e-03, 5.00000e-03, 6.00000e-03, 8.00000e-03,
                1.00000e-02, 1.50000e-02, 2.00000e-02, 3.00000e-02, 4.00000e-02,
                5.00000e-02, 6.00000e-02, 8.00000e-02, 1.00000e-01, 1.50000e-01,
                2.00000e-01, 3.00000e-01, 4.00000e-01, 5.00000e-01, 6.00000e-01,
                8.00000e-01, 1.00000e+00, 1.25000e+00, 1.50000e+00, 2.00000e+00,
                3.00000e+00, 4.00000e+00, 5.00000e+00, 6.00000e+00, 8.00000e+00,
                1.00000e+01, 1.50000e+01, 2.00000e+01
            ),
            mu_cm2_g = c(
                1.570e+03, 5.355e+02, 3.092e+02, 3.192e+03, 2.777e+03, 9.784e+02,
                4.529e+02, 2.450e+02, 1.470e+02, 6.468e+01, 3.389e+01, 1.034e+01,
                4.464e+00, 1.436e+00, 7.012e-01, 4.385e-01, 3.207e-01, 2.228e-01,
                1.835e-01, 1.448e-01, 1.275e-01, 1.082e-01, 9.614e-02, 8.748e-02,
                8.077e-02, 7.082e-02, 6.361e-02, 5.688e-02, 5.183e-02, 4.480e-02,
                3.678e-02, 3.240e-02, 2.967e-02, 2.788e-02, 2.574e-02, 2.462e-02,
                2.352e-02, 2.338e-02
            ),
            atom = "Si"
        ),
        data.frame(
            E_MeV = c(
                1.00000e-03, 1.50000e-03, 2.00000e-03, 2.14550e-03, 2.14550e-03,
                3.00000e-03, 4.00000e-03, 5.00000e-03, 6.00000e-03, 8.00000e-03,
                1.00000e-02, 1.50000e-02, 2.00000e-02, 3.00000e-02, 4.00000e-02,
                5.00000e-02, 6.00000e-02, 8.00000e-02, 1.00000e-01, 1.50000e-01,
                2.00000e-01, 3.00000e-01, 4.00000e-01, 5.00000e-01, 6.00000e-01,
                8.00000e-01, 1.00000e+00, 1.25000e+00, 1.50000e+00, 2.00000e+00,
                3.00000e+00, 4.00000e+00, 5.00000e+00, 6.00000e+00, 8.00000e+00,
                1.00000e+01, 1.50000e+01, 2.00000e+01
            ),
            mu_cm2_g = c(
                1.913e+03, 6.547e+02, 3.018e+02, 2.494e+02, 2.473e+03, 1.118e+03,
                5.242e+02, 2.860e+02, 1.726e+02, 7.660e+01, 4.035e+01, 1.239e+01,
                5.352e+00, 1.700e+00, 8.096e-01, 4.916e-01, 3.494e-01, 2.324e-01,
                1.865e-01, 1.432e-01, 1.250e-01, 1.055e-01, 9.359e-02, 8.511e-02,
                7.854e-02, 6.884e-02, 6.182e-02, 5.526e-02, 5.039e-02, 4.358e-02,
                3.590e-02, 3.172e-02, 2.915e-02, 2.747e-02, 2.552e-02, 2.452e-02,
                2.364e-02, 2.363e-02
            ),
            atom = "P"
        ),
        data.frame(
            E_MeV = c(
                1.00000e-03, 1.50000e-03, 2.00000e-03, 2.47200e-03, 2.47200e-03,
                3.00000e-03, 4.00000e-03, 5.00000e-03, 6.00000e-03, 8.00000e-03,
                1.00000e-02, 1.50000e-02, 2.00000e-02, 3.00000e-02, 4.00000e-02,
                5.00000e-02, 6.00000e-02, 8.00000e-02, 1.00000e-01, 1.50000e-01,
                2.00000e-01, 3.00000e-01, 4.00000e-01, 5.00000e-01, 6.00000e-01,
                8.00000e-01, 1.00000e+00, 1.25000e+00, 1.50000e+00, 2.00000e+00,
                3.00000e+00, 4.00000e+00, 5.00000e+00, 6.00000e+00, 8.00000e+00,
                1.00000e+01, 1.50000e+01, 2.00000e+01
            ),
            mu_cm2_g = c(
                2.429e+03, 8.342e+02, 3.853e+02, 2.168e+02, 2.070e+03, 1.339e+03,
                6.338e+02, 3.487e+02, 2.116e+02, 9.465e+01, 5.012e+01, 1.550e+01,
                6.708e+00, 2.113e+00, 9.872e-01, 5.849e-01, 4.053e-01, 2.585e-01,
                2.020e-01, 1.506e-01, 1.302e-01, 1.091e-01, 9.665e-02, 8.781e-02,
                8.102e-02, 7.098e-02, 6.373e-02, 5.697e-02, 5.193e-02, 4.498e-02,
                3.715e-02, 3.293e-02, 3.036e-02, 2.872e-02, 2.682e-02, 2.589e-02,
                2.517e-02, 2.529e-02
            ),
            atom = "S"
        ),
        data.frame(
            E_MeV = c(
                1.00000e-03, 1.50000e-03, 2.00000e-03, 2.82240e-03, 2.82240e-03,
                3.00000e-03, 4.00000e-03, 5.00000e-03, 6.00000e-03, 8.00000e-03,
                1.00000e-02, 1.50000e-02, 2.00000e-02, 3.00000e-02, 4.00000e-02,
                5.00000e-02, 6.00000e-02, 8.00000e-02, 1.00000e-01, 1.50000e-01,
                2.00000e-01, 3.00000e-01, 4.00000e-01, 5.00000e-01, 6.00000e-01,
                8.00000e-01, 1.00000e+00, 1.25000e+00, 1.50000e+00, 2.00000e+00,
                3.00000e+00, 4.00000e+00, 5.00000e+00, 6.00000e+00, 8.00000e+00,
                1.00000e+01, 1.50000e+01, 2.00000e+01
            ),
            mu_cm2_g = c(
                2.832e+03, 9.771e+02, 4.520e+02, 1.774e+02, 1.637e+03, 1.473e+03,
                7.037e+02, 3.901e+02, 2.384e+02, 1.075e+02, 5.725e+01, 1.784e+01,
                7.739e+00, 2.425e+00, 1.117e+00, 6.483e-01, 4.395e-01, 2.696e-01,
                2.050e-01, 1.480e-01, 1.266e-01, 1.054e-01, 9.311e-02, 8.453e-02,
                7.795e-02, 6.826e-02, 6.128e-02, 5.478e-02, 4.994e-02, 4.328e-02,
                3.585e-02, 3.188e-02, 2.950e-02, 2.798e-02, 2.628e-02, 2.549e-02,
                2.496e-02, 2.520e-02
            ),
            atom = "Cl"
        ),
        data.frame(
            E_MeV = c(
                1.00000e-03, 1.50000e-03, 2.00000e-03, 3.00000e-03, 3.20290e-03,
                3.20290e-03, 4.00000e-03, 5.00000e-03, 6.00000e-03, 8.00000e-03,
                1.00000e-02, 1.50000e-02, 2.00000e-02, 3.00000e-02, 4.00000e-02,
                5.00000e-02, 6.00000e-02, 8.00000e-02, 1.00000e-01, 1.50000e-01,
                2.00000e-01, 3.00000e-01, 4.00000e-01, 5.00000e-01, 6.00000e-01,
                8.00000e-01, 1.00000e+00, 1.25000e+00, 1.50000e+00, 2.00000e+00,
                3.00000e+00, 4.00000e+00, 5.00000e+00, 6.00000e+00, 8.00000e+00,
                1.00000e+01, 1.50000e+01, 2.00000e+01
            ),
            mu_cm2_g = c(
                3.184e+03, 1.105e+03, 5.120e+02, 1.703e+02, 1.424e+02, 1.275e+03,
                7.572e+02, 4.225e+02, 2.593e+02, 1.180e+02, 6.316e+01, 1.983e+01,
                8.629e+00, 2.697e+00, 1.228e+00, 7.012e-01, 4.664e-01, 2.760e-01,
                2.043e-01, 1.427e-01, 1.205e-01, 9.953e-02, 8.776e-02, 7.958e-02,
                7.335e-02, 6.419e-02, 5.762e-02, 5.150e-02, 4.695e-02, 4.074e-02,
                3.384e-02, 3.019e-02, 2.802e-02, 2.667e-02, 2.517e-02, 2.451e-02,
                2.418e-02, 2.453e-02
            ),
            atom = "Ar"
        ),
        data.frame(
            E_MeV = c(
                1.00000e-03, 1.50000e-03, 2.00000e-03, 3.00000e-03, 3.60740e-03,
                3.60740e-03, 4.00000e-03, 5.00000e-03, 6.00000e-03, 8.00000e-03,
                1.00000e-02, 1.50000e-02, 2.00000e-02, 3.00000e-02, 4.00000e-02,
                5.00000e-02, 6.00000e-02, 8.00000e-02, 1.00000e-01, 1.50000e-01,
                2.00000e-01, 3.00000e-01, 4.00000e-01, 5.00000e-01, 6.00000e-01,
                8.00000e-01, 1.00000e+00, 1.25000e+00, 1.50000e+00, 2.00000e+00,
                3.00000e+00, 4.00000e+00, 5.00000e+00, 6.00000e+00, 8.00000e+00,
                1.00000e+01, 1.50000e+01, 2.00000e+01
            ),
            mu_cm2_g = c(
                4.058e+03, 1.418e+03, 6.592e+02, 2.198e+02, 1.327e+02, 1.201e+03,
                9.256e+02, 5.189e+02, 3.205e+02, 1.469e+02, 7.907e+01, 2.503e+01,
                1.093e+01, 3.413e+00, 1.541e+00, 8.679e-01, 5.678e-01, 3.251e-01,
                2.345e-01, 1.582e-01, 1.319e-01, 1.080e-01, 9.495e-02, 8.600e-02,
                7.922e-02, 6.929e-02, 6.216e-02, 5.556e-02, 5.068e-02, 4.399e-02,
                3.666e-02, 3.282e-02, 3.054e-02, 2.915e-02, 2.766e-02, 2.704e-02,
                2.687e-02, 2.737e-02
            ),
            atom = "K"
        ),
        data.frame(
            E_MeV = c(
                1.00000e-03, 1.50000e-03, 2.00000e-03, 3.00000e-03, 4.00000e-03,
                4.03810e-03, 4.03810e-03, 5.00000e-03, 6.00000e-03, 8.00000e-03,
                1.00000e-02, 1.50000e-02, 2.00000e-02, 3.00000e-02, 4.00000e-02,
                5.00000e-02, 6.00000e-02, 8.00000e-02, 1.00000e-01, 1.50000e-01,
                2.00000e-01, 3.00000e-01, 4.00000e-01, 5.00000e-01, 6.00000e-01,
                8.00000e-01, 1.00000e+00, 1.25000e+00, 1.50000e+00, 2.00000e+00,
                3.00000e+00, 4.00000e+00, 5.00000e+00, 6.00000e+00, 8.00000e+00,
                1.00000e+01, 1.50000e+01, 2.00000e+01
            ),
            mu_cm2_g = c(
                4.867e+03, 1.714e+03, 7.999e+02, 2.676e+02, 1.218e+02, 1.187e+02,
                1.023e+03, 6.026e+02, 3.731e+02, 1.726e+02, 9.341e+01, 2.979e+01,
                1.306e+01, 4.080e+00, 1.830e+00, 1.019e+00, 6.578e-01, 3.656e-01,
                2.571e-01, 1.674e-01, 1.376e-01, 1.116e-01, 9.783e-02, 8.851e-02,
                8.148e-02, 7.122e-02, 6.388e-02, 5.709e-02, 5.207e-02, 4.524e-02,
                3.780e-02, 3.395e-02, 3.170e-02, 3.035e-02, 2.892e-02, 2.839e-02,
                2.838e-02, 2.903e-02
            ),
            atom = "Ca"
        ),
        data.frame(
            E_MeV = c(
                1.00000e-03, 1.50000e-03, 2.00000e-03, 3.00000e-03, 4.00000e-03,
                5.00000e-03, 5.98920e-03, 5.98920e-03, 6.00000e-03, 8.00000e-03,
                1.00000e-02, 1.50000e-02, 2.00000e-02, 3.00000e-02, 4.00000e-02,
                5.00000e-02, 6.00000e-02, 8.00000e-02, 1.00000e-01, 1.50000e-01,
                2.00000e-01, 3.00000e-01, 4.00000e-01, 5.00000e-01, 6.00000e-01,
                8.00000e-01, 1.00000e+00, 1.25000e+00, 1.50000e+00, 2.00000e+00,
                3.00000e+00, 4.00000e+00, 5.00000e+00, 6.00000e+00, 8.00000e+00,
                1.00000e+01, 1.50000e+01, 2.00000e+01
            ),
            mu_cm2_g = c(
                7.405e+03, 2.694e+03, 1.277e+03, 4.339e+02, 1.988e+02, 1.080e+02,
                6.574e+01, 5.977e+02, 5.160e+02, 2.513e+02, 1.386e+02, 4.571e+01,
                2.038e+01, 6.434e+00, 2.856e+00, 1.550e+00, 9.639e-01, 4.905e-01,
                3.166e-01, 1.788e-01, 1.378e-01, 1.067e-01, 9.213e-02, 8.281e-02,
                7.598e-02, 6.620e-02, 5.930e-02, 5.295e-02, 4.832e-02, 4.213e-02,
                3.559e-02, 3.235e-02, 3.057e-02, 2.956e-02, 2.869e-02, 2.855e-02,
                2.920e-02, 3.026e-02
            ),
            atom = "Cr"
        ),
        data.frame(
            E_MeV = c(
                1.00000e-03, 1.50000e-03, 2.00000e-03, 3.00000e-03, 4.00000e-03,
                5.00000e-03, 6.00000e-03, 6.53900e-03, 6.53900e-03, 8.00000e-03,
                1.00000e-02, 1.50000e-02, 2.00000e-02, 3.00000e-02, 4.00000e-02,
                5.00000e-02, 6.00000e-02, 8.00000e-02, 1.00000e-01, 1.50000e-01,
                2.00000e-01, 3.00000e-01, 4.00000e-01, 5.00000e-01, 6.00000e-01,
                8.00000e-01, 1.00000e+00, 1.25000e+00, 1.50000e+00, 2.00000e+00,
                3.00000e+00, 4.00000e+00, 5.00000e+00, 6.00000e+00, 8.00000e+00,
                1.00000e+01, 1.50000e+01, 2.00000e+01
            ),
            mu_cm2_g = c(
                8.093e+03, 2.984e+03, 1.421e+03, 4.851e+02, 2.229e+02, 1.212e+02,
                7.350e+01, 5.803e+01, 4.520e+02, 2.734e+02, 1.514e+02, 5.027e+01,
                2.253e+01, 7.141e+00, 3.169e+00, 1.714e+00, 1.060e+00, 5.306e-01,
                3.367e-01, 1.838e-01, 1.391e-01, 1.062e-01, 9.133e-02, 8.192e-02,
                7.509e-02, 6.537e-02, 5.852e-02, 5.224e-02, 4.769e-02, 4.162e-02,
                3.524e-02, 3.213e-02, 3.045e-02, 2.952e-02, 2.875e-02, 2.871e-02,
                2.951e-02, 3.068e-02
            ),
            atom = "Mn"
        ),
        data.frame(
            E_MeV = c(
                1.00000e-03, 1.50000e-03, 2.00000e-03, 3.00000e-03, 4.00000e-03,
                5.00000e-03, 6.00000e-03, 7.11200e-03, 7.11200e-03, 8.00000e-03,
                1.00000e-02, 1.50000e-02, 2.00000e-02, 3.00000e-02, 4.00000e-02,
                5.00000e-02, 6.00000e-02, 8.00000e-02, 1.00000e-01, 1.50000e-01,
                2.00000e-01, 3.00000e-01, 4.00000e-01, 5.00000e-01, 6.00000e-01,
                8.00000e-01, 1.00000e+00, 1.25000e+00, 1.50000e+00, 2.00000e+00,
                3.00000e+00, 4.00000e+00, 5.00000e+00, 6.00000e+00, 8.00000e+00,
                1.00000e+01, 1.50000e+01, 2.00000e+01
            ),
            mu_cm2_g = c(
                9.085e+03, 3.399e+05, 1.626e+03, 5.576e+02, 2.567e+02, 1.398e+02,
                8.484e+01, 5.319e+01, 4.076e+02, 3.056e+02, 1.706e+02, 5.708e+01,
                2.568e+01, 8.176e+00, 3.629e+00, 1.958e+00, 1.205e+00, 5.952e-01,
                3.717e-01, 1.964e-01, 1.460e-01, 1.099e-01, 9.400e-02, 8.414e-02,
                7.704e-02, 6.699e-02, 5.995e-02, 5.350e-02, 4.883e-02, 4.265e-02,
                3.621e-02, 3.312e-02, 3.146e-02, 3.057e-02, 2.991e-02, 2.994e-02,
                3.092e-02, 3.224e-02
            ),
            atom = "Fe"
        ),
        data.frame(
            E_MeV = c(
                1.00000e-03, 1.00980e-03, 1.01970e-03, 1.01970e-03, 1.03119e-03,
                1.04280e-03, 1.04280e-03, 1.11565e-03, 1.19360e-03, 1.19360e-03,
                1.50000e-03, 2.00000e-03, 3.00000e-03, 4.00000e-03, 5.00000e-03,
                6.00000e-03, 8.00000e-03, 9.65860e-03, 9.65860e-03, 1.00000e-02,
                1.50000e-02, 2.00000e-02, 3.00000e-02, 4.00000e-02, 5.00000e-02,
                6.00000e-02, 8.00000e-02, 1.00000e-01, 1.50000e-01, 2.00000e-01,
                3.00000e-01, 4.00000e-01, 5.00000e-01, 6.00000e-01, 8.00000e-01,
                1.00000e+00, 1.25000e+00, 1.50000e+00, 2.00000e+00, 3.00000e+00,
                4.00000e+00, 5.00000e+00, 6.00000e+00, 8.00000e+00, 1.00000e+01,
                1.50000e+01, 2.00000e+01
            ),
            mu_cm2_g = c(
                1.553e+03, 1.518e+03, 1.484e+03, 3.804e+03, 5.097e+03, 6.518e+03,
                8.274e+03, 8.452e+03, 7.371e+03, 8.396e+03, 4.825e+03, 2.375e+03,
                8.311e+02, 3.865e+02, 2.118e+02, 1.290e+02, 5.875e+01, 3.505e+01,
                2.536e+02, 2.331e+02, 8.117e+01, 3.719e+01, 1.207e+01, 5.384e+00,
                2.892e+00, 1.760e+00, 8.364e-01, 4.973e-01, 2.341e-01, 1.617e-01,
                1.141e-01, 9.539e-02, 8.450e-02, 7.695e-02, 6.656e-02, 5.941e-02,
                5.296e-02, 4.834e-02, 4.235e-02, 3.634e-02, 3.360e-02, 3.225e-02,
                3.160e-02, 3.138e-02, 3.175e-02, 3.335e-02, 3.509e-02
            ),
            atom = "Zn"
        ),
        data.frame(
            E_MeV = c(
                1.00000e-03, 1.00404e-03, 1.00810e-03, 1.00810e-03, 1.50000e-03,
                2.00000e-03, 3.00000e-03, 4.00000e-03, 5.00000e-03, 6.00000e-03,
                8.00000e-03, 8.33280e-03, 8.33280e-03, 1.00000e-02, 1.50000e-02,
                2.00000e-02, 3.00000e-02, 4.00000e-02, 5.00000e-02, 6.00000e-02,
                8.00000e-02, 1.00000e-01, 1.50000e-01, 2.00000e-01, 3.00000e-01,
                4.00000e-01, 5.00000e-01, 6.00000e-01, 8.00000e-01, 1.00000e+00,
                1.25000e+00, 1.50000e+00, 2.00000e+00, 3.00000e+00, 4.00000e+00,
                5.00000e+00, 6.00000e+00, 8.00000e+00, 1.00000e+01, 1.50000e+01,
                2.00000e+01
            ),
            mu_cm2_g = c(
                9.855e+03, 9.753e+03, 9.654e+03, 1.099e+04, 4.234e+03, 2.049e+03,
                7.094e+02, 3.282e+02, 1.793e+02, 1.090e+02, 4.952e+01, 4.428e+01,
                3.294e+02, 2.090e+02, 7.081e+01, 3.220e+01, 1.034e+01, 4.600e+00,
                2.474e+00, 1.512e+00, 7.306e-01, 4.440e-01, 2.208e-01, 1.582e-01,
                1.154e-01, 9.765e-02, 8.698e-02, 7.944e-02, 6.891e-02, 6.160e-02,
                5.494e-02, 5.015e-02, 4.387e-02, 3.745e-02, 3.444e-02, 3.289e-02,
                3.210e-02, 3.164e-02, 3.185e-02, 3.320e-02, 3.476e-02
            ),
            atom = "Ni"
        )
    ) |>
    dplyr::filter(E_MeV <= 2)

# Bulk Material Mass Fractions ----

steel <- data.frame(
    atom = c("C", "N", "Si", "P", "S", "Cr", "Mn", "Fe", "Ni"),
    mass_fraction = c(
        0.000750, 0.001250, 0.005000, 0.000300, 0.000150, 0.180000,
        0.087500, 0.675050, 0.050000
    )
)

air <- data.frame(
    atom = c("C", "N", "O", "Ar"),
    mass_fraction = c(0.000124, 0.755268, 0.231781, 0.012827)
)

icrp <- data.frame(
    atom = c(
        "H", "C", "N", "O", "Na", "Mg",
        "P", "S", "Cl", "K", "Ca", "Fe", "Zn"
        ),
    mass_fraction = c(
        0.104472, 0.232190, 0.024880, 0.630238, 0.001130, 0.000130, 0.001330,
        0.001990, 0.001340, 0.001990, 0.000230, 0.000050, 0.000030
    )
)

# Scrap Metal Mass Fractions ----

get_scrap_metal <- function() {
    total_volume_cm3 <- 843.3 * 251.46 * 279.4

    df <-
        data.frame(
            material = c("Air", "Steel"),
            density_g_cm3 = c(0.001205, 7.86),
            allowed_mass_g = c(Inf, 2e7)
        ) |>
        mutate(volume_cm3 = total_volume_cm3 / density_g_cm3)

    df$volume_cm3[1] <- total_volume_cm3 - df$volume_cm3[2]

    df <- mutate(df,
        volume_fraction = volume_cm3 / total_volume_cm3,
        mass_g = density_g_cm3 * volume_cm3
    )
    total_mass_g = sum(df$mass_g)

    df$mass_fraction <- df$mass_g / total_mass_g

    scrap_metal <-
        rbind(
            mutate(air, scrap_fraction = df$mass_fraction[1]),
            mutate(steel, scrap_fraction = df$mass_fraction[2])
        ) |>
        mutate(
            total_mass_fraction = mass_fraction * scrap_fraction
        ) |>
        select(atom, total_mass_fraction) |>
        group_by(atom) |>
        summarise(mass_fraction = sum(total_mass_fraction))

    bulk_density_g_cm3 <- total_mass_g / total_volume_cm3

    return(list(bulk_density_g_cm3, scrap_metal))
}

# Foodstuff Mass Fractions ----

get_foodstuff <- function(packing_efficiency) {
    total_volume_cm3 <- 843.3 * 251.46 * 279.4

    df <-
        data.frame(
            material = c("Air", "ICRP"),
            density_g_cm3 = c(0.001205,1.0),
            volume_fraction = c(1 - packing_efficiency, packing_efficiency)
        ) |>
        mutate(
            volume_cm3 = volume_fraction * total_volume_cm3,
            mass_g = volume_cm3 * density_g_cm3
        )
    total_mass_g = sum(df$mass_g)
    bulk_density_g_cm3 = total_mass_g / total_volume_cm3

    df$mass_fraction <- df$mass_g / total_mass_g

    foodstuff <-
        rbind(
            mutate(air, food_fraction = df$mass_fraction[1]),
            mutate(icrp, food_fraction = df$mass_fraction[2])
        ) |>
        mutate(
            total_mass_fraction = mass_fraction * food_fraction
        ) |>
        select(atom, total_mass_fraction) |>
        group_by(atom) |>
        summarise(mass_fraction = sum(total_mass_fraction))

    return(list(bulk_density_g_cm3, foodstuff))
}

# Bulk attenuation coefficients ----

scrap_metal <- get_scrap_metal()
foodstuff50 <- get_foodstuff(0.5)
foodstuff90 <- get_foodstuff(0.9)
foodstuff30 <- get_foodstuff(0.3)

atomic_compositions <-
    rbind(
        mutate(
            scrap_metal[[2]],
            density = scrap_metal[[1]],
            material = "Scrap Metal"
        ),
        mutate(
            foodstuff30[[2]],
            density = foodstuff30[[1]],
            material = "Foodstuff (30%)"
        ),
        mutate(
            foodstuff50[[2]],
            density = foodstuff50[[1]],
            material = "Foodstuff (50%)"
        ),
        mutate(
            foodstuff90[[2]],
            density = foodstuff50[[1]],
            material = "Foodstuff (90%)"
        )
    )

atom_lookup <- Vectorize(function(.atom, .material) {
    output <- atomic_compositions |>
        filter(material == .material) |>
        filter(atom == .atom) |>
        pull(mass_fraction) |>
        unname()

    return(output[1])
})


materials <-
    attenuation_coefficients |>
    mutate(
        scrap_metal = unname(atom_lookup(atom, "Scrap Metal")),
        food30 = unname(atom_lookup(atom, "Foodstuff (30%)")),
        food50 = unname(atom_lookup(atom, "Foodstuff (50%)")),
        food90 = unname(atom_lookup(atom, "Foodstuff (90%)"))
    )

materials[is.na(materials$scrap_metal),]$scrap_metal <- 0
materials[is.na(materials$food30),]$food30 <- 0
materials[is.na(materials$food50),]$food50 <- 0
materials[is.na(materials$food90),]$food90 <- 0

materials <-
    materials |>
    rename(
        `Foodstuff (90%)` = food90,
        `Foodstuff (50%)` = food50,
        `Foodstuff (30%)` = food30,
        `Scrap Metal` = scrap_metal
    ) |>
    pivot_longer(
        cols = c(
            "Foodstuff (90%)",
            "Foodstuff (30%)",
            "Foodstuff (50%)",
            "Scrap Metal"
        ),
        names_to = "interior",
        values_to = "mass_fraction"
    ) |>
    as.data.frame() |>
    mutate(density_g_cm3 = Vectorize(function (.interior) {
        if (.interior == "Scrap Metal") {
            return(0.3387145)
        }
        else if (.interior == "Foodstuff (30%)") {
            return(foodstuff30[[1]])
        }
        else if (.interior == "Foodstuff (50%)") {
            return(foodstuff50[[1]])
        }
        else if (.interior == "Foodstuff (90%)") {
            return(foodstuff90[[1]])
        }
        else {
            return(0)
        }
    })(interior)) |>
    mutate(
        partial_mu = mu_cm2_g * mass_fraction * density_g_cm3,
        E_MeV = 1000 * E_MeV
    ) |>
    rename(E_keV = E_MeV) |>
    select(E_keV, atom, interior, partial_mu)




# Interoplate Energies ----

all_E_keV <- unique(materials$E_keV)
num_E_keV <- length(all_E_keV)
all_atom <- unique(materials$atom)
num_atom <- length(all_atom)
all_interior <- unique(materials$interior)
num_interior <- length(all_interior)

mu_lookup <- Vectorize(function(.E_keV, .atom, .interior) {
    element_subset <-
        materials |>
        filter(atom == .atom) |>
        filter(interior == .interior)

    mass_fraction <- element_subset$mass_fraction[1]
    element_subset <- select(element_subset, E_keV, partial_mu)

    if (.E_keV %in% element_subset$E_keV) {
        partial_mu <-
            element_subset |>
            filter(E_keV == .E_keV) |>
            pull(partial_mu)
        partial_mu <- partial_mu[1]
    }
    else {
        lower_energy <- max(
            element_subset[element_subset$E_keV < .E_keV,]$E_keV
        )
        upper_energy <- min(
            element_subset[element_subset$E_keV > .E_keV,]$E_keV
        )

        lower_mu <- element_subset[
            element_subset$E_keV == lower_energy,]$partial_mu[1]
        upper_mu <- element_subset[
            element_subset$E_keV == upper_energy,]$partial_mu[1]

        partial_mu <-
            lower_mu +
            (((upper_mu - lower_mu) / (upper_energy - lower_energy)) *
                 (.E_keV - lower_energy))
    }
    return(unlist(unname(partial_mu))[1])
})

materials2 <-
    data.frame(
        E_keV = rep(
            rep(all_E_keV, each = num_atom),
            times = num_interior
        ),
        atom = rep(
            rep(all_atom, times = num_E_keV),
            times = num_interior
        ),
        interior = rep(all_interior, each = num_atom * num_E_keV)
    ) |>
    mutate(partial_mu = mu_lookup(E_keV, atom, interior))



materials2 <-
    materials2 |>
    select(-atom) |>
    group_by(interior, E_keV) |>
    summarise(mu = sum(partial_mu)) |>
    mutate(interior = as.factor(interior))

ggplot(materials2, aes(x=E_keV, y=mu, color=interior)) + geom_line()+scale_x_log10()+scale_y_log10()

# Plot Creation ----









all_E <- unique(materials$E_MeV)

energy_interpolate <- function(element) {
    element_subset <- dplyr::filter(materials, Material == element)

    foods_mass_fraction <- element_subset$mass_fraction_foods[1]
    metal_mass_fraction <- element_subset$mass_fraction_metal[1]

    element_subset <- dplyr::select(element_subset, E_MeV, mu_cm2_g)

    for (energy in all_E) {
        if(!(energy %in% element_subset$E_MeV)) {

            lower_energy <- max(
                element_subset[element_subset$E_MeV < energy,]$E_MeV
            )
            upper_energy <- min(
                element_subset[element_subset$E_MeV > energy,]$E_MeV
            )

            lower_mu <- element_subset[
                element_subset$E_MeV == lower_energy,]$mu_cm2_g
            upper_mu <- element_subset[
                element_subset$E_MeV == upper_energy,]$mu_cm2_g

            estimated_mu <-
                lower_mu +
                (((upper_mu - lower_mu) / (upper_energy - lower_energy)) *
                     (energy - lower_energy))

            element_subset[nrow(element_subset) + 1,] <- c(energy, estimated_mu)
        }
    }

    element_subset <- dplyr::mutate(
        element_subset,
        Material = element,
        mass_fraction_foods = foods_mass_fraction,
        mass_fraction_metal = metal_mass_fraction
    )

    return(element_subset)
}

materials2 <- rbind(
    energy_interpolate("Hydrogen"),
    energy_interpolate("Carbon"),
    energy_interpolate("Nitrogen"),
    energy_interpolate("Oxygen"),
    energy_interpolate("Sodium"),
    energy_interpolate("Magnesium"),
    energy_interpolate("Silicon"),
    energy_interpolate("Phosphorus"),
    energy_interpolate("Sulfur"),
    energy_interpolate("Chlorine"),
    energy_interpolate("Argon"),
    energy_interpolate("Potassium"),
    energy_interpolate("Calcium"),
    energy_interpolate("Chromium"),
    energy_interpolate("Manganese"),
    energy_interpolate("Iron"),
    energy_interpolate("Zinc"),
    energy_interpolate("Nickel")
) |>
    dplyr::mutate(
        Material = as.factor(Material),
        density_foods = 0.5006025,
        density_metal = 0.3387145,
        partial_foods_mu_cm2_g = mass_fraction_foods * mu_cm2_g,
        partial_metal_mu_cm2_g = mass_fraction_metal * mu_cm2_g,
        partial_foods_mu_cm = density_foods * partial_foods_mu_cm2_g,
        partial_metal_mu_cm = density_metal * partial_metal_mu_cm2_g
    )

bulk_materials <- dplyr::mutate(
    data.frame(E_MeV = all_E),
    Foodstuff = 0,
    Scrap_Metal = 0
)

for (i in 1:nrow(bulk_materials)) {
    subset_i <- dplyr::filter(materials2, E_MeV == bulk_materials$E_MeV[i])
    bulk_materials$Foodstuff[i] <- sum(subset_i$partial_foods_mu_cm)
    bulk_materials$Scrap_Metal[i] <- sum(subset_i$partial_metal_mu_cm)
}

bulk_materials <- tidyr::pivot_longer(
    bulk_materials,
    cols = c("Foodstuff", "Scrap_Metal"),
    names_to = "Material",
    values_to = "mu_cm"
)

bulk_materials[
    bulk_materials$Material == "Scrap_Metal",
]$Material <- "Scrap Metal"
























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

ggsave(
    paste(
        getwd(),
        "/",
        "data_raw/4_figure_generation/gg/",
        "fig-methods-cargo_atten.png",
        sep = ""
    ),
    plot = cargo_atten,
    width = 7,
    height = 5,
    units = "in",
    dpi = 1200
)
