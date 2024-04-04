# Parse inputs for isotropic source ----
library(tidyverse)
library(pracma)
library(btools)

fileList <- as.vector(readLines(
    "data_raw/bareSource/isotropic/outputs/deckList.txt"
))
fileList <- fileList[2:length(fileList)]

fileList <- paste("out_", fileList, sep = "")

isotropic <- mcnparse(
    fileList,
    directory = "data_raw/bareSource/isotropic/outputs/"
)

isotropic <- isotropic$data

isotropic <- mutate(
    isotropic,
    Es_keV = 0,
    Ed_keV = bin_mev * 1e3,
    bin_mev = NULL)
    
isotropic <- filter(isotropic, complete.cases(isotropic))

isotropic[isotropic$tally == "11",]$tally <- "F1"
isotropic[isotropic$tally == "18",]$tally <- "F8"

for (i in 1:nrow(isotropic)) {
    isotropic$Es_keV[i] <- as.numeric(stringsplit(
        isotropic$run_id[i],
        splt = "-")[2])
}

isotropic <- isotropic |> 
    pivot_wider(
        names_from = "tally",
        values_from = c("value","re")
    ) |> 
    mutate(
        F8 = value_F8,
        F1 = value_F1,
        uF8 = value_F8 * re_F8,
        uF1 = value_F1 * re_F1,
        run_id = NULL,
        value_F8 = NULL,
        value_F1 = NULL,
        re_F1 = NULL,
        re_F8 = NULL
    )

save(isotropic, file = "data/isotropic.rda")
