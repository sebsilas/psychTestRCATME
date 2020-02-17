input <- read.csv("data-raw/dict.csv", stringsAsFactors = FALSE)
ptrcat_dict <- psychTestR::i18n_dict$new(input)

usethis::use_data(ptrcat_dict, overwrite = TRUE)
