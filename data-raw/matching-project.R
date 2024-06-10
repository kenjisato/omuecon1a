## code to prepare project.zip

odir <- setwd(file.path("data-raw", "matching-project"))

utils::zip(here::here("inst", "zip", "matching-project.zip"),
           ".",
           flags = "-r9X")

setwd(odir)
