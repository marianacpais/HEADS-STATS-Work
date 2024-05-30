ds_a_path <- "datasets/Anesthesia-DatasetA.sav"
ds_b_path <- "datasets/Anesthesia-DatasetB.sav"
ds_c_path <- "datasets/Cancer-DatasetC.sav"
ds_d_path <- "datasets/PA-DatasetD.sav"

library("haven")

ds_a <- read_spss(ds_a_path)
ds_b <- read_spss(ds_b_path)
ds_c <- read_spss(ds_c_path)
ds_d <- read_spss(ds_d_path)

plot(ds_d$weight, ds_d$height)
