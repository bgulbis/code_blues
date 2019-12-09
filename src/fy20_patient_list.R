library(readxl)
library(edwr)

pts <- read_excel(
    "data/external/fy20/patients.xlsx",
    col_names = c("id", "fin"),
    skip = 1
)

mbo_fin <- concat_encounters(pts$fin)
print(mbo_fin)
