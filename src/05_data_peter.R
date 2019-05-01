library(tidyverse)
library(readxl)
library(lubridate)
library(edwr)
library(openxlsx)

target_pts <- read_excel(
    "data/external/fy19/abg_list.xlsx",
    col_names = c("fin", "code_start", "code_end"),
    skip = 1
)

mbo_fins <- concat_encounters(target_pts$fin)
print(mbo_fins)
