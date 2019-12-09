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

df <- read_data2("data/raw/peter", "peter") %>%
    inner_join(target_pts, by = "fin")

labs_prior <- df %>%
    filter(lab.datetime < code_start) %>%
    arrange(millennium.id, desc(lab.datetime)) %>%
    distinct(millennium.id, lab, .keep_all = TRUE) %>%
    select(fin, lab.datetime, lab, lab.result) %>%
    spread(lab, lab.result)

labs_during <- df %>%
    filter(
        lab.datetime >= code_start,
        lab.datetime <= code_end
    ) %>%
    select(fin, lab.datetime, lab, lab.result) %>%
    spread(lab, lab.result)

write.xlsx(
    list(labs_prior = labs_prior, labs_during = labs_during),
    "data/external/fy19/labs_before-during_code.xlsx")
