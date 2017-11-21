library(tidyverse)
library(readxl)
library(lubridate)
library(edwr)

dir_raw <- "data/raw"

pts <- read_excel("data/raw/patient_list.xlsx", sheet = "All Patients") %>%
    select(FIN, DATE, TIME) %>%
    rename_all(str_to_lower) %>%
    mutate(code_datetime = ymd_hms(paste(paste(year(date), month(date), day(date), sep = "/"),
                                         paste(hour(time), minute(time), second(time), sep = ":"),
                                         sep = " "),
                                   tz = "US/Central")) %>%
    select(fin, code_datetime)

mbo_fin <- concat_encounters(pts$fin)

# run MBO query
#   * Identifiers - by FIN

id <- read_data(dir_raw, "id", FALSE) %>%
    rename(millennium.id = `Encounter Identifier`,
           fin = `Financial Number`)

mbo_id <- concat_encounters(id$millennium.id)

# run MBO queries
#   * Clinical Events - Prompt
#       - Clinical Event: SpO2 percent;Apical Heart Rate;Respiratory Rate
#   * Labs - CBC
#   * Vitals - BP
#   * Vitals - Temp
