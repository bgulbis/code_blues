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

data_patients <- pts %>%
    left_join(id, by = "fin") %>%
    arrange(millennium.id, code_datetime) %>%
    distinct(millennium.id, .keep_all = TRUE)

# run MBO queries
#   * Clinical Events - No Order Id - Prompt
#       - Clinical Event: SpO2 percent;Apical Heart Rate;Respiratory Rate;Peripheral Pulse Rate
#   * Labs - CBC
#   * Vitals - BP
#   * Vitals - Temp

labs <- read_data(dir_raw, "labs", FALSE) %>%
    as.labs() %>%
    tidy_data() %>%
    inner_join(data_patients, by = "millennium.id") %>%
    filter(lab %in% c("hgb", "hct"),
           lab.datetime >= code_datetime - hours(24),
           lab.datetime <= code_datetime + hours(24))

vitals_bp <- read_data(dir_raw, "vitals-bp", FALSE) %>%
    as.vitals() %>%
    inner_join(data_patients, by = "millennium.id") %>%
    filter(vital.datetime >= code_datetime - hours(24),
           vital.datetime <= code_datetime + hours(24)) %>%
    select(millennium.id, code_datetime, vital.datetime, vital, vital.result)

vitals_hr <- read_data(dir_raw, "vitals-hr", FALSE) %>%
    as.events(order_var = FALSE) %>%
    inner_join(data_patients, by = "millennium.id") %>%
    filter(event.datetime >= code_datetime - hours(24),
           event.datetime <= code_datetime + hours(24)) %>%
    select(millennium.id, code_datetime, vital.datetime = event.datetime, vital = event, vital.result = event.result) %>%
    mutate_at("vital.result", as.numeric)

data_vitals <- vitals_bp %>%
    bind_rows(vitals_hr) %>%
    mutate_at("vital.datetime", funs(floor_date(., unit = "min"))) %>%
    arrange(millennium.id, vital.datetime) %>%
    distinct(millennium.id, code_datetime, vital.datetime, vital, .keep_all = TRUE) %>%
    group_by(millennium.id, code_datetime) %>%
    spread(vital, vital.result)
