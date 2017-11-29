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
#       - Clinical Event: SpO2 percent;Apical Heart Rate;Respiratory Rate;Peripheral Pulse Rate;Hgb;Hct
#   * Clinical Events - No Order Id - Prompt
#       - Clinical Event: Hgb, Hct

#   * Labs - CBC
#   * Vitals - BP
#   * Vitals - Temp

labs <- read_data(dir_raw, "events-cbc", FALSE) %>%
    as.events(order_var = FALSE) %>%
    mutate(censor.low = str_detect(event.result, "<"),
           censor.high = str_detect(event.result, ">")) %>%
    mutate_at("event.result", as.numeric) %>%
    inner_join(data_patients, by = "millennium.id") %>%
    filter(event %in% c("hgb", "hct"),
           event.datetime >= code_datetime - hours(24),
           event.datetime <= code_datetime + hours(24))

vitals_temp <- read_data(dir_raw, "events-temp", FALSE) %>%
    as.events(order_var = FALSE) %>%
    mutate(censor.low = str_detect(event.result, "<"),
           censor.high = str_detect(event.result, ">")) %>%
    mutate_at("event.result", as.numeric) %>%
    inner_join(data_patients, by = "millennium.id") %>%
    filter(event.datetime >= code_datetime - hours(24),
           event.datetime <= code_datetime + hours(24))

vitals_bp <- read_data(dir_raw, "vitals-bp", FALSE) %>%
    as.vitals() %>%
    inner_join(data_patients, by = "millennium.id") %>%
    filter(vital.datetime >= code_datetime - hours(24),
           vital.datetime <= code_datetime + hours(24))

vitals_hr <- read_data(dir_raw, "vitals-hr", FALSE) %>%
    as.events(order_var = FALSE) %>%
    inner_join(data_patients, by = "millennium.id") %>%
    filter(event.datetime >= code_datetime - hours(24),
           event.datetime <= code_datetime + hours(24)) %>%
    # select(millennium.id, code_datetime, vital.datetime = event.datetime, vital = event, vital.result = event.result) %>%
    mutate_at("event.result", as.numeric)

x <- vitals_bp %>%
    select(event = vital) %>%
    distinct(event) %>%
    bind_rows(distinct(labs, event), distinct(vitals_hr, event), distinct(vitals_temp, event))

# SBP < 40, > 300; DBP < 20, > 200; RR > 50, < 3; SPO2 < 10; MAP < 20, > 200

data_sbp <- vitals_bp %>%
    filter(vital %in% c("systolic blood pressure", "arterial systolic bp 1"),
           vital.result >= 40,
           vital.result < 300) %>%
    mutate(event_group = "sbp",
           pre_code = vital.datetime < code_datetime) %>%
    select(fin, event.datetime = vital.datetime, pre_code, event_group, event = vital, event.result = vital.result)

data_dbp <- vitals_bp %>%
    filter(vital %in% c("diastolic blood pressure", "arterial diastolic bp 1"),
           vital.result >= 20,
           vital.result < 200) %>%
    mutate(event_group = "dbp",
           pre_code = vital.datetime < code_datetime) %>%
    select(fin, event.datetime = vital.datetime, pre_code, event_group, event = vital, event.result = vital.result)

data_map <- vitals_bp %>%
    filter(vital %in% c("mean arterial pressure (invasive)", "mean arterial pressure"),
           vital.result >= 20,
           vital.result < 200) %>%
    mutate(event_group = "map",
           pre_code = vital.datetime < code_datetime) %>%
    select(fin, event.datetime = vital.datetime, pre_code, event_group, event = vital, event.result = vital.result)

data_hr <- vitals_hr %>%
    filter(event %in% c("apical heart rate", "peripheral pulse rate"),
           event.result >0) %>%
    mutate(event_group = "hr",
           pre_code = event.datetime < code_datetime) %>%
    select(fin, event.datetime, pre_code, event_group, event, event.result)

data_rr <- vitals_hr %>%
    filter(event == "respiratory rate",
           event.result >= 3,
           event.result < 50) %>%
    mutate(event_group = "rr",
           pre_code = event.datetime < code_datetime) %>%
    select(fin, event.datetime, pre_code, event_group, event, event.result)

data_spo2 <- vitals_hr %>%
    filter(event == "spo2 percent",
           event.result >0) %>%
    mutate(event_group = "spo2",
           pre_code = event.datetime < code_datetime) %>%
    select(fin, event.datetime, pre_code, event_group, event, event.result)

data_temp <- vitals_temp %>%
    filter(str_detect(event, "temperature")) %>%
    mutate(event_group = "temp",
           pre_code = event.datetime < code_datetime) %>%
    select(fin, event.datetime, pre_code, event_group, event, event.result)

data_hgb <- labs %>%
    filter(event == "hgb") %>%
    mutate(event_group = "hgb",
           pre_code = event.datetime < code_datetime) %>%
    select(fin, event.datetime, pre_code, event_group, event, event.result)

data_hct <- labs %>%
    filter(event == "hct") %>%
    mutate(event_group = "hct",
           pre_code = event.datetime < code_datetime) %>%
    select(fin, event.datetime, pre_code, event_group, event, event.result)

write.csv(data_sbp, "data/external/data_sbp.csv")
write.csv(data_dbp, "data/external/data_dbp.csv")
write.csv(data_map, "data/external/data_map.csv")
write.csv(data_hr, "data/external/data_hr.csv")
write.csv(data_rr, "data/external/data_rr.csv")
write.csv(data_spo2, "data/external/data_spo2.csv")
write.csv(data_temp, "data/external/data_temp.csv")
write.csv(data_hgb, "data/external/data_hgb.csv")
write.csv(data_hct, "data/external/data_hct.csv")

data_files <- list.files("data/external", pattern = "csv", full.names = TRUE)
zip("data/external/data_codes.zip", data_files, flags = "-j")


# data_vitals <- vitals_bp %>%
#     bind_rows(vitals_hr) %>%
#     mutate_at("vital.datetime", funs(floor_date(., unit = "min"))) %>%
#     arrange(millennium.id, vital.datetime) %>%
#     distinct(millennium.id, code_datetime, vital.datetime, vital, .keep_all = TRUE) %>%
#     group_by(millennium.id, code_datetime) %>%
#     spread(vital, vital.result)
