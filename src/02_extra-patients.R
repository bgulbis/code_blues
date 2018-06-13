library(tidyverse)
library(lubridate)
library(edwr)

id_col <- "millennium.id"

pts <- "data/raw/extra_patients.csv" %>%
    read_csv(
        col_types = cols(
            fin = col_character(),
            date = col_date("%m/%d/%Y"),
            time = col_time()
        )
    ) %>%
    unite(code_datetime, date, time, sep = " ") %>%
    mutate_at("code_datetime", ymd_hms)

mbo_fin <- concat_encounters(pts$fin)

# run MBO query
#   * Identifiers - by FIN

dir_raw <- "data/raw/extra_pts"

dirr::gzip_files(dir_raw)

id <- read_data(dir_raw, "id", FALSE) %>%
    as.id()

mbo_id <- concat_encounters(id$millennium.id)

data_patients <- pts %>%
    left_join(id, by = "fin") %>%
    select(millennium.id, code_datetime)

# run MBO queries
#   * Clinical Events - No Order Id - Prompt
#       - Clinical Event: SpO2 percent;Respiratory Rate
#   * Labs - CBC
#   * Vitals - BP
#   * Vitals - HR
#   * Vitals - Temp

# Temperature Oral;Temperature Tympanic;Temperature Rectal;Temperature Axillary;Temperature Intravascular;Temperature Esophageal;Temperature;Temperature Bladder;Temperature Skin;Temperature Brain;Temperature Sensor

resp <- read_data(dir_raw, "events-resp", FALSE) %>%
    as.events(order_var = FALSE) %>%
    inner_join(data_patients, by = id_col) %>%
    filter(
        event.datetime >= code_datetime - hours(24),
        event.datetime <= code_datetime + hours(24)
    )

labs <- read_data(dir_raw, "labs", FALSE) %>%
    as.labs() %>%
    tidy_data() %>%
    inner_join(data_patients, by = id_col) %>%
    filter(
        lab %in% c("hgb", "hct"),
        lab.datetime >= code_datetime - hours(24),
        lab.datetime <= code_datetime + hours(24)
    )

vitals <- read_data(dir_raw, "vitals", FALSE) %>%
    as.vitals() %>%
    inner_join(data_patients, by = id_col) %>%
    filter(
        vital.datetime >= code_datetime - hours(24),
        vital.datetime <= code_datetime + hours(24)
    )

vitals_temp <- vitals %>%
    filter(str_detect(vital, "temperature"))

vitals_bp <- vitals %>%
    filter(str_detect(vital, "pressure|bp"))

vitals_hr <- vitals %>%
    filter(str_detect(vital, "rate"))

# SBP < 40, > 300; DBP < 20, > 200; RR > 50, < 3; SPO2 < 10; MAP < 20, > 200

data_sbp <- vitals_bp %>%
    filter(
        vital %in% c("systolic blood pressure", "arterial systolic bp 1"),
        vital.result >= 40,
        vital.result < 300
    ) %>%
    mutate(
        event_group = "sbp",
        pre_code = vital.datetime < code_datetime
    ) %>%
    left_join(id, by = id_col) %>%
    select(
        fin,
        event.datetime = vital.datetime,
        pre_code,
        event_group,
        event = vital,
        event.result = vital.result
    )

data_dbp <- vitals_bp %>%
    filter(
        vital %in% c("diastolic blood pressure", "arterial diastolic bp 1"),
        vital.result >= 20,
        vital.result < 200
    ) %>%
    mutate(
        event_group = "dbp",
        pre_code = vital.datetime < code_datetime
    ) %>%
    left_join(id, by = id_col) %>%
    select(
        fin,
        event.datetime = vital.datetime,
        pre_code,
        event_group,
        event = vital,
        event.result = vital.result
    )

data_map <- vitals_bp %>%
    filter(
        vital %in% c("mean arterial pressure (invasive)", "mean arterial pressure"),
        vital.result >= 20,
        vital.result < 200
    ) %>%
    mutate(
        event_group = "map",
        pre_code = vital.datetime < code_datetime
    ) %>%
    left_join(id, by = id_col) %>%
    select(
        fin,
        event.datetime = vital.datetime,
        pre_code,
        event_group,
        event = vital,
        event.result = vital.result
    )

data_hr <- vitals_hr %>%
    filter(
        vital %in% c("apical heart rate", "peripheral pulse rate"),
        vital.result >0
    ) %>%
    mutate(
        event_group = "hr",
        pre_code = vital.datetime < code_datetime
    ) %>%
    left_join(id, by = id_col) %>%
    select(
        fin,
        event.datetime = vital.datetime,
        pre_code,
        event_group,
        event = vital,
        event.result = vital.result
    )

data_rr <- resp %>%
    filter(
        event == "respiratory rate",
        event.result >= 3,
        event.result < 50
    ) %>%
    mutate(
        event_group = "rr",
        pre_code = event.datetime < code_datetime
    ) %>%
    left_join(id, by = id_col) %>%
    select(
        fin,
        event.datetime,
        pre_code,
        event_group,
        event,
        event.result
    )

data_spo2 <- resp %>%
    filter(
        event == "spo2 percent",
        event.result >0
    ) %>%
    mutate(
        event_group = "spo2",
        pre_code = event.datetime < code_datetime
    ) %>%
    left_join(id, by = id_col) %>%
    select(
        fin,
        event.datetime,
        pre_code,
        event_group,
        event,
        event.result
    )

data_temp <- vitals_temp %>%
    mutate(
        event_group = "temp",
        pre_code = vital.datetime < code_datetime
    ) %>%
    left_join(id, by = id_col) %>%
    select(
        fin,
        event.datetime = vital.datetime,
        pre_code,
        event_group,
        event = vital,
        event.result = vital.result
    )

data_hgb <- labs %>%
    filter(lab == "hgb") %>%
    mutate(
        event_group = "hgb",
        pre_code = lab.datetime < code_datetime
    ) %>%
    left_join(id, by = id_col) %>%
    select(
        fin,
        event.datetime = lab.datetime,
        pre_code,
        event_group,
        event = lab,
        event.result = lab.result
    )

data_hct <- labs %>%
    filter(lab == "hct") %>%
    mutate(
        event_group = "hct",
        pre_code = lab.datetime < code_datetime
    ) %>%
    left_join(id, by = id_col) %>%
    select(
        fin,
        event.datetime = lab.datetime,
        pre_code,
        event_group,
        event = lab,
        event.result = lab.result
    )

write.csv(data_sbp, "data/external/extra/data_sbp.csv")
write.csv(data_dbp, "data/external/extra/data_dbp.csv")
write.csv(data_map, "data/external/extra/data_map.csv")
write.csv(data_hr, "data/external/extra/data_hr.csv")
write.csv(data_rr, "data/external/extra/data_rr.csv")
write.csv(data_spo2, "data/external/extra/data_spo2.csv")
write.csv(data_temp, "data/external/extra/data_temp.csv")
write.csv(data_hgb, "data/external/extra/data_hgb.csv")
write.csv(data_hct, "data/external/extra/data_hct.csv")

data_files <- list.files(
    "data/external/extra",
    pattern = "csv",
    full.names = TRUE
)

zip("data/external/data_codes.zip", data_files, flags = "-j")

