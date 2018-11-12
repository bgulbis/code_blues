library(tidyverse)
library(readxl)
library(lubridate)
library(edwr)

dir_raw <- "data/raw/fy19"

dirr::gzip_files(dir_raw)

# patient list -----------------------------------------
pts <- read_excel("data/external/fy19/patient_list.xlsx") %>%
    select(FIN, DATE, TIME) %>%
    rename_all(str_to_lower) %>%
    mutate(
        code.datetime =
            ymd_hms(
                paste(
                    paste(year(date), month(date), day(date), sep = "/"),
                    paste(hour(time), minute(time), second(time), sep = ":"),
                    sep = " "),
                tz = "US/Central"
            )
    ) %>%
    select(fin, code.datetime)

mbo_fin <- concat_encounters(pts$fin)
mbo_fin

# patient id -------------------------------------------

# run MBO query
#   * Identifiers - by FIN

id <- read_data(dir_raw, "identifiers") %>%
    as.id()

data_patients <- pts %>%
    left_join(id, by = "fin") %>%
    arrange(millennium.id, code.datetime) %>%
    distinct(millennium.id, .keep_all = TRUE)

mbo_id <- concat_encounters(data_patients$millennium.id)

mbo_id_split <- concat_encounters(data_patients$millennium.id, 300)

# 01_demographics --------------------------------------

print(mbo_id)

data_demographics <- read_data(dir_raw, "demographics", FALSE) %>%
    as.demographics(
        extras = c(
            "admit.datetime" = "Date and Time - Admit",
            "discharge.datetime" = "Date and Time - Discharge"
        )
    ) %>%
    format_dates(c("admit.datetime", "discharge.datetime")) %>%
    filter(facility != "SE Southeast")

data_include <- data_patients %>%
    semi_join(data_demographics, by = "millennium.id") %>%
    select(millennium.id, pie.id, code.datetime)

# 02_vitals --------------------------------------------

# Clinical Event: Mean Arterial Pressure;Mean Arterial Pressure (Invasive);Systolic Blood Pressure;Arterial Systolic BP 1;Diastolic Blood Pressure;Arterial Diastolic BP 1;Apical Heart Rate;Peripheral Pulse Rate;Respiratory Rate;SpO2 percent

print(mbo_id_split)

data_vitals <- read_data(dir_raw, "vitals", FALSE) %>%
    as.vitals() %>%
    inner_join(data_include, by = "millennium.id") %>%
    arrange(millennium.id, vital.datetime)

vitals_first <- data_vitals %>%
    group_by(millennium.id, vital) %>%
    filter(vital.datetime == min(vital.datetime)) %>%
    select(millennium.id, vital, vital.result) %>%
    distinct(millennium.id, vital, .keep_all = TRUE) %>%
    spread(vital, vital.result)

vitals_code <- data_vitals %>%
    group_by(millennium.id) %>%
    filter(
        vital.datetime >= code.datetime - hours(6),
        vital.datetime <= code.datetime + hours(12)
    )

# 03_temperatures --------------------------------------

# Clinical Event: Temperature Oral;Temperature Tympanic;Temperature Rectal;Temperature Axillary;Temperature Intravascular;Temperature Esophageal;Temperature;Temperature Bladder;Temperature Skin;Temperature Brain;Temperature Sensor

data_temps <- read_data(dir_raw, "temps", FALSE) %>%
    as.vitals() %>%
    inner_join(data_include, by = "millennium.id") %>%
    arrange(millennium.id, vital.datetime)

temps_first <- data_temps %>%
    distinct(millennium.id, .keep_all = TRUE) %>%
    select(millennium.id, temperature = vital.result)

temps_code <- data_temps %>%
    group_by(millennium.id) %>%
    filter(
        vital.datetime >= code.datetime - hours(6),
        vital.datetime <= code.datetime + hours(12)
    )

# 04_labs ----------------------------------------------

# Lab Event: Hct;Hgb;WBC;POC A pH;POC A PO2;Creatinine Lvl;Potassium Lvl;Sodium Lvl

data_labs <- read_data(dir_raw, "labs", FALSE) %>%
    as.labs() %>%
    tidy_data() %>%
    inner_join(data_include, by = "millennium.id") %>%
    arrange(millennium.id, lab.datetime)

labs_first <- data_labs %>%
    group_by(millennium.id, lab) %>%
    filter(lab.datetime == min(lab.datetime)) %>%
    select(millennium.id, lab, lab.result) %>%
    distinct(millennium.id, lab, .keep_all = TRUE) %>%
    spread(lab, lab.result)

labs_code <- data_labs %>%
    group_by(millennium.id) %>%
    filter(
        lab.datetime >= code.datetime - hours(6),
        lab.datetime <= code.datetime + hours(12)
    )

# 05_medications ---------------------------------------

data_meds <- read_data(dir_raw, "meds", FALSE) %>%
    as.meds_inpt() %>%
    inner_join(data_include, by = "millennium.id") %>%
    arrange(millennium.id, med.datetime)

meds_arrhythm <- data_meds %>%
    filter(
        med %in% c("amiodarone", "lidocaine"),
        med.datetime < code.datetime
    ) %>%
    distinct(millennium.id, med) %>%
    mutate(dose = TRUE) %>%
    spread(med, dose)

meds_code <- data_meds %>%
    filter(
        med %in% c(
            "amiodarone",
            "lidocaine",
            "dopamine",
            "norepinephrine",
            "epinephrine",
            "phenylephrine",
            "vasopressin",
            "dobutamine",
            "milrinone"
        ),
        med.datetime >= code.datetime
    ) %>%
    distinct(millennium.id, med) %>%
    mutate(dose = TRUE) %>%
    spread(med, dose)

meds_electrolytes <- data_meds %>%
    filter(
        str_detect(med, "^potassium|phosphate|calcium chloride|calcium gluconate|magnesium sulfate"),
        med.datetime >= code.datetime
    ) %>%
    distinct(millennium.id, med) %>%
    mutate(dose = TRUE) %>%
    spread(med, dose)

# 06_locations -----------------------------------------

print(mbo_id)

data_locations <- read_data(dir_raw, "locations", FALSE) %>%
    as.locations() %>%
    filter(depart.datetime <= today(tzone = "US/Central")) %>%
    tidy_data() %>%
    inner_join(data_include, by = "millennium.id")

location_code <- data_locations %>%
    mutate(
        code = code.datetime >= arrive.datetime & code.datetime <= depart.datetime,
        time.code = difftime(
            arrive.datetime,
            code.datetime,
            units = "hours"
        )
    ) %>%
    filter(code | (time.code > 0 & time.code <= 2))

# 07_measures ------------------------------------------

print(mbo_id_split)

data_measures <- read_data(dir_raw, "measures", FALSE) %>%
    as.events(order_var = FALSE) %>%
    inner_join(data_include, by = "millennium.id") %>%
    arrange(millennium.id, event.datetime)

weights_first <- data_measures %>%
    filter(event == "weight") %>%
    distinct(millennium.id, .keep_all = TRUE) %>%
    select(millennium.id, weight = event.result)

# 08_gcs -----------------------------------------------

data_gcs <- read_data(dir_raw, "gcs", FALSE) %>%
    as.events(order_var = FALSE) %>%
    inner_join(data_include, by = "millennium.id") %>%
    arrange(millennium.id, event.datetime)

gcs_first <- data_gcs %>%
    filter(event == "glasgow coma score") %>%
    distinct(millennium.id, .keep_all = TRUE)

# 09_hypothermia ---------------------------------------

print(mbo_id)

data_hypothermia <- read_data(dir_raw, "hypothermia", FALSE) %>%
    as.order_info()

# 10_readmission ---------------------------------------

edw_person_id <- concat_encounters(data_patients$person.id)
print(edw_person_id)

# run EDW query
#   * Encounters - by Person ID

encounters <- read_data(dir_raw, "encounters") %>%
    as.encounters()

data_readmits <- encounters %>%
    filter(visit.type == "Inpatient") %>%
    rename(encounter.datetime = admit.datetime) %>%
    left_join(
        data_demographics[c(
            "millennium.id",
            "admit.datetime",
            "discharge.datetime"
        )],
        by = "millennium.id"
    ) %>%
    arrange(person.id, encounter.datetime) %>%
    group_by(person.id) %>%
    fill(admit.datetime, .direction = "down") %>%
    filter(
        !is.na(admit.datetime),
        disposition != "Cadaver Organ Donor"
    ) %>%
    mutate(
        readmit.days = difftime(
            encounter.datetime,
            admit.datetime,
            units = "days"
        )
    ) %>%
    filter(
        readmit.days > 0,
        readmit.days <= 30
    ) %>%
    ungroup() %>%
    select(
        millennium.id,
        readmit.datetime = encounter.datetime,
        readmit.facility = facility,
        readmit.days
    )


# run MBO queries
# Hypothermia protocol, code status

# tidy data sets ---------------------------------------

tidy_patients <- data_demographics %>%
    left_join(data_readmits, by = "millennium.id") %>%
    left_join(weights_first, by = "millennium.id")

tidy_admit_vals <- temps_first %>%
    full_join(labs_first, by = "millennium.id") %>%
    full_join(vitals_first, by = "millennium.id") %>%
    full_join(gcs_first, by = "millennium.id") %>%
    full_join(meds_arrhythm, by = "millennium.id")

tidy_meds_post <- meds_code %>%
    full_join(meds_electrolytes, by = "millennium.id")


# old --------------------------------------------------

# labs <- read_data(dir_raw, "events-cbc", FALSE) %>%
#     as.events(order_var = FALSE) %>%
#     mutate(censor.low = str_detect(event.result, "<"),
#            censor.high = str_detect(event.result, ">")) %>%
#     mutate_at("event.result", as.numeric) %>%
#     inner_join(data_patients, by = "millennium.id") %>%
#     filter(event %in% c("hgb", "hct"),
#            event.datetime >= code_datetime - hours(24),
#            event.datetime <= code_datetime + hours(24))
#
# vitals_temp <- read_data(dir_raw, "events-temp", FALSE) %>%
#     as.events(order_var = FALSE) %>%
#     mutate(censor.low = str_detect(event.result, "<"),
#            censor.high = str_detect(event.result, ">")) %>%
#     mutate_at("event.result", as.numeric) %>%
#     inner_join(data_patients, by = "millennium.id") %>%
#     filter(event.datetime >= code_datetime - hours(24),
#            event.datetime <= code_datetime + hours(24))
#
# vitals_bp <- read_data(dir_raw, "vitals-bp", FALSE) %>%
#     as.vitals() %>%
#     inner_join(data_patients, by = "millennium.id") %>%
#     filter(vital.datetime >= code_datetime - hours(24),
#            vital.datetime <= code_datetime + hours(24))
#
# vitals_hr <- read_data(dir_raw, "vitals-hr", FALSE) %>%
#     as.events(order_var = FALSE) %>%
#     inner_join(data_patients, by = "millennium.id") %>%
#     filter(event.datetime >= code_datetime - hours(24),
#            event.datetime <= code_datetime + hours(24)) %>%
#     # select(millennium.id, code_datetime, vital.datetime = event.datetime, vital = event, vital.result = event.result) %>%
#     mutate_at("event.result", as.numeric)
#
# x <- vitals_bp %>%
#     select(event = vital) %>%
#     distinct(event) %>%
#     bind_rows(distinct(labs, event), distinct(vitals_hr, event), distinct(vitals_temp, event))
#
# # SBP < 40, > 300; DBP < 20, > 200; RR > 50, < 3; SPO2 < 10; MAP < 20, > 200
#
# data_sbp <- vitals_bp %>%
#     filter(vital %in% c("systolic blood pressure", "arterial systolic bp 1"),
#            vital.result >= 40,
#            vital.result < 300) %>%
#     mutate(event_group = "sbp",
#            pre_code = vital.datetime < code_datetime) %>%
#     select(fin, event.datetime = vital.datetime, pre_code, event_group, event = vital, event.result = vital.result)
#
# data_dbp <- vitals_bp %>%
#     filter(vital %in% c("diastolic blood pressure", "arterial diastolic bp 1"),
#            vital.result >= 20,
#            vital.result < 200) %>%
#     mutate(event_group = "dbp",
#            pre_code = vital.datetime < code_datetime) %>%
#     select(fin, event.datetime = vital.datetime, pre_code, event_group, event = vital, event.result = vital.result)
#
# data_map <- vitals_bp %>%
#     filter(vital %in% c("mean arterial pressure (invasive)", "mean arterial pressure"),
#            vital.result >= 20,
#            vital.result < 200) %>%
#     mutate(event_group = "map",
#            pre_code = vital.datetime < code_datetime) %>%
#     select(fin, event.datetime = vital.datetime, pre_code, event_group, event = vital, event.result = vital.result)
#
# data_hr <- vitals_hr %>%
#     filter(event %in% c("apical heart rate", "peripheral pulse rate"),
#            event.result >0) %>%
#     mutate(event_group = "hr",
#            pre_code = event.datetime < code_datetime) %>%
#     select(fin, event.datetime, pre_code, event_group, event, event.result)
#
# data_rr <- vitals_hr %>%
#     filter(event == "respiratory rate",
#            event.result >= 3,
#            event.result < 50) %>%
#     mutate(event_group = "rr",
#            pre_code = event.datetime < code_datetime) %>%
#     select(fin, event.datetime, pre_code, event_group, event, event.result)
#
# data_spo2 <- vitals_hr %>%
#     filter(event == "spo2 percent",
#            event.result >0) %>%
#     mutate(event_group = "spo2",
#            pre_code = event.datetime < code_datetime) %>%
#     select(fin, event.datetime, pre_code, event_group, event, event.result)
#
# data_temp <- vitals_temp %>%
#     filter(str_detect(event, "temperature")) %>%
#     mutate(event_group = "temp",
#            pre_code = event.datetime < code_datetime) %>%
#     select(fin, event.datetime, pre_code, event_group, event, event.result)
#
# data_hgb <- labs %>%
#     filter(event == "hgb") %>%
#     mutate(event_group = "hgb",
#            pre_code = event.datetime < code_datetime) %>%
#     select(fin, event.datetime, pre_code, event_group, event, event.result)
#
# data_hct <- labs %>%
#     filter(event == "hct") %>%
#     mutate(event_group = "hct",
#            pre_code = event.datetime < code_datetime) %>%
#     select(fin, event.datetime, pre_code, event_group, event, event.result)
#
# write.csv(data_sbp, "data/external/data_sbp.csv")
# write.csv(data_dbp, "data/external/data_dbp.csv")
# write.csv(data_map, "data/external/data_map.csv")
# write.csv(data_hr, "data/external/data_hr.csv")
# write.csv(data_rr, "data/external/data_rr.csv")
# write.csv(data_spo2, "data/external/data_spo2.csv")
# write.csv(data_temp, "data/external/data_temp.csv")
# write.csv(data_hgb, "data/external/data_hgb.csv")
# write.csv(data_hct, "data/external/data_hct.csv")
#
# data_files <- list.files("data/external", pattern = "csv", full.names = TRUE)
# zip("data/external/data_codes.zip", data_files, flags = "-j")
#

# data_vitals <- vitals_bp %>%
#     bind_rows(vitals_hr) %>%
#     mutate_at("vital.datetime", funs(floor_date(., unit = "min"))) %>%
#     arrange(millennium.id, vital.datetime) %>%
#     distinct(millennium.id, code_datetime, vital.datetime, vital, .keep_all = TRUE) %>%
#     group_by(millennium.id, code_datetime) %>%
#     spread(vital, vital.result)
