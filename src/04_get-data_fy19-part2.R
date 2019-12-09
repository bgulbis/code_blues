library(tidyverse)
library(readxl)
library(lubridate)
library(edwr)
library(openxlsx)

dir_raw <- "data/raw/fy19_2"

# patient list -----------------------------------------
pts <- read_excel("data/external/fy19/second_list.xlsx") %>%
    rename(
        fin = MRN,
        date = `Code Date`,
        time = `Code Time`
    ) %>%
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

# run EDW query
#   * Identifiers - by FIN

id <- read_data2(dir_raw, "identifiers")

data_patients <- pts %>%
    left_join(id, by = "fin") %>%
    arrange(millennium.id, code.datetime) %>%
    distinct(millennium.id, .keep_all = TRUE)

# mbo_id <- concat_encounters(data_patients$millennium.id)

mbo_id_split <- concat_encounters(data_patients$millennium.id, 75)

print(mbo_id_split)

# run MBO query
#   * Resident Projects/code_blues/code_data
#
# extract zip files

dirr::gzip_files(dir_raw, pattern = "*.csv", recursive = TRUE)

# 01_demographics --------------------------------------

data_demographics <- read_data2(dir_raw, "demographics", FALSE, TRUE) %>%
    # as.demographics(
    #     extras = c(
    #         "admit.datetime" = "Date and Time - Admit",
    #         "discharge.datetime" = "Date and Time - Discharge"
    #     )
    # ) %>%
    # format_dates(c("admit.datetime", "discharge.datetime")) %>%
    filter(facility != "SE Southeast")

data_include <- data_patients %>%
    semi_join(data_demographics, by = "millennium.id") %>%
    select(millennium.id, pie.id, fin, code.datetime)

# 02_vitals --------------------------------------------

# Clinical Event: Mean Arterial Pressure;Mean Arterial Pressure (Invasive);Systolic Blood Pressure;Arterial Systolic BP 1;Diastolic Blood Pressure;Arterial Diastolic BP 1;Apical Heart Rate;Peripheral Pulse Rate;Respiratory Rate;SpO2 percent

data_vitals <- read_data2(dir_raw, "vitals", FALSE, TRUE) %>%
    # as.vitals() %>%
    inner_join(data_include, by = "millennium.id") %>%
    arrange(millennium.id, event.datetime)

vitals_first <- data_vitals %>%
    group_by(millennium.id, event) %>%
    filter(event.datetime == min(event.datetime)) %>%
    select(millennium.id, event, event.result) %>%
    distinct(millennium.id, event, .keep_all = TRUE) %>%
    spread(event, event.result)

vitals_code <- data_vitals %>%
    group_by(millennium.id) %>%
    filter(
        event.datetime >= code.datetime - hours(6),
        event.datetime <= code.datetime + hours(12)
    )

vitals_minmax <- data_vitals %>%
    group_by(millennium.id, event) %>%
    left_join(
        data_demographics[c("millennium.id", "admit.datetime")],
        by = "millennium.id"
    ) %>%
    filter(event.datetime <= admit.datetime + hours(24)) %>%
    summarize_at("event.result", funs(min, max), na.rm = TRUE)

# 03_temperatures --------------------------------------

# Clinical Event: Temperature Oral;Temperature Tympanic;Temperature Rectal;Temperature Axillary;Temperature Intravascular;Temperature Esophageal;Temperature;Temperature Bladder;Temperature Skin;Temperature Brain;Temperature Sensor

data_temps <- read_data2(dir_raw, "temps", FALSE, TRUE) %>%
    # as.events() %>%
    inner_join(data_include, by = "millennium.id") %>%
    arrange(millennium.id, event.datetime)

temps_first <- data_temps %>%
    distinct(millennium.id, .keep_all = TRUE) %>%
    select(millennium.id, temperature = event.result)

temps_code <- data_temps %>%
    group_by(millennium.id) %>%
    filter(
        event.datetime >= code.datetime - hours(6),
        event.datetime <= code.datetime + hours(12)
    )

temps_minmax <- data_temps %>%
    group_by(millennium.id) %>%
    left_join(
        data_demographics[c("millennium.id", "admit.datetime")],
        by = "millennium.id"
    ) %>%
    filter(event.datetime <= admit.datetime + hours(24)) %>%
    summarize_at("event.result", funs(min, max), na.rm = TRUE)

# 04_labs ----------------------------------------------

# Lab Event: Hct;Hgb;WBC;POC A pH;POC A PO2;Creatinine Lvl;Potassium Lvl;Sodium Lvl

data_labs <- read_data2(dir_raw, "labs", FALSE, TRUE) %>%
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

labs_minmax <- data_labs %>%
    group_by(millennium.id, lab) %>%
    left_join(
        data_demographics[c("millennium.id", "admit.datetime")],
        by = "millennium.id"
    ) %>%
    filter(lab.datetime <= admit.datetime + hours(24)) %>%
    summarize_at("lab.result", funs(min, max), na.rm = TRUE)

# 05_medications ---------------------------------------

data_meds <- read_data2(dir_raw, "meds", FALSE, TRUE) %>%
    # as.meds_inpt() %>%
    inner_join(data_include, by = "millennium.id") %>%
    arrange(millennium.id, med.datetime)

meds_arrhythm <- data_meds %>%
    filter(
        med %in% c("amiodarone", "lidocaine"),
        med.datetime < code.datetime
    ) %>%
    distinct(millennium.id, med) %>%
    mutate(dose = TRUE) %>%
    spread(med, dose) %>%
    rename(
        pre.amiodarone = amiodarone,
        pre.lidocaine = lidocaine
    )

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

data_locations <- read_data2(dir_raw, "locations", FALSE, TRUE) %>%
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

data_measures <- read_data2(dir_raw, "measures", FALSE, TRUE) %>%
    # as.events(order_var = FALSE) %>%
    inner_join(data_include, by = "millennium.id") %>%
    arrange(millennium.id, event.datetime) %>%
    mutate_at("event.result", as.numeric)

weights_first <- data_measures %>%
    filter(event == "weight") %>%
    distinct(millennium.id, .keep_all = TRUE) %>%
    select(millennium.id, weight = event.result)

# 08_gcs -----------------------------------------------

data_gcs <- read_data2(dir_raw, "gcs", FALSE, TRUE) %>%
    # as.events(order_var = FALSE) %>%
    inner_join(data_include, by = "millennium.id") %>%
    arrange(millennium.id, event.datetime)

gcs_first <- data_gcs %>%
    filter(event == "glasgow coma score") %>%
    mutate_at("event.result", as.numeric) %>%
    distinct(millennium.id, .keep_all = TRUE) %>%
    select(millennium.id, gcs = event.result)

gcs_minmax <- data_gcs %>%
    filter(event == "glasgow coma score") %>%
    group_by(millennium.id) %>%
    left_join(
        data_demographics[c("millennium.id", "admit.datetime")],
        by = "millennium.id"
    ) %>%
    filter(event.datetime <= admit.datetime + hours(24)) %>%
    summarize_at("event.result", funs(min, max), na.rm = TRUE)


# 09_hypothermia ---------------------------------------

# data_hypothermia <- read_data2(dir_raw, "hypothermia", FALSE, TRUE)
#     as.order_info()

# 10_readmission ---------------------------------------

edw_person_id <- concat_encounters(data_patients$person.id)
print(edw_person_id)

# run EDW query
#   * Encounters - by Person ID

encounters <- read_data2(dir_raw, "encounters")

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
    mutate(admit.id = if_else(!is.na(admit.datetime), millennium.id, NA_real_)) %>%
    # mutate_at("admit.id", na_if, y = is.na(admit.d))
    fill(admit.datetime, .direction = "down") %>%
    fill(admit.id, .direction = "down") %>%
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
        millennium.id = admit.id,
        readmit.datetime = encounter.datetime,
        readmit.facility = facility,
        readmit.days
    )


# run MBO queries
# Hypothermia protocol, code status

# tidy data sets ---------------------------------------

tidy_patients <- data_demographics %>%
    left_join(data_readmits, by = "millennium.id") %>%
    left_join(weights_first, by = "millennium.id") %>%
    left_join(
        data_patients[c("millennium.id", "fin")],
        by = "millennium.id"
    ) %>%
    select(
        fin,
        age,
        gender,
        race,
        weight,
        length.stay,
        disposition,
        admit.datetime,
        discharge.datetime,
        readmit.facility,
        readmit.days
    ) %>%
    mutate_at("readmit.days", as.numeric)

tidy_admit_vals <- temps_first %>%
    full_join(labs_first, by = "millennium.id") %>%
    full_join(vitals_first, by = "millennium.id") %>%
    full_join(gcs_first, by = "millennium.id") %>%
    full_join(
        data_include[c("millennium.id", "fin")],
        by = "millennium.id"
    ) %>%
    select(fin, everything(), -millennium.id)

tidy_meds <- meds_code %>%
    full_join(meds_electrolytes, by = "millennium.id") %>%
    full_join(meds_arrhythm, by = "millennium.id") %>%
    full_join(
        data_include[c("millennium.id", "fin")],
        by = "millennium.id"
    ) %>%
    select(fin, starts_with("pre."), everything(), -millennium.id) %>%
    mutate_if(is.logical, funs(coalesce(., FALSE)))

tidy_vitals <- vitals_code %>%
    mutate(pre.code = event.datetime < code.datetime) %>%
    semi_join(data_include, by = "millennium.id") %>%
    ungroup() %>%
    select(fin, event.datetime, event, event.result, pre.code) %>%
    distinct(fin, event.datetime, event, .keep_all = TRUE) %>%
    spread(event, event.result)

tidy_temps <- temps_code %>%
    mutate(pre.code = event.datetime < code.datetime) %>%
    semi_join(data_include, by = "millennium.id") %>%
    ungroup() %>%
    select(fin, event.datetime, event, event.result, pre.code)

tidy_labs <- labs_code %>%
    mutate(pre.code = lab.datetime < code.datetime) %>%
    semi_join(data_include, by = "millennium.id") %>%
    ungroup() %>%
    select(fin, lab.datetime, lab, lab.result, pre.code) %>%
    distinct(fin, lab.datetime, lab, .keep_all = TRUE) %>%
    spread(lab, lab.result)

tidy_dataset <- list(
    "patients" = tidy_patients,
    "admit_values" = tidy_admit_vals,
    "meds" = tidy_meds,
    "vitals" = tidy_vitals,
    "temps" = tidy_temps,
    "labs" = tidy_labs
)

write.xlsx(tidy_dataset, "data/external/fy19/patient_data_second.xlsx")

# admission min / max values ---------------------------

tidy_minmax_vitals <- vitals_minmax %>%
    ungroup() %>%
    full_join(
        data_include[c("millennium.id", "fin")],
        by = "millennium.id"
    ) %>%
    select(fin, everything(), -millennium.id)

tidy_minmax_temps <- temps_minmax %>%
    ungroup() %>%
    full_join(
        data_include[c("millennium.id", "fin")],
        by = "millennium.id"
    ) %>%
    select(fin, everything(), -millennium.id)

tidy_minmax_labs <- labs_minmax %>%
    ungroup() %>%
    full_join(
        data_include[c("millennium.id", "fin")],
        by = "millennium.id"
    ) %>%
    select(fin, everything(), -millennium.id)

tidy_minmax_gcs <- gcs_minmax %>%
    ungroup() %>%
    full_join(
        data_include[c("millennium.id", "fin")],
        by = "millennium.id"
    ) %>%
    select(fin, everything(), -millennium.id)

tidy_minmax_dataset <- list(
    "vitals" = tidy_minmax_vitals,
    "temps" = tidy_minmax_temps,
    "labs" = tidy_minmax_labs,
    "gcs" = tidy_minmax_gcs
)

write.xlsx(tidy_minmax_dataset, "data/external/fy19/admit_min-max_second.xlsx")

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
