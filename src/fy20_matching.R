library(tidyverse)
library(readxl)
library(edwr)
library(lubridate)
library(MatchIt)
library(openxlsx)

pts <- read_excel(
    "U:/Data/code_blues/external/fy20/patients.xlsx",
    col_names = c("id", "fin"),
    skip = 1
)

mbo_fin <- concat_encounters(pts$fin)
print(mbo_fin)

# matching ---------------------------------------------

df <- read_excel("U:/Data/code_blues/external/fy20/data_ada_matching.xlsx") %>%
    select(
        id = `Patient number`,
        fin = FIN,
        cci = CCI,
        code_start_date = `Date compressions started`,
        code_start_time = `...54`,
        ca_given = Ca,
        code_end_date = `Code end date`,
        code_end_time = `...128`,
        duration_xl = `Duration of Code`
    ) %>%
    mutate_at("ca_given", ~(. == 1)) %>%
    mutate_at(
        "code_end_date",
        ~case_when(
            id == 215 ~ ymd("2017-10-19", tz = "UTC"),
            id == 124 & code_start_date == ymd("2018-01-13", tz = "UTC") ~ ymd("2018-01-13", tz = "UTC"),
            id == 80 ~ ymd("2018-12-30", tz = "UTC"),
            id == 105 ~ ymd("2017-09-30", tz = "UTC"),
            id == 92 ~ ymd("2019-02-08", tz = "UTC"),
            id == 18 ~ ymd("2019-04-07", tz = "UTC"),
            id == 118 ~ ymd("2017-09-09", tz = "UTC"),
            id == 112 ~ ymd("2017-11-05", tz = "UTC"),
            TRUE ~ .
        )
    ) %>%
    mutate(
        code_start = make_datetime(
            year(code_start_date),
            month = month(code_start_date),
            day = day(code_start_date),
            hour = hour(code_start_time),
            min = minute(code_start_time),
            sec = second(code_start_time)
        ),
        code_end = make_datetime(
            year(code_end_date),
            month = month(code_end_date),
            day = day(code_end_date),
            hour = hour(code_end_time),
            min = minute(code_end_time),
            sec = second(code_end_time)
        ),
        code_duration = difftime(code_end, code_start, units = "mins")
    ) %>%
    select(fin, ca_given, cci, code_duration) %>%
    column_to_rownames("fin")

set.seed(77123)
m_data <- matchit(
    ca_given ~ cci + code_duration,
    data = df,
    discard = "both"
)

data_matched <- match.data(m_data) %>%
    rownames_to_column("fin")

df_matches <- m_data$match.matrix %>%
    as_tibble(rownames = "ca_fins") %>%
    rename(no_ca_fins = `1`)

summary(m_data)

write.xlsx(
    data_matched,
    "U:/Data/code_blues/external/fy20/matched_patients.xlsx"
)

write.xlsx(
    df_matches,
    "U:/Data/code_blues/external/fy20/group_matches.xlsx"
)
