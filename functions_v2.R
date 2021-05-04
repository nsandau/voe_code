conflict_prefer("year", "lubridate")

# CREATE BINARYS FUNCTION
data_bin <- data_cont %>%
    ### OBS REMOVE NÅR JEG LAVER TING DER IKKE ER PÅ STUDIEPLAN!
    group_by(studlab) %>%
    mutate(
        bin_lang = if_else(language == "english", 1, 2),
        bin_year = case_when(
            year(publ_date) > 1995 ~ 1,
            year(publ_date) > 1990 ~ 2,
            TRUE ~ 3
        ),
        bin_design = case_when(
            design == "rct" ~ 1,
            design == "prospective nrsi" ~ 2,
            design == "retrospective nrsi" ~ 3
        ),
        bin_age = case_when(
            total_avg_age >= 65 ~ 1,
            between(total_avg_age, 60, 64.999) ~ 2,
            between(total_avg_age, 55, 59.999) ~ 3,
            between(total_avg_age, 50, 54.999) ~ 4,
            between(total_avg_age, 18, 49.999) ~ 5,
            between(total_avg_age, 16, 17.999) ~ 6,
            total_avg_age < 16 ~ 7
        ),
        bin_34part = case_when(
            total_neer_1_part == 0 & total_neer_2_part == 0 ~ 1,
            TRUE ~ 2
        ),
        bin_toi = case_when(
            between(time_of_intervention, 0, 2) ~ 1,
            between(time_of_intervention, 2.0001, 14) ~ 2,
            between(time_of_intervention, 14.0001, 21) ~ 3,
            TRUE ~ 4
        ),
        bin_doctreat = if_else(documentation_for_treatment == "yes", 1, 2)
    ) %>%
    mutate(across(where(is.numeric), as.integer))

### CREATE SELECTION GRID

sel_grid <- expand_grid(
    language = unique(data_bin$bin_lang),
    year = unique(data_bin$bin_year),
    design = unique(data_bin$bin_design),
    age = unique(data_bin$bin_age),
    neer_34part = unique(data_bin$bin_34part),
    toi = unique(data_bin$bin_toi),
    doctreat = unique(data_bin$bin_doctreat)
) %>%
    mutate(across(where(is.numeric), as.integer))
sel_grid

### SUBSET FUNCTION

do_subset <- function(df,
                      language,
                      year,
                      design,
                      age,
                      neer_34part,
                      toi,
                      doctreat) {
    # assign selection vars
    sel_lang <- 1:language
    sel_year <- 1:year
    sel_design <- 1:design
    sel_age <- 1:age
    sel_34part <- 1:neer_34part
    sel_toi <- 1:toi
    sel_doctreat <- 1:doctreat

    # do subset
    # convert to data.table when finished using dtplyr

    df %>%
        filter(
            bin_lang %in% sel_lang,
            bin_year %in% sel_year,
            bin_design %in% sel_design,
            bin_age %in% sel_age,
            bin_34part %in% sel_34part,
            bin_toi %in% sel_toi,
            bin_doctreat %in% sel_doctreat
        ) %>%
        return()
}

subsets <- sel_grid %>% pmap(.,
    do_subset,
    df = data_bin %>% select(starts_with("bin_"))
)

subsets %>% map(~ distinct(.x))

subsets[[1]] %>% distinct()