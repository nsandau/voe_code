# TROR MIN LØSNING MED FACTOR GIVER MIG SLOWDOWNS.
# HVIS FOR MEGET NÅR JEG BRUGER DT MÅ JEG KONVERTERE TIL NUMERISK




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
        bin_doctreat = if_else(documentation_for_treatment == "yes", 1, 2),
        bin_loss_fu = case_when(
            (total_loss_to_fu / total_n) < 0.15 ~ 1,
            TRUE ~ 2
        )
    ) %>%
    ungroup() %>%
    mutate(
        bin_outcome = as.numeric(fct_relevel(outcome, "cs")), # making sure CS is no 1
        across(where(is.numeric), as.integer)
    )

### CREATE SELECTION GRID
make_sel_grid <- function(df, outcome_type = NULL) {
    testthat::expect_true(outcome_type %in% c("qol", "func"))

    if (outcome_type == "qol") {
        outcome_vals <- c(unique(df$bin_outcome), 51) # 51: QoLs
    } else {
        outcome_vals <- c(unique(df$bin_outcome), 51:52) # 51: PROMS + CS,52 PROMS
    }

    expand_grid(
        language = unique(df$bin_lang),
        year = unique(df$bin_year),
        design = unique(df$bin_design),
        age = unique(df$bin_age),
        neer_34part = unique(df$bin_34part),
        toi = unique(df$bin_toi),
        doctreat = unique(df$bin_doctreat),
        loss_fu = unique(df$bin_loss_fu),
        outcome = outcome_vals
    ) %>%
        mutate(
            across(where(is.numeric), as.integer),
            across(where(is.character), as_factor)
        ) %>%
        return()
}



### SUBSET FUNCTION

do_subset <- function(df,
                      language,
                      year,
                      design,
                      age,
                      neer_34part,
                      toi,
                      doctreat,
                      loss_fu,
                      outcome) {
    # Simple selection vars
    sel_lang <- 1:language
    sel_year <- 1:year
    sel_design <- 1:design
    sel_age <- 1:age
    sel_34part <- 1:neer_34part
    sel_toi <- 1:toi
    sel_doctreat <- 1:doctreat
    sel_loss_fu <- 1:loss_fu

    # outcome selection
    if (!outcome %in% c(51, 52)) {
        sel_outcome <- outcome
    }
    else if (outcome == 51) {
        sel_outcome <- unique(df$bin_outcome)
    }
    else if (outcome == 52) {
        unq <- unique(df$bin_outcome)
        sel_outcome <- unq[!unq %in% 1]
    }

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
            bin_doctreat %in% sel_doctreat,
            bin_loss_fu %in% sel_loss_fu,
            bin_outcome %in% sel_outcome
        ) %>%
        return()
}



## TESTING

# data split

data_qol <- data_bin %>%
    filter(outcome %in% qol_outcomes) %>%
    select(studlab, follow_up, smd, se, starts_with("bin_"))
data_func <- data_bin %>%
    filter(outcome %in% c("cs", prom_outcomes)) %>%
    select(studlab, follow_up, smd, se, starts_with("bin_"))

# make sel grid

sel_grid_qol <- data_qol %>%
    make_sel_grid(outcome_type = "qol")

sel_grid_func <- data_func %>%
    make_sel_grid(outcome_type = "func")

sel_grid_qol %>%
    slice_head(n = 1000) %>%
    View()
sel_grid_func %>%
    slice_head(n = 1000) %>%
    View()


# do subsets
subsets_qol <- sel_grid_qol %>%
    slice_head(n = 1000) %>%
    pmap(.,
        do_subset,
        df = data_qol
    )

subsets_qol[1:50]

subsets_func <- sel_grid_func %>%
    slice_head(n = 1000) %>%
    pmap(.,
        do_subset,
        df = data_func
    )

subsets_func[1:50]




profvis(sel_grid_qol %>% pmap(.,
    do_subset,
    df = subset_data
))


subsets %>% map(~ distinct(.x))

subsets[[1]] %>% distinct()