# CREATE BINARYS FUNCTION
make_binary <- function(df) {
    df %>%
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
        group_by(studlab, outcome) %>%
        mutate(
            bin_fu_longest = case_when(
                follow_up == max(follow_up) ~ 1,
                TRUE ~ 0
            ),
            bin_fu_period = case_when(
                between(follow_up, 0, 11.999) ~ 1,
                between(follow_up, 12, 23.999) ~ 2,
                follow_up >= 24 ~ 3
            )
        ) %>%
        group_by(studlab, outcome, bin_fu_period) %>%
        mutate(bin_fu_period_long = case_when(
            follow_up == max(follow_up) ~ 1,
            TRUE ~ 0
        )) %>%
        ungroup() %>%
        mutate(
            bin_outcome = as.numeric(fct_relevel(outcome, "cs")), # making sure CS is no 1
            across(starts_with("bin_"), as.integer)
        ) %>%
        return()
}

### CREATE SELECTION GRID
make_sel_grid <- function(df, outcome_type = NULL) {
    testthat::expect_true(outcome_type %in% c("qol", "func"))

    if (outcome_type == "qol") {
        outcome_vals <- c(unique(df$bin_outcome), 98) # 98: QoLs
    } else {
        outcome_vals <- c(unique(df$bin_outcome), 98:99) # 98: PROMS + CS,99 PROMS
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
        outcome = outcome_vals,
        follow_up = c(unique(df$follow_up), 98:99),
        fu_period = unique(df$bin_fu_period)
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
                      outcome,
                      follow_up,
                      fu_period) {
    # Simple selection vars
    sel_lang <- 1:language
    sel_year <- 1:year
    sel_design <- 1:design
    sel_age <- 1:age
    sel_34part <- 1:neer_34part
    sel_toi <- 1:toi
    sel_doctreat <- 1:doctreat
    sel_loss_fu <- 1:loss_fu

    # outcomes
    if (!outcome %in% c(98, 99)) { # select specific outcomes
        sel_outcome <- outcome
    }
    else if (outcome == 98) { # select all outcomes
        sel_outcome <- unique(df$bin_outcome)
    }
    else if (outcome == 99) {
        unq <- unique(df$bin_outcome)
        sel_outcome <- unq[!unq %in% 1] # remove CS from proms
    }

    # follow-up

    if (!follow_up %in% c(98, 99)) { # selects identical FUs
        sel_follow_up <- follow_up
        sel_fu_longest <- c(0, 1) # select all
        sel_fu_period <- unique(df$bin_fu_period) # select all
        sel_fu_period_long <- c(0, 1) # select all
    } else if (follow_up == 98) { # longest fu from each study
        sel_follow_up <- unique(df$follow_up) # select all fus
        sel_fu_longest <- 1 # select only longest
        sel_fu_period <- unique(df$bin_fu_period) # select all
        sel_fu_period_long <- c(0, 1) # select all
    } else if (follow_up == 99) {
        sel_follow_up <- unique(df$follow_up) # select all fus
        sel_fu_longest <- c(0, 1) # select only longest
        sel_fu_period <- fu_period # select specific period
        sel_fu_period_long <- 1 # select longest fu in each period
    }


    # do subset
    # convert to data.table  using dtplyr when done

    subset <- df %>%
        filter(
            bin_lang %in% sel_lang,
            bin_year %in% sel_year,
            bin_design %in% sel_design,
            bin_age %in% sel_age,
            bin_34part %in% sel_34part,
            bin_toi %in% sel_toi,
            bin_doctreat %in% sel_doctreat,
            bin_loss_fu %in% sel_loss_fu,
            bin_outcome %in% sel_outcome,
            follow_up %in% sel_follow_up,
            bin_fu_longest %in% sel_fu_longest,
            bin_fu_period %in% sel_fu_period,
            bin_fu_period_long %in% sel_fu_period_long
        )
    if (nrow(subset) == 0) {
        return(NULL)
    }
    else {
        return(subset)
    }
}

## TESTING

# data split

data_qol <- data_cont %>%
    filter(outcome %in% qol_outcomes) %>%
    make_binary() %>%
    select(studlab, outcome, follow_up, smd, se, starts_with("bin_"))
data_func <- data_cont %>%
    filter(outcome %in% c("cs", prom_outcomes)) %>%
    make_binary() %>%
    select(studlab, outcome, follow_up, smd, se, starts_with("bin_"))

# make sel grid

sel_grid_qol <- data_qol %>%
    make_sel_grid(outcome_type = "qol")

sel_grid_func <- data_func %>%
    make_sel_grid(outcome_type = "func")

sel_grid_qol %>%
    slice_head(n = 50) %>%
    View()
sel_grid_func %>%
    slice_head(n = 1000) %>%
    View()

# do subsets
subsets_qol <- sel_grid_qol %>%
    slice_head(n = 5000) %>%
    pmap(.,
        do_subset,
        df = data_qol
    ) %>%
    set_names(1:length(.)) %>%
    discard(~ is.null(.x))

subsets_func <- sel_grid_func %>%
    slice_head(n = 5000) %>%
    pmap(.,
        do_subset,
        df = data_func
    ) %>%
    set_names(seq_along(.)) %>%
    discard(~ is.null(.x))

subsets_func[100:200]

# splitting dfs afterwards

multi_out_qol <- subsets_qol %>%
    keep(~ any(duplicated(.x[["studlab"]])))

multi_out_func <- subsets_func %>%
    keep(~ any(duplicated(.x[["studlab"]])))

multi_out_func %>%
    enframe() %>%
    mutate(
        n_dupls = map_dbl(value, ~ sum(duplicated(.x[["studlab"]]))),
        n_rows = map_dbl(value, ~ nrow(.x))
    ) %>%
    filter(n_rows > 2, n_dupls < 8)

df <- multi_out_func[["3928"]]

split_dfs <- df %>%
    group_by(studlab) %>%
    filter(n() > 1) %>%
    group_by(outcome) %>%
    group_split()


split_multi_outc <- function(df) {
    split_dfs <- df %>%
        group_by(studlab) %>%
        filter(n() > 1) %>%
        group_by(outcome) %>%
        group_split()
    unq_df <- df %>%
        group_by(studlab) %>%
        filter(n() <= 1)


    ### DET ER DEN HER DER SLOWER! JEG BEHØVER IKKE ALLE PERMUTATIONS. JEG BEHØVER KUN AT HVER ENKELT ER FØRST! gør jeg ik?
    perms <- t(gtools::permutations(n = length(split_dfs), r = length(split_dfs)))

    loop_list <- list()
    temp_df <- tibble()
    for (i in seq_along(perms)) {
        if (i %% length(split_dfs) == 1) {
            temp_df <- split_dfs[[perms[[i]]]]
        }
        if (i %% length(split_dfs) != 1) {
            temp_df <- bind_rows(
                temp_df,
                split_dfs[[perms[[i]]]] %>% filter(!studlab %in% temp_df[["studlab"]])
            )
        }
        if (i %% length(split_dfs) == 0) {
            temp_df <- bind_rows(temp_df, unq_df)
            loop_list <- c(loop_list, list(temp_df))
            temp_df <- tibble()
        }
    }
    return(loop_list)
}
# multisession
plan(multicore, workers = 10)
tictoc::tic()
multi_outcome <- multi_out_func %>% future_map(split_multi_outc)
tictoc::toc()

###### ALTERNATIV FUNKTIONER TIL SPLIT OUTCOMES JEG IKKE HAR TESTET HELT FÆRDIG
unqs <- x %>%
    group_by(studlab) %>%
    filter(n() < 2)
dupls <- x %>%
    group_by(studlab) %>%
    filter(n() > 1)

rows <- dupls %>%
    group_by(outcome) %>%
    group_split()
rows

row_combs <- t(gtools::permutations(n = length(rows), r = length(rows))) %>% as_tibble()

nested_dfs <- row_combs %>%
    mutate(across(everything(), ~ map(., ~ pluck(rows, .x))))


sbu_func <- function(acc_df, new_df) {
    studlabs <- acc_df %>% pull(studlab)

    bind_rows(acc_df, new_df %>% filter(!studlab %in% studlabs))
}

tictoc::tic()
test <- nested_dfs %>% future_map(~ reduce(.x, sbu_func))
tictoc::toc()


# FROM STACKOVERLO

row_combinations <- t(combn(x = 1:nrow(x), m = k)) %>% as_tibble()
row_combinations
row_combinations %>%
    mutate(across(everything(), ~ map(., ~ pluck(rows, .x))))







all_equal(nested_dfs[["V1"]], nested_dfs[["V2"]])


df <- multi_out_func[["3928"]]
df <- multi_out_func[["4499"]]
df

### START FOR I IN LIST OF DUPL LISTS
new_split_mout <- function(df) {
    split_dfs <- df %>%
        group_by(studlab) %>%
        filter(n() > 1) %>%
        group_by(outcome) %>%
        group_split()
    unq_df <- df %>%
        group_by(studlab) %>%
        filter(n() <= 1)

    idxs <- seq_along(split_dfs)
    loop_list <- list()
    for (df_idx in idxs) {
        if (df_idx %in% c(1, idxs[length(idxs)])) {
            js <- setdiff(idxs, df_idx)
        }
        else {
            js <- c(idxs[(df_idx + 1):length(idxs)], idxs[1:(df_idx - 1)])
        }
        temp_df <- split_dfs[[df_idx]]
        for (j in js)
        {
            temp_df <- bind_rows(
                temp_df,
                split_dfs[[j]] %>% filter(!studlab %in% temp_df[["studlab"]])
            )
        }
        temp_df <- bind_rows(temp_df, unq_df)
        loop_list <- c(loop_list, list(temp_df))
    }
    return(loop_list)
}


plan(multicore, workers = 10)
tictoc::tic()
multi_outcome <- multi_out_func %>% future_map(new_split_mout)
tictoc::toc()






## profiling
profvis(multi_out_func[1:50] %>% map(split_multi_outc))