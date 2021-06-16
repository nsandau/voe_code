
# MAKE BINARYS ----------------------------------------

make_binary <- function(df, outcome) {
    df_bin <- df %>%
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
            ),
            bin_oa = if_else(method_of_outcome_analysis == "intention-to-treat", 1, 2),
            bin_neer = case_when(
                all(total_neer_2_part > c(total_neer_1_part, total_neer_3_part, total_neer_4_part)) ~ 2,
                all(total_neer_3_part > c(total_neer_1_part, total_neer_2_part, total_neer_4_part)) ~ 3,
                all(total_neer_4_part > c(total_neer_1_part, total_neer_2_part, total_neer_3_part)) ~ 4,
                TRUE ~ 0
            ),
            bin_imputed = case_when(
                imputed_outcome == "no" ~ 1,
                str_detect(imputed_outcome, "yes|wrong") & !str_detect(imputed_vars, as.character(outcome)) ~ 1,
                imputed_outcome == "yes" & str_detect(imputed_vars, as.character(outcome)) ~ 2,
                imputed_outcome == "wrong" & str_detect(imputed_vars, as.character(outcome)) ~ 3
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
            ),
            bin_ttfu = case_when(
                max(follow_up) >= 12 ~ 1,
                between(max(follow_up), 6, 11.999) ~ 2,
                TRUE ~ 3
            )
        ) %>%
        group_by(studlab, outcome, bin_fu_period) %>%
        mutate(
            bin_fu_period_long = case_when(
                follow_up == max(follow_up) ~ 1,
                TRUE ~ 0
            )
        ) %>%
        ungroup() %>%
        mutate(
            bin_outcome = outcome,
            across(starts_with("bin_"), as.integer)
        )

    if (outcome == "func") {
        df_bin <- df_bin %>% mutate(
            bin_outcome = as.integer(fct_relevel(outcome, "cs")) # making sure CS is no 1
        )
    }

    return(df_bin)
}

#### Function to remove null_combinations in grid ----------------------------------------


remove_nulls <- function(df, grid, list_of_combos) {
    df <- df %>%
        select(follow_up, bin_outcome, interv) %>%
        rename(
            outcome = bin_outcome,
            intervention = interv
        )


    for (i in seq_along(list_of_combos)) {
        var1 <- list_of_combos[[i]][1]
        var2 <- list_of_combos[[i]][2]
        var1_vals <- unique(df[[var1]])
        var2_vals <- unique(df[[var2]])

        for (val1 in var1_vals) {
            for (val2 in var2_vals) {
                n_rows <- df[get(var1) == val1 & get(var2) == val2] %>%
                    nrow()
                if (n_rows == 0) {
                    grid <- grid[!(get(var1) == val1 & get(var2) == val2)]
                }
            }
        }
    }
    return(grid)
}


### CREATE SELECTION GRID ------------------------------------------------------------

make_sel_grid <- function(df, outcome_type = NULL) {
    if (outcome_type == "qol") {
        outcome_vals <- c(unique(df$bin_outcome), 98) # 98: QoLs
    } else if (outcome_type == "func") {
        outcome_vals <- c(unique(df$bin_outcome), 98:99) # 98: PROMS + CS,99 PROMS
    } else if (outcome_type == "bin") {
        outcome_vals <- unique(df$bin_outcome)
    }

    # neer
    neer_vals <- unique(df$bin_neer)
    neer_vals <- neer_vals[neer_vals %in% 2:4]

    # outcome
    interv_vals <- as.factor(c(levels(fct_drop(df$interv)), "plate_tb", "artro", "all"))

    # create grid
    grid <- expand_grid(
        language = unique(df$bin_lang),
        year = unique(df$bin_year),
        design = unique(df$bin_design),
        age = unique(df$bin_age),
        neer_34part = unique(df$bin_34part),
        toi = unique(df$bin_toi),
        doctreat = unique(df$bin_doctreat),
        loss_fu = unique(df$bin_loss_fu),
        outcome = outcome_vals,
        follow_up = c(unique(df$follow_up), 98:99), # 98: longest fu from each study, 99: periods of fu
        fu_period = unique(df$bin_fu_period),
        outcome_analysis = c(unique(df$bin_oa), 98), # 98: both types of oa
        intervention = interv_vals,
        neer = c(neer_vals, 98, 99), # 98: 3-part + 4-part, 99 ALL
        imputed = unique(df$bin_imputed),
        ttfu = unique(df$bin_ttfu)
    ) %>%
        mutate(
            across(where(is.numeric), as.integer),
            across(where(is.character), as_factor)
        ) %>%
        as.data.table()

    nrow_before <- nrow(grid)
    cat("Rows before removal of nulls:", nrow_before, "\n")

    # remove null combinations
    combs <- list(
        c("follow_up", "outcome"),
        c("intervention", "outcome"),
        c("intervention", "follow_up")
    )
    grid <- remove_nulls(df, grid, combs)
    nrow_after <- nrow(grid)
    cat("Rows after removal of nulls:", nrow_after, "\n")
    nrow_diff <- nrow_before - nrow_after
    cat("Rows removed:", nrow_diff, round(nrow_diff / nrow_before) * 100, "%", "\n")
    return(grid)
}

### SUBSET FUNCTION --------------------

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
                      fu_period,
                      outcome_analysis,
                      intervention,
                      neer,
                      imputed,
                      ttfu) {
    # Simple selection vars
    sel_lang <- 1:language
    sel_year <- 1:year
    sel_design <- 1:design
    sel_age <- 1:age
    sel_34part <- 1:neer_34part
    sel_toi <- 1:toi
    sel_doctreat <- 1:doctreat
    sel_loss_fu <- 1:loss_fu
    sel_imputed <- 1:imputed
    sel_ttfu <- 1:ttfu

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

    # outcome analysis
    if (outcome_analysis == 98) {
        sel_oa <- unique(df$bin_oa)
    }
    else {
        sel_oa <- outcome_analysis
    }

    # intervention
    if (intervention == "plate_tb") {
        sel_interv <- c("tension-band", "plate")
    } else if (intervention == "artro") {
        sel_interv <- c("rsa", "ha")
    } else if (intervention == "all") {
        sel_interv <- levels(df$interv)
    } else {
        sel_interv <- intervention
    }

    # neer
    if (!neer %in% c(98, 99)) { # select specific neer type
        sel_neer <- neer
    }
    else if (neer == 98) { # select 3 and 4 parts
        sel_neer <- c(3, 4)
    }
    else if (neer == 99) { # select all neer types
        sel_neer <- unique(df$bin_neer)
    }

    # do subset
    subset <- df[
        bin_lang %in% sel_lang &
            bin_year %in% sel_year &
            bin_design %in% sel_design &
            bin_age %in% sel_age &
            bin_34part %in% sel_34part &
            bin_toi %in% sel_toi &
            bin_doctreat %in% sel_doctreat &
            bin_loss_fu %in% sel_loss_fu &
            bin_outcome %in% sel_outcome &
            follow_up %in% sel_follow_up &
            bin_fu_longest %in% sel_fu_longest &
            bin_fu_period %in% sel_fu_period &
            bin_fu_period_long %in% sel_fu_period_long &
            bin_oa %in% sel_oa &
            interv %in% sel_interv &
            bin_neer %in% sel_neer &
            bin_imputed %in% sel_imputed &
            bin_ttfu %in% sel_ttfu
    ]

    if (nrow(subset) == 0) {
        return(NULL)
    }
    else {
        return(as_tibble(subset))
    }
}

# split multi outcome -----------------------------------------


split_multi_outc <- function(split_dfs) {
    loop_list <- list()
    for (df_idx in seq_along(split_dfs)) {
        loop_df <- split_dfs[[df_idx]]
        sub_idx <- df_idx + 1
        while (sub_idx <= length(split_dfs)) {
            loop_df <- bind_rows(
                loop_df,
                split_dfs[[sub_idx]] %>%
                    filter(!studlab %in% loop_df[["studlab"]])
            )
            sub_idx <- sub_idx + 1
        }

        sub_idx <- 1
        while (sub_idx < df_idx) {
            loop_df <- bind_rows(loop_df, split_dfs[[sub_idx]] %>%
                filter(!studlab %in% loop_df[["studlab"]]))
            sub_idx <- sub_idx + 1
        }
        loop_list <- c(loop_list, list(loop_df))
    }
    return(loop_list)
}


# do metagen --------------------------------------------------------------

do_meta <- function(data, outcome) {
    if (outcome %in% c("qol", "func")) {
        metagen(
            TE = smd,
            seTE = se,
            hakn = FALSE,
            method.tau = "DL",
            studlab = studlab,
            data = data
        )
    }
    else if (outcome == "bin") {
        metabin(
            event.e = int_e,
            n.e = int_n,
            event.c = con_e,
            n.c = con_n,
            studlab = studlab,
            data = data,
            sm = "RR",
            method = "MH",
            MH.exact = FALSE,
            comb.fixed = FALSE,
            comb.random = TRUE,
            method.tau = "PM",
            hakn = TRUE
        )
    }
}

# extract results metagen -------------------------------------------------

extract_meta <- function(data, outcome) {
    res <- data %>%
        map_dbl("TE.fixed") %>%
        enframe(name = "iteration", "te.fixed") %>%
        mutate(
            sete.fixed = map_dbl(data, "seTE.fixed"),
            lower.fixed = map_dbl(data, "lower.fixed"),
            upper.fixed = map_dbl(data, "upper.fixed"),
            pval.fixed = map_dbl(data, "pval.fixed"),
            te.random = map_dbl(data, "TE.random"),
            sete.random = map_dbl(data, "seTE.random"),
            lower.random = map_dbl(data, "lower.random"),
            upper.random = map_dbl(data, "upper.random"),
            pval.random = map_dbl(data, "pval.random"),
            i2 = map_dbl(data, "I2"),
            q = map_dbl(data, "Q"),
            pval.q = map_dbl(data, "pval.Q"),
            k = map_dbl(data, "k"),
            studlab = map_chr(data, ~ str_flatten(.x$studlab, collapse = " "))
        )
    if (outcome == "bin") {
        res <- res %>%
            filter(k != 0) %>%
            mutate(
                across(starts_with(c("te.", "sete.", "lower.", "upper.")), exp)
            )
    }

    return(res)
}

# gather pvals ------------------------------------------------------------

gather_pvals <- function(data) {
    data %>%
        transmute(
            pval = pval.fixed,
            estimate = te.fixed,
            k = k,
            method = "fixed"
        ) %>%
        bind_rows(
            .,
            data %>%
                transmute(
                    pval = pval.random,
                    estimate = te.random,
                    k = k,
                    method = "random"
                )
        )
}


if (sys.nframe() == 0) { # if __name__ == __main__

    ## TESTING

    # data split

    sel_grid_qol %>%
        slice_head(n = 50) %>%
        View()
    sel_grid_func %>%
        slice_head(n = 1000) %>%
        View()

    # do subsets
    subsets_qol <- sel_grid_qol %>%
        slice_head(n = 10000) %>%
        future_pmap(
            .l = .,
            .f = do_subset,
            df = data_qol
        ) %>%
        set_names(seq_along(.)) %>%
        discard(~ is.null(.x))


    subsets_func <- sel_grid_func %>%
        slice_head(n = 50000) %>%
        future_pmap(
            .l = .,
            .f = do_subset,
            df = data_func
        ) %>%
        set_names(seq_along(.)) # %>%
    discard(~ is.null(.x))

    # splitting dfs with multi outcome

    multi_out_qol <- subsets_qol %>%
        keep(~ any(duplicated(.x[["studlab"]])))

    multi_out_func <- subsets_func %>%
        keep(~ any(duplicated(.x[["studlab"]])))

    tic()
    multi_out_qol_splits <- multi_out_qol %>% future_map(split_multi_outc)
    tictoc::toc()

    subsets_qol_final <- c(
        subsets_qol %>%
            discard(~ any(duplicated(.x[["studlab"]]))),
        multi_out_qol_splits %>% flatten()
    )



    ## profiling
    profvis(multi_out_func[1:50] %>% map(split_multi_outc))


    profvis::profvis(sel_grid_qol %>%
        slice_head(n = 50000) %>%
        pmap(
            .l = .,
            .f = do_subset,
            df = data_qol
        ) %>%
        set_names(seq_along(.)) %>%
        discard(~ is.null(.x)))
}