
# MAKE BINARYS ----------------------------------------

make_binary <- function(df, outcome, protocol) {
    df_bin <- df %>%
        group_by(studlab) %>%
        mutate(
            bin_lang = case_when(
                language == "english" ~ 1,
                TRUE ~ 2
            ),
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
            ),
            rob_evals = case_when(
                rob_tool == "rob2" & rob2_overall == "some concern" ~ "moderate",
                rob_tool == "rob2" & rob2_overall != "some concern" ~ rob2_overall,
                rob_tool == "rob" & rob_overall == "unclear" ~ "moderate",
                rob_tool == "rob" & rob_overall != "unclear" ~ rob_overall,
                rob_tool == "minors" & minors_total <= 8 ~ "high",
                rob_tool == "minors" & between(minors_total, 9, 15) ~ "moderate",
                rob_tool == "minors" & minors_total >= 16 ~ "low",
                rob_tool == "nos" & nos_total <= 2 ~ "high",
                rob_tool == "nos" & between(nos_total, 3, 4) ~ "moderate",
                rob_tool == "nos" & nos_total >= 5 ~ "low",
                TRUE ~ "high" # One study has NA. Setting ROB to high ensures it gets included "last"
            ),
            bin_rob = factor(rob_evals, ordered = TRUE, levels = c("low", "moderate", "high"))
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
        mutate(
            bin_fu_period_long = case_when(
                follow_up == max(follow_up) ~ 1,
                TRUE ~ 0
            )
        ) %>%
        group_by(studlab, outcome, follow_up) %>%
        mutate(
            bin_ttfu = case_when(
                follow_up >= 12 ~ 1,
                between(follow_up, 6, 11.999) ~ 2,
                TRUE ~ 3
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

    ### RECODE TO PROTOCOLS
    if (protocol == "beks") {
        df_bin <- df_bin %>%
            mutate(
                bin_lang = case_when( # english, german or dutch
                    language %in% c("english", "german", "dutch") ~ 1
                ),
                bin_year = case_when( # studies after 2007, all studies
                    year(publ_date) > 2007 ~ 1,
                    TRUE ~ 2
                ),
                bin_design = case_when( # rct, rct + any NRSI
                    design == "rct" ~ 1,
                    TRUE ~ 2
                )
            ) %>%
            group_by(studlab, outcome) %>%
            mutate(
                bin_fu_period = case_when(
                    between(follow_up, 0, 12) ~ 1,
                    TRUE ~ 2
                ),
            ) %>%
            group_by(studlab, outcome, bin_fu_period) %>%
            mutate(
                bin_fu_period_long = case_when(
                    follow_up == max(follow_up) ~ 1,
                    TRUE ~ 0
                )
            ) %>%
            ungroup() %>%
            filter(interv != "k-wires") # excludes exfix in protocol
    }

    if (protocol == "skou") {
        df_bin <- df_bin %>%
            mutate(
                bin_year = case_when(
                    year(publ_date) > 2000 ~ 1,
                    TRUE ~ 2
                ), ,
                bin_design = case_when( # rct, rct + any NRSI
                    design == "rct" ~ 1,
                    TRUE ~ 2
                )
            ) %>%
            group_by(studlab, outcome) %>%
            mutate(
                fu_diff = abs(follow_up - 12),
                bin_fu_longest = case_when(
                    fu_diff == min(fu_diff) ~ 1,
                    TRUE ~ 0
                )
            ) %>%
            ungroup()
    }


    return(df_bin)
}


### CREATE SELECTION GRID ------------------------------------------------------------

make_sel_grid <- function(df, outcome_type = NULL, protocol = NULL) {
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

    # create grid_values
    grid_vals <- list(
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
        ttfu = unique(df$bin_ttfu),
        rob = unique(df$bin_rob)
    )
    # TODO: REMOVE
    ##### Drop doctreat when outcome = "func"
    if (outcome_type == "func") {
        grid_vals[["doctreat"]] <- 2
    } # any regardless of doctreat

    ## RECODE GRID_VALS TO PROTOCOL VALUES

    if (protocol == "handoll") {
        grid_vals[["design"]] <- 1 # only rct
        grid_vals[["age"]] <- 5 # > 18 years
    }

    if (protocol == "beks") {
        grid_vals[["language"]] <- 1 # english dutch or german
        grid_vals[["age"]] <- 5 # > 18 years
        grid_vals[["follow_up"]] <- 99 # only use bin_periods (<12 >12 months)
        grid_vals[["rob"]] <- c(1, 3) # only low risk of bias, or all
        grid_vals[["imputed"]] <- 2 # studies not need imputation, and studies with imputed vals from hanbook


        if (outcome_type == "bin") {
            grid_vals[["outcome"]] <- c(1, 3, 6, 7)
        } # revisions, displacement, avn, nonunion}
    }


    if (protocol %in% c("skou", "skou_rct")) {
        grid_vals[["language"]] <- 2
        grid_vals[["age"]] <- 5 # > 18 years
        grid_vals[["imputed"]] <- 2 # studies not need imputation, and studies with imputed vals from hanbook

        if (outcome_type %in% c("func", "qol")) {
            grid_vals[["year"]] <- 2 # all studies regardless of publ_year
            grid_vals[["design"]] <- 1 # only rct
            grid_vals[["follow_up"]] <- c(unique(df$follow_up), 98) # unique vals together or val closest to 12 mths - bin_fu_longest recoded
        }
        if (outcome_type == "bin") {
            grid_vals[["year"]] <- 1 # only studies published after 2000
            grid_vals[["design"]] <- 2 # RCT + any type of NRSI
            grid_vals[["follow_up"]] <- 98 # only val closest to 12 mths bin_fu_long recoded

            if (protocol == "skou_rct") {
                grid_vals[["design"]] <- 1
            }
        }
    }


    if (protocol == "rct") {
        grid_vals["design"] <- 1
    }


    ### CREATE GRID
    grid <- expand_grid.(!!!grid_vals) %>%
        mutate.(
            across.(where(is.numeric), as.integer),
            across.(where(is.character), as_factor)
        )

    return(grid)
}


# remove null combos from grid

remove_nulls <- function(df, grid, list_of_combos) {
    df <- df %>%
        select.(-outcome) %>%
        rename.(
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
                n_rows <- nrow(df[get(var1) == val1 & get(var2) == val2])
                if (n_rows == 0) {
                    grid <- grid[!(get(var1) == val1 & get(var2) == val2)]
                }
            }
        }
    }
    return(grid)
}



# split multi outcome -----------------------------------------

get_dupli_outcomes <- function(df) {
    studlabs <- df %>%
        count.(studlab) %>%
        filter.(N > 1) %>%
        pull.("studlab")
    outcomes <- df %>%
        filter.(studlab %in% studlabs) %>%
        pull.(outcome) %>%
        unique() %>%
        as.character()
    return(outcomes)
}



get_dfs <- function(df, outcomes) {
    loop_out <- list()
    if (length(outcomes) == 0) {
        loop_out[[1]] <- df
    } else {
        for (outc in outcomes) {
            loop_out[[outc]] <- df %>% filter.(outcome == outc)
        }
    }
    return(loop_out)
}


merge_dfs <- function(base_df, prim_dfs) {
    out <- list()
    for (idx in seq_along(prim_dfs)) {
        df <- prim_dfs[[idx]]
        out[[idx]] <- bind_rows.(base_df, df)
    }
    return(out)
}



## for loop over outcomes
split_outcomes <- function(df, outc, row_id) {
    out <- list()
    ### Filter on primary outcome ->  filter unq -> merge to base_df
    prim_df <- df %>% filter.(outcome == outc)
    rest_df <- df %>% filter.(!studlab %in% prim_df[["studlab"]])
    dupli_studlabs <- rest_df %>%
        count.(studlab) %>%
        filter.(N > 1) %>%
        pull.(studlab)
    unq_rest <- rest_df %>% filter.(!studlab %in% dupli_studlabs)
    # it is only the first time that there are unq.
    unq_rest <- rest_df %>% filter.(!studlab %in% dupli_studlabs)

    base_df <- bind_rows.(prim_df, unq_rest)

    ### Identify remaining -> split on outcome -> merge to base
    rest_df2 <- df %>% filter.(!studlab %in% pull.(base_df, studlab))

    if (nrow(rest_df2) == 0) {
        out <- append(out, list(base_df))
        out <- out %>%
            map(~ .x %>% arrange.(studlab)) %>%
            set_names(~ str_c(row_id, outc, seq_along(.), sep = "_"))
        return(out)
    }

    prim_dfs <- get_dupli_outcomes(rest_df2) %>% map(~ rest_df2 %>% filter.(outcome == .x))
    base_dfs <- prim_dfs %>% map(~ bind_rows.(base_df, .x))

    ### identify remaining -> split on outcome -> add to base_dfs

    rest_dfs <- base_dfs %>% map(~ df %>% filter.(!studlab %in% .x[["studlab"]]))
    done_idx <- rest_dfs %>%
        map(~ nrow(.x) == 0) %>%
        unlist()
    if (sum(done_idx) == length(base_dfs)) {
        out <- base_dfs
    } else {
        out <- append(out, base_dfs[done_idx])
    }

    rest_dfs <- rest_dfs[!done_idx]
    base_dfs <- base_dfs[!done_idx]


    while (length(base_dfs) > 0) {
        dupli_outcomes <- rest_dfs %>% map(~ get_dupli_outcomes(.x))
        prim_dfs <- map2(dupli_outcomes, rest_dfs, ~ get_dfs(.y, .x))
        base_dfs <- map2(prim_dfs, base_dfs, ~ merge_dfs(.y, .x)) %>% flatten()

        ### identify remaining -> split on outcome -> add to base_dfs
        rest_dfs <- base_dfs %>% map(~ df %>% filter.(!studlab %in% .x[["studlab"]]))
        done_idx <- rest_dfs %>%
            map(~ nrow(.x) == 0) %>%
            unlist()
        out <- append(out, base_dfs[done_idx])
        rest_dfs <- rest_dfs[!done_idx]
        base_dfs <- base_dfs[!done_idx]
    }
    out <- out %>%
        map(~ .x %>% arrange.(studlab)) %>%
        set_names(~ str_c(row_id, outc, seq_along(.), sep = "_"))
    return(out)
}

### SUBSET FUNCTION --------------------

do_subset <- function(df,
                      row_id,
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
                      ttfu,
                      rob) {
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
    sel_rob <- 1:rob

    # outcomes
    if (!outcome %in% c(98, 99)) { # select specific outcomes
        sel_outcome <- outcome
    } else if (outcome == 98) { # select all outcomes
        sel_outcome <- unique(df$bin_outcome)
    } else if (outcome == 99) {
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
        sel_fu_longest <- c(0, 1) # select all
        sel_fu_period <- fu_period # select specific period
        sel_fu_period_long <- 1 # select longest fu in each period
    }

    # outcome analysis
    if (outcome_analysis == 98) {
        sel_oa <- unique(df$bin_oa)
    } else {
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
    } else if (neer == 98) { # select 3 and 4 parts
        sel_neer <- c(3, 4)
    } else if (neer == 99) { # select all neer types
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
            bin_ttfu %in% sel_ttfu &
            bin_rob %in% sel_rob
    ]

    if (nrow(subset) == 0) {
        return(NULL)
    }
    if (any(duplicated(subset[["studlab"]]))) {
        subset <- get_dupli_outcomes(subset) %>%
            map(~ split_outcomes(subset, .x, row_id)) %>%
            flatten()
        subset <- subset[!duplicated(subset)]
    } else {
        return(subset)
    }
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
            data = data,
            comb.random = TRUE,
            comb.fixed = TRUE
        )
    } else if (outcome == "bin") {
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
            comb.fixed = TRUE,
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
    if (outcome == "bin") { # backconvert te
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
            iteration = iteration,
            pval = pval.fixed,
            estimate = te.fixed,
            k = k,
            method = "fixed",
            outcome = outcome,
            protocol = protocol,
            i2 = i2,
            q = q,
            pval.q = pval.q,
            studlab = studlab
        ) %>%
        bind_rows(
            .,
            data %>%
                transmute(
                    iteration = iteration,
                    pval = pval.random,
                    estimate = te.random,
                    k = k,
                    method = "random",
                    outcome = outcome,
                    protocol = protocol,
                    i2 = i2,
                    q = q,
                    pval.q = pval.q,
                    studlab = studlab
                )
        )
}