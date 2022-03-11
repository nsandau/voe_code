library(tidyverse)
library(tidytable)
library(data.table)
library(arrow)
library(flextable)



df <- data_cont

df %>% view()
#####################

data_extract %>%
    select(-neer_converted) %>%
    mutate(imputed_outcome = str_replace_all(imputed_outcome, "wrong", "not_std"), imputed_vars = str_replace_all(imputed_vars, "cs, avn", "cs")) %>%
    readr::write_csv("output/data.csv", na = "")


readr::read_csv("output/data.csv") %>%
    select(-starts_with("complications")) %>%
    glimpse()

##########

# CALCULATE COMPLICATIONS

data_extract
##### WORKING

complications <- data_cont %>%
    group_by(studlab, follow_up) %>%
    select(studlab, follow_up, outcome, starts_with(c("con_", "int_"))) %>%
    filter(!outcome %in% c("complications", "revision")) %>%
    summarize(int_e = sum(int_e), int_n = int_n, con_e = sum(con_e), con_n = con_n) %>%
    distinct() %>%
    filter(con_n == max(con_n) & int_n == max(int_n)) %>%
    mutate(outcome = "complications") %>%
    ungroup() %>%
    left_join(data_cont %>% select(-c(int_e, int_n, con_e, con_n, outcome)), by = c("studlab", "follow_up")) %>%
    distinct()

bind_rows(data_cont, complications)

data_cont




data_cont %>% view()


##################
data_extract %>%
    filter(studlab == "stableforth 1984") %>%
    select(infection_18_e)

data_all <- read_rds("output/data_all.rds")


data_all %>%
    select(studlab, language, total_n, total_avg_age, interv, follow_up, outcome, adverse_events) %>%
    group_by(studlab) %>%
    mutate(
        follow_up = paste0(unique(follow_up)[!is.na(unique(follow_up))], collapse = ", "),
        outcome = paste0(unique(outcome)[!is.na(unique(outcome))], collapse = ", "),
        adverse_events = paste0(unique(adverse_events)[!is.na(unique(adverse_events))], collapse = ", "),
    ) %>%
    distinct() %>%
    flextable()



###
data %>%
    glimpse() %>%
    select(studlab, publ_date, bin_year) %>%
    distinct() %>%
    mutate(
        bin_year_2 = case_when(
            year(publ_date) >= 1995 ~ 1,
            year(publ_date) >= 1990 ~ 2,
            TRUE ~ 3
        )
    ) %>%
    view()


###



out[["fjalestad 2014"]][!is.na(out[["fjalestad 2014"]])]

data_all %>%
    group_by(studlab) %>%
    dplyr::summarise(
        intervention = unique(interv)
    )


readr::write_rds(data_qol, "output/data_qol.rds")
readr::write_rds(data_func, "output/data_func.rds")
readr::write_rds(data_bin, "output/data_bin.rds")
readr::write_rds(data_all, "output/data_all.rds")


data_qol
data_func
data_bin



subsets <- read_rds("output/test_rowid_qol_subsets.rds")


subsets[["230641"]]
subsets <- subsets %>%
    set_names(sel_grid$row_id) %>%
    discard(~ is.null(.x))

names(subsets) %>%
    enframe() %>%
    filter(str_detect(value, "230641"))
##### NÅET HERTIL ! ###
split_multi_outc <- function(df, row_id) {
    studlabs <- df %>%
        count.(studlab) %>%
        filter.(N > 1) %>%
        pull.("studlab")
    outcomes <- df %>%
        filter.(studlab %in% studlabs) %>%
        pull.(outcome) %>%
        unique() %>%
        as.character()

    loop_out <- list()
    for (outc in outcomes) {
        lbl <- str_c(as.character(row_id), "_", outc)
        prim_df <- df %>% filter.(outcome == outc)

        prim_studlabs <- unique(prim_df[["studlab"]])

        rest_df <- df %>%
            filter.(!studlab %in% prim_studlabs)

        if (any(duplicated(rest_df$studlab))) {
            split_list <- split_multi_outc(rest_df, row_id)
            loop_out[[lbl]] <- c(list(prim_df), flatten(split_list))
        } else {
            if (nrow(rest_df) > 0) {
                loop_out[[lbl]] <- list(prim_df, rest_df)
            } else {
                loop_out[[lbl]] <- list(prim_df)
            }
        }
    }
    return(loop_out)
}

# For outcomes hvor n_studlab er > 2

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


outc

split_dfs <- function(df, outcomes, row_id) {
    prim_df <- df %>% filter.(outcome == outc)
    prim_studlabs <- unique(prim_df[["studlab"]])
    rest_df <- df %>%
        filter.(!studlab %in% prim_studlabs)
    dupli_rest <- rest_df %>%
        count.(studlab) %>%
        filter.(N > 1) %>%
        pull.("studlab")
    unq_rest <- rest_df %>% filter.(!studlab %in% dupli_rest)
    df_list <- append(df_list[[lbl]], list(prim_df))
    df_list <- append(df_list[[lbl]], list(unq_rest))
    ## remaining dupli_res
    remainder <- rest_df %>% filter.(studlab %in% dupli_rest)

    return(df_list)
}






split_df <- function(df, outcomes) {
    df_list <- list()
    for (outc in outcomes) {
        ### identify studies
        prim_df <- df %>% filter.(outcome == outc)
        prim_studlabs <- unique(prim_df[["studlab"]])
        rest_df <- df %>%
            filter.(!studlab %in% prim_studlabs)
        dupli_rest <- rest_df %>%
            count.(studlab) %>%
            filter.(N > 1) %>%
            pull.("studlab")
        unq_rest <- rest_df %>% filter.(!studlab %in% dupli_rest)

        ## Merge dataframes
        out_df <- bind_rows.(prim_df, unq_rest)

        if (length(dupli_rest) > 0) {
            remain_outc <- rest_df %>% filter.(studlab %in% dupli_rest)
            df_lists <- split_df(remain_outc, get_dupli_outcomes(remain_outc))
        } else {
            out_list <- list(out_df)
        }
        df_list[[outc]] <- out_list
    }
    return(df_list)
}


df_list <- list()
prim_df <- df %>% filter.(outcome == outc)
prim_studlabs <- unique(prim_df[["studlab"]])
rest_df <- df %>%
    filter.(!studlab %in% prim_studlabs)
dupli_rest <- rest_df %>%
    count.(studlab) %>%
    filter.(N > 1) %>%
    pull.("studlab")
unq_rest <- rest_df %>% filter.(!studlab %in% dupli_rest)
df_list <- append(df_list, list(prim_df))
df_list <- append(df_list, list(unq_rest))
df_out <- rbindlist(df_list)
remain_outc <- rest_df %>% filter.(studlab %in% dupli_rest)

prim_df

unq_rest

## Split rest i unique rest og dupli_studlab rest

## Hvis nrow dupli_studlab_rest > 0:
## gentag ovenstående?


split_dfs <- function(df) {
    outcomes <- get_dupli_outcomes(df)
    prim_dfs <- outcomes %>% map(~ df %>% filter.(outcome == .x))
    rest_dfs <- prim_dfs %>% map(~ df %>% filter.(!studlab %in% .x[["studlab"]]))
    dupli_studlabs <- rest_dfs %>% map(~ .x %>%
        count.(studlab) %>%
        filter(N > 1) %>%
        pull.(studlab))

    unq_rest <- rest_dfs %>% map2(dupli_studlabs, ~ .x %>% filter.(!studlab %in% .y))

    base_dfs <- unq_rest %>% map2(prim_dfs, ~ bind_rows.(.x, .y))

    return(base_dfs)
}



##################


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
####################################### 3

## inde i subset func:

if (any(duplicated(subset[["studlab"]]))) {
    duplicated_outcomes <- get_dupli_outcomes(subset)
    subset <- duplicated_outcomes %>%
        map(~ split_outcomes(subset, .x, row_id)) %>%
        flatten()
    subset <- subset[!duplicated(subset)]
}

#############

## INDE I METAGEN

subsets <- read_rds("output/test_subsets.rds")

subsets[str_detect(names(subsets), "52519")]

subsets <- c(
    rrapply(subsets, f = identity, classes = "data.frame", how = "flatten"),
    rrapply(subsets, f = identity, classes = "list", how = "flatten") %>%
        flatten()
)

rrap1 <- rrapply(subsets, f = identity, classes = "data.frame", how = "flatten")
rrap2 <- rrapply(subsets, f = identity, classes = "list", how = "flatten")


rrap1 %>% length()


rrap2 %>% length()

subsets %>% length()

(length(rrap2) + length(subsets)) == length(rrap1)

rrap1[str_detect(names(rrap1), "52519")]


#############################

## REMAINING DATAFRAME
dupli_rest_dfs <- base_dfs %>% map(~ df %>% filter.(!studlab %in% .x[["studlab"]]))

prim_dfs2 <- dupli_rest_dfs %>% map(~ get_prim_dfs(.x))
unq_rest_dfs2 <- get_unq_rest_dfs(prim_dfs2)


map2(unq_rest_dfs2, ~ bind_rows(.x, .y)) ### JEG ER NÅET HERTIL - DET ER FORDI DET ER NESTED LISTS. SKAL FINDE PÅ EN MÅDE AT KOMBINERE HVER SUBLISTE MED PRIM_DFS2

# TODO: Første gang er anderledes end anden
# Hvis jeg kan lave en function der står for anden kan jeg køre den så mange gange jeg vil ..
## Nemmere at lave det uden map men som en func?
# fokusere på at dine dfs med unique entries og add dem baglæns


############ MULTI_SPLIT
df_qol <- read_rds("output/test_split_qol.rds") %>% select.(studlab, outcome)
df_func <- read_rds("output/test_split_func.rds") %>% select.(studlab, outcome)

outc <- "ases"
row_id <- 123
df <- df_func

subset <- df_func %>%
    split_multi_outc(row_id) %>%
    map(rbindlist)

subset

##### TEST QOL

test_qol <- df_qol %>%
    split_multi_outc(row_id) %>%
    map(rbindlist)
test_qol_bool <- test_qol %>%
    map(~ any(duplicated(.x[["studlab"]]))) %>%
    unlist()

testthat::expect_false(any(test_qol_bool))

##### TEST FUNC
test_func <- df_func %>%
    split_multi_outc(row_id) %>%
    map(rbindlist)
test_func_bool <- test_func %>%
    map(~ any(duplicated(.x[["studlab"]]))) %>%
    unlist()

testthat::expect_false(any(test_func_bool))






# %%
df1 <- df %>% filter(outcome == "eq5d")
df2 <- df %>% filter(!(studlab == "rangan 2015" & outcome == "eq5d"))

test <- list("123_sf12pcs" = df2, "123_eq5d" = df1)

testthat::expect_equal(subset, test)
test

####

### COMBINING LISTS
subsets <- c(
    rrapply(subsets, f = identity, classes = "data.frame", how = "flatten"),
    rrapply(subsets, f = identity, classes = "list", how = "flatten") %>%
        flatten()
)


testthat::expect

#####

sel_grid %>% filter(row_id == 73249)

subset


idx <- c("3261299_sf12pcs", "11633")


str_split(idx, "_") %>%
    map(~ .x[[1]]) %>%
    unlist()


OUTCOME <- "qol"
PROTOCOL <- "handoll"

sel_grid <- readr::read_rds("output/sel_grid_qol.rds") %>% mutate.(row_id = row_number.())

## read in results_merged

results_merged <- read_feather("output/results_merged.feather")


## identify most disc for specific outcome and protocol

most_disc <- list()

results <- results_merged %>%
    gather_pvals() %>%
    drop_na() %>%
    filter(outcome == OUTCOME, protocol == PROTOCOL) %>%
    filter(estimate == max(estimate) | estimate == min(estimate)) %>%
    arrange(iteration)

most_disc[["results"]] <- results

idx <- results %>%
    distinct(iteration) %>%
    pull(iteration) %>%
    as.integer()

## use indexes to subset sel grid

sel_grid <- sel_grid %>%
    filter(row_id %in% idx)

most_disc[["sel_grid"]] <- sel_grid

## Create a list with sel_grid, subsets and meta-analyses





read_feather(results_path) %>%
    pivot_longer(c(te.fixed, te.random)) %>%
    filter(k > 1) %>%
    select(name, value, pval.fixed, pval.random, protocol, iteration) %>%
    filter(protocol == "handoll") %>%
    arrange(value)


read_feather(results_path) %>%
    gather_pvals() %>%
    filter(protocol == "handoll", outcome == "func") %>%
    subsets() %>%
    names()

test_list <- list("tester" = data_cont, "tester3" = list("2_jas" = data_cont, "2_jos" = data_cont))

test_list %>% set_names(seq_along(.))


grid <- readr::read_rds("output/sel_grid_qol.rds") %>% mutate(row_id = row_number())
sel_grid <- grid

grid %>%
    mutate.(id = row_number.()) %>%
    slice(idx) %>%
    mutate(iter = idx) %>%
    select(iter, id)


## add row_numbers
grid %>%
    mutate.(
        iter = row_number.()
    ) %>%
    slice_sample(n = 50) %>%
    View()
### finding bin outlier

results_df %>% filter(te.random > 50)

pvals %>%
    filter(estimate > 50) %>%
    sel_grid[[12913]]


subsets[["129133"]]


res <- read_feather("output/results_merged.feather")


res %>% glimpse()

### function for removing non-distinct values

results_merged <- results_merged %>% mutate(protocol = "test", outcome = "qol")

results_merged <- results_merged %>% mutate.(protocol = "proto", outcome = "outcome")

results_merged %>%
    distinct(across(-starts_with("iteration"))) %>%
    nrow()



subs_1k <- subsets %>% discard(~ nrow(.x) < 2)

length(subs_1k)











sel_grid <- read_rds("output/sel_grid_qol.rds")
OUTCOME <- "func"
PROTOCOL <- "none"
SPLIT_NO <- 1
N_SPLITS <- 6

splits <- list()


for (SPLIT_NO in 1:6) {
    sel_grid <- read_rds("output/sel_grid_qol.rds")
    if (OUTCOME %in% c("func", "bin") & PROTOCOL %in% c("none", "handoll")) {
        cat("Splitting sel_grid", "\n")
        part <- floor(nrow(sel_grid) / N_SPLITS)

        start <- ((SPLIT_NO - 1) * part + 1)
        stop <- (SPLIT_NO * part)

        if (SPLIT_NO == N_SPLITS) {
            stop <- nrow(sel_grid)
        }

        sel_grid <- sel_grid[start:stop, ]
        cat("Length of sel_grid after split: ", nrow(sel_grid), "\n")
        splits[[SPLIT_NO]] <- sel_grid
    }
}

df <- bind_rows(splits)


testthat::expect_equal(df, sel_grid)



data_extract <- read_rds("data/data_extract.rd")

data %>%
    select(outcome, bin_outcome) %>%
    distinct()


data_cont %>%
    group_by(studlab, outcome) %>%
    mutate(
        fu_diff = abs(follow_up - 12),
        bin_fu_longest = case_when(
            fu_diff == min(fu_diff) ~ 1,
            TRUE ~ 0
        )
    ) %>%
    select(studlab, outcome, follow_up, fu_diff, bin_fu_longest)

test %>% select(studlab, follow_up, bin_fu_period, interv)

unique(test$bin_lang)


filter(test, bin_lang == 1)

list_of_cols() <- list(
    intervention = c(1, 2, 3, 4),
    year = c(1, 2, 3, 4)
)

expand_grid.(!!!list_of_cols)


list_of_cols[["intervention"]] <- c(1, 1)

list_of_cols

expand_grid.(!!!list_of_cols) %>% distinct.()

unique(c(1, max(data$bin_lang)))

### MERGE BINS

bin_bin <- data_cont %>%
    filter(outcome %in% OUTCOME_VARS) %>%
    make_binary(outcome = OUTCOME)

bin_qol %>% pull(outcome)
bin_func %>% pull(outcome)
bin_bin %>% pull(outcome)


bin_all <- bin_qol %>%
    bind_rows(bin_func) %>%
    bind_rows(bin_bin)


write_rds(bin_all, "/home/nicolai/OneDrive/Forskning/Phd/3 Simulation/04 - manus/data/data_bin_all.rds")





data_extract %>%
    mutate(
        bin_rob = case_when(
            rob_tool == "rob2" & rob2_overall == "some concern" ~ "moderate",
            rob_tool == "rob2" & rob2_overall != "some concern" ~ rob2_overall,
            rob_tool == "rob" & rob_overall == "unclear" ~ "moderate",
            rob_tool == "rob" & rob_overall != "unclear" ~ rob_overall,
            rob_tool == "minors" & minors_total <= 8 ~ "high",
            rob_tool == "minors" & between(minors_total, 9, 15) ~ "moderate",
            rob_tool == "minors" & minors_total >= 16 ~ "low",
            rob_tool == "nos" & nos_total <= 2 ~ "high",
            rob_tool == "nos" & between(nos_total, 3, 4) ~ "moderate",
            rob_tool == "nos" & nos_total >= 5 ~ "low"
        ),
        bin_rob = factor(bin_rob, ordered = TRUE, levels = c("low", "moderate", "high")),
        bin_rob_int = as.integer(bin_rob)
    ) %>%
    select(rob_tool, starts_with(c("rob", "minors", "nos")), bin_rob, bin_rob_int) %>%
    View()












### TABLE 1

library(gtsummary, quietly = T)

data_bin_bin <- data_cont %>% make_binary(outcome = OUTCOME)


data_bin_all <- bind_rows(data_bin_qol, data_bin_qol) %>% bind_rows(data_bin_bin)

write_rds(data_bin_all, here("output", "data_bin_all.rds"))
write_rds(data_bin_func, here("output", "data_bin_func.rds"))


table1 <- data_bin_all %>%
    group_by(studlab) %>%
    mutate(
        publ_year = case_when(
            bin_year == 1 ~ "After 1995",
            bin_year == 2 ~ "Between 1990 and 1995",
            bin_year == 3 ~ "Before 1990"
        ),
        time_of_intervention = case_when(
            bin_toi == 1 ~ "Less than 48 hours from injury",
            bin_toi == 2 ~ "Between 2 and 14 days from injury",
            bin_toi == 3 ~ "Between 14 and 21 days from injury",
            bin_toi == 4 ~ "More than 21 days from injury"
        ),
        mean_age_patients = case_when(
            bin_age == 1 ~ "Above 65 years",
            bin_age == 2 ~ "Between 60 and 65",
            bin_age == 3 ~ "Between 55 and 60",
            bin_age == 4 ~ "Between 50 and 55",
            bin_age == 5 ~ "Between 18 and 50",
            bin_age == 6 ~ "Between 16 and 18",
            bin_age == 7 ~ "Less than 16"
        ),
        # hvis et studie har imputed outcome for et outcome, score som yes
        imputed_outcome = if_else(any("yes" %in% imputed_outcome), "yes", "no"),
        tbl_n = if_else(total_n > 15, "More than 15", "Less than 15"),
        ltfu_more_than_15_pct = if_else(bin_loss_fu == 2, "Yes", "No"),
        tbl_neer = case_when(
            bin_neer == 2 ~ "Primarily 2-part fractures",
            bin_neer == 3 ~ "Primarily 3-part fractures",
            bin_neer == 4 ~ "Primarily 4-part fractures",
            bin_neer == 0 ~ "Reported as AO classification"
        ),
        tbl_ttfu = case_when(
            bin_ttfu == 1 ~ "12 months, or longer",
            bin_ttfu == 2 ~ "Between 6 and 12 months",
            bin_ttfu == 3 ~ "Less than 6 months"
        )
    ) %>%
    select(
        studlab,
        interv,
        language,
        publ_year,
        design,
        databases,
        documentation_for_treatment,
        time_of_intervention,
        outcome_reported_as,
        method_of_outcome_analysis,
        nrsi_adj_outcome,
        imputed_outcome,
        risk_of_bias_tool,
        mean_age_patients,
        tbl_n,
        ltfu_more_than_15_pct,
        tbl_neer,
        tbl_ttfu
    ) %>%
    ungroup() %>%
    distinct() %>%
    select(-studlab) %>%
    tbl_summary()






# FOR OUTCOMES KAN JEG BARE STACKE

data_cont %>% glimpse()

data_cont %>%
    mutate(
        imputed_outcome = case_when(
            str_detect(imputed_vars, outcome) & follow_up == imputed_timepoint ~ imputed_outcome,
            TRUE ~ "no"
        )
    ) %>%
    select(studlab, starts_with("imputed"), outcome, follow_up) %>%
    View()


data_cont %>%
    select(studlab, starts_with("imputed"), outcome, follow_up) %>%
    View()



str_detect(data_cont$imputed_vars, data_cont$outcome)

####