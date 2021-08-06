library(tidyverse)
library(data.table)
library(arrow)

results_path <- "/home/nicolai/Desktop/results_merged.feather"


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