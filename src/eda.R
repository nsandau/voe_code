
test <- data_cont %>% mutate(
    bin_outcome = as.numeric(fct_relevel(outcome, "cs"))
)
test %>% select(outcome, bin_outcome)

df <- test

grid <- expand_grid(
    language = unique(df$bin_lang),
    outcome = c(unique(df$bin_outcome), 51:52)
)

str_subset(unique(data_bin$outcome), fixed("cs"), negate = TRUE)


data_cont %>%
    mutate(
        bin_outcome = as.integer(as.factor(outcome))
    ) %>%
    select(outcome, bin_outcome)


!unique(data_bin$outcome) %in% c("cs")


############################ 3
data_cont %>%
    group_by(studlab) %>%
    mutate(
        pct_loss = (total_loss_to_fu / total_n),
        bin_loss_fu = case_when(
            (total_loss_to_fu / total_n) < 0.15 ~ 1,
            TRUE ~ 2
        )
    ) %>%
    select(studlab, total_n, total_loss_to_fu, pct_loss, bin_loss_fu) %>%
    View()

data_cont %>% count(documentation_for_treatment)


data_cont %>%
    mutate(
        bin_doctreat = if_else(documentation_for_treatment == "yes", 1, 2)
    ) %>%
    select(studlab, documentation_for_treatment, bin_doctreat) %>%
    distinct() %>%
    View()



data_cont %>%
    mutate(
        bin_toi = case_when(
            between(time_of_intervention, 0, 2) ~ 1,
            between(time_of_intervention, 2.0001, 14) ~ 2,
            between(time_of_intervention, 14.0001, 21) ~ 3,
            TRUE ~ 4
        )
    ) %>%
    select(studlab, time_of_intervention, bin_toi) %>%
    distinct() %>%
    View()




data_cont %>%
    mutate(
        bin_34part = case_when(
            total_neer_1_part == 0 & total_neer_2_part == 0 ~ 1,
            TRUE ~ 2
        )
    ) %>%
    select(studlab, contains("neer"), bin_34part) %>%
    View()

data_cont %>%
    mutate(
        bin_34part = if_else(total_neer_1_part == 0 & total_neer_2_part == 0, 1, 2)
    ) %>%
    select(studlab, contains("neer"), bin_34part) %>%
    View()



test_bin <- data_extract %>%
    select(studlab, intervention_type, language) %>%
    mutate(
        intervention_type = as_factor(intervention_type),
        intervention_type = fct_expand(intervention_type, c("orif")),
        language = as_factor(if_else(language == "english", language, "other"))
    )

expand_grid(
    interv = levels(test_bin$intervention_type),
    studlab = unique(test_bin$studlab)
) %>% mutate(across(where(is.character), as.factor))




data_extract %>%
    group_by(studlab) %>%
    summarize(n_sample = total_n - replace_na(total_loss_to_fu, 0)) %>%
    select(n_sample)

data_extract %>%
    mutate(n_sample = total_n - replace_na(total_loss_to_fu, 0)) %>%
    select(studlab, total_n, total_loss_to_fu, n_sample)


unq_df() <- map_df(df, ~ tibble(class = class(.), value = toString(unique(.))))


unq_df %>% filter(class == "character")


data_extract %>%
    select(
        -starts_with(bin_outcomes),
        -starts_with(qol_outcomes),
        -starts_with(prom_outcomes)
    ) %>%
    glimpse()


data_extract %>%
    left_join(data_extract %>%
        group_by(studlab) %>%
        summarize(
            total_n = sum(n),
            total_loss_to_fu = sum(lost_to_fu)
        )) %>%
    select(-n, lost_to_fu)


data_extract %>% left()
total_loss_to_fu

data_extract %>%
    group_by(studlab) %>%
    summarize()

data_extract %>%
    select(starts_with(cont_outcomes)) %>%
    ncol()


data_extract %>%
    select(studlab, starts_with("neer")) %>%
    group_by(studlab) %>%
    summarize(
        total_neer_1_part = sum(neer_1_part),
        total_neer_2_part = sum(neer_2_part),
        total_neer_3_part = sum(neer_3_part),
        total_neer_4_part = sum(neer_4_part)
    )



data_cont %>% map_df(~ count(.x))


data_cont %>% count(databases)


characteristics <- data_extract %>%
    select(2:14, starts_with("total"), -imputed_vars) %>%
    names()

characteristics %>%
    map(~ n_distinct(data_extract[.x])) %>%
    set_names(characteristics) %>%
    enframe() %>%
    unnest(value)


data_extract %>% count(language)


unq_df <- data_extract %>%
    select(characteristics) %>%
    map_df(~ tibble(class = class(.), n_dist = n_distinct(.), value = toString(unique(.))))



data_cont %>% filter(outcome %in% c(prom_outcomes, "cs"))