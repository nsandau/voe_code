pacman::p_load(tidyverse)

df <- read_excel(here("data", "p3 data extract.xlsx")) %>%
    clean_names() %>%
    mutate_if(is.character, str_to_lower) %>%
    rename(studlab = study_identifier)

unq_df <- map_df(df, ~ tibble(class = class(.), value = toString(unique(.))))


unq_df %>% filter(class == "character")


data_extract <- read_excel(here("data", "p3 data extract.xlsx")) %>%
    clean_names() %>%
    mutate_if(is.character, str_to_lower) %>%
    rename(studlab = study_identifier)


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