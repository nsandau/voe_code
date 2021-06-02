### NOTER

## TODO:
## unit testing
## når laver imputation - i make_bin tage højde for hvilket outcome der er imputed


## Udgået:
# zv: publ_status, databases, outcom_rep_as
# sample_n: none smaller than min requirement (15)
# time to follow-up: ingen under 12 mdr

# LIBRARIES ---------------------------------------------------------------

### SKAL LAVE ET DOCKER IMAGE DER ALLEREDE HAR INSTALLERET PACKAGES.
## INDTIL DA:
## lib <- .libPaths("/home/kmd592_ku_dk/modi_mount/R_lib")
## install.packages("pacman", repos = "https://mirrors.dotsrc.org/cran/", lib = lib[1])

pacman::p_load(
  esc,
  conflicted,
  janitor,
  furrr,
  testthat,
  magrittr,
  readxl,
  here,
  lubridate,
  dplyr,
  stringr,
  purrr,
  tidyr,
  forcats,
  benchmarkme,
  tictoc,
  data.table,
  update = FALSE
)

# settings
cores <- future::availableCores()
ram <- benchmarkme::get_ram()
cat("Using", cores, "cores, and", round(ram / 1024 / 1024 / 1024), "gb ram", "\n")
plan(multicore, workers = cores)

conflict_prefer("filter", "dplyr")
conflict_prefer("between", "dplyr")
conflict_prefer("year", "lubridate")

# import functions
source(here("src", "functions.R"))

# IMPORT ------------------------------------------------------------------
data_extract <- read_excel(here("data", "p3 data extract.xlsx")) %>%
  clean_names() %>%
  mutate(
    across(where(is.character), str_to_lower),
    publ_date = as_date(publ_date)
  ) %>%
  rename(studlab = study_identifier) %>%
  select(-notes)

# write_rds(data_extract, here("data", "data_extract.rds"))

# STRINGS FOR FILTERING ---------------------------------------------------

prom_outcomes <- c(
  "oss",
  "ases",
  "sane",
  "pss",
  "qdash",
  "dash",
  "sst"
)
qol_outcomes <- c(
  "eq5d",
  "qol15d",
  "qolpromis",
  "sf12pcs",
  "sf36pcs",
  "vr12"
)
cont_outcomes <- c(prom_outcomes, "cs", qol_outcomes)
bin_outcomes <- c(
  "revision",
  "complications",
  "displacement",
  "impingement",
  "rct",
  "instability",
  "failure",
  "metalwork",
  "nerve_inj",
  "infection",
  "avn",
  "malunion",
  "nonunion",
  "stiffness"
)

nrsi_studies <- data_extract %>%
  filter(str_detect(design, "nrsi")) %>%
  pull(studlab) %>%
  unique()

# REMOVE COLS WITH ZERO VARIANCE--------------------------------------
char_col <- data_extract %>%
  select(-starts_with(cont_outcomes), -starts_with(bin_outcomes)) %>%
  names()

zv_cols <- data_extract %>%
  select(all_of(char_col)) %>%
  map_df(~ tibble(
    class = class(.),
    n_dist = n_distinct(.),
    value = toString(unique(.)),
  ), .id = "col_name") %>%
  filter(n_dist == 1) %>%
  pull(col_name)

data_extract <- data_extract %>% select(-all_of(zv_cols))

# CALCULATE TOTAL AGE AND LOSS TO FU --------------------------------
data_extract <- data_extract %>%
  left_join(data_extract %>%
    group_by(studlab) %>%
    summarize(
      total_n = sum(n),
      total_loss_to_fu = sum(lost_to_fu),
      total_neer_1_part = sum(neer_1_part),
      total_neer_2_part = sum(neer_2_part),
      total_neer_3_part = sum(neer_3_part),
      total_neer_4_part = sum(neer_4_part)
    )) %>%
  select(-c(n, lost_to_fu), -starts_with("neer"))


# TEST: NO STUDY HAS < 15 samples
expect_true(all(data_extract$total_n - replace_na(data_extract$total_loss_to_fu, 0) > 15))

# RESHAPE DF --------------------------------------------------------------

data_cont <- data_extract %>%
  select(
    -starts_with(all_of(bin_outcomes))
  ) %>%
  pivot_longer(
    cols = starts_with(all_of(cont_outcomes)),
    names_to = c("outcome", "follow_up", "type"),
    names_sep = "_",
    values_drop_na = T
  ) %>%
  mutate(
    bin_interv = case_when(
      intervention_type == "nonop" ~ "con",
      TRUE ~ "int"
    )
  ) %>%
  select(-intervention_type) %>%
  pivot_wider(
    names_from = c("bin_interv", "type"),
    values_from = value
  ) %>%
  left_join(
    (data_extract %>%
      select(
        studlab,
        intervention_type
      ) %>%
      filter(intervention_type != "nonop") %>%
      rename(interv = intervention_type)
    )
  ) %>%
  mutate(
    follow_up = as.integer(follow_up),
    outcome = as_factor(outcome),
    interv = as_factor(interv)
  )

### TESTS ###
testthat::expect_setequal(data_cont %>% filter(is.na(con_n)) %>% nrow(), 0)
testthat::expect_setequal(data_cont %>% filter(is.na(int_n)) %>% nrow(), 0)
####

# REVERSE DASH  ---------------------------------------------------

data_cont <- data_cont %>%
  mutate(
    int_mean = if_else(outcome == "dash", 100 - int_mean, int_mean),
    con_mean = if_else(outcome == "dash", 100 - con_mean, con_mean),
    int_mean = if_else(outcome == "qdash", 100 - int_mean, int_mean),
    con_mean = if_else(outcome == "qdash", 100 - con_mean, con_mean)
  )

# CALCULATE SMD ------------------------------------------------

data_cont <- data_cont %>%
  group_by(studlab, interv) %>%
  nest() %>%
  mutate(
    esc_output = map(data, ~
    esc_mean_sd(
      grp1m = .x$int_mean,
      grp1sd = .x$int_sd,
      grp1n = .x$int_n,
      grp2m = .x$con_mean,
      grp2sd = .x$con_sd,
      grp2n = .x$con_n,
      es.type = "g"
    ))
  ) %>%
  mutate(
    smd = map(esc_output, "es"),
    se = map(esc_output, "se")
  ) %>%
  select(-esc_output) %>%
  unnest(c(data, smd, se)) %>%
  ungroup()

## TEST: No study has FU less than 6 or 12
ttfu_test <- data_cont %>%
  group_by(studlab, outcome) %>%
  mutate(bin_ttfu = case_when(
    max(follow_up) >= 12 ~ 1,
    between(max(follow_up), 6, 11.999) ~ 2,
    TRUE ~ 3
  )) %>%
  select(bin_ttfu)

expect_true(all(ttfu_test$bin_ttfu == 1))

# CREATE BINARYS AND DF FOR EACH OUTCOME TYPE -------------------------

## OBS - BEHØVER IKKE OUTCOME MED!
data_qol <- data_cont %>%
  filter(outcome %in% qol_outcomes) %>%
  make_binary() %>%
  select(studlab, interv, outcome, follow_up, smd, se, starts_with("bin_")) %>%
  data.table()

data_func <- data_cont %>%
  filter(outcome %in% c("cs", prom_outcomes)) %>%
  make_binary() %>%
  select(studlab, interv, outcome, follow_up, smd, se, starts_with("bin_")) %>%
  data.table()

# Create selection grids  ---------------------------------------------------

print("Starting QoL selection grid")
tictoc::tic()
sel_grid_qol <- data_qol %>%
  make_sel_grid(outcome_type = "qol")
tictoc::toc()


print("Starting functional outcome selection grid")
tictoc::tic()
sel_grid_func <- data_func %>%
  make_sel_grid(outcome_type = "func")
tictoc::toc()


# Subset data -------------------------------------------------------------

##### QoL
print("Starting subset of QoL")
tictoc::tic()
subsets_qol <- sel_grid_qol %>%
  future_pmap(
    .l = .,
    .f = do_subset,
    df = data_qol
  ) %>%
  set_names(seq_along(.)) %>%
  discard(~ is.null(.x))
tictoc::toc()

write_rds(subsets_qol, here::here("output", "subsets_qol.rds"))


print("Starting subset of functional outcome")
tictoc::tic()
subsets_func <- sel_grid_func %>%
  future_pmap(
    .l = .,
    .f = do_subset,
    df = data_func
  ) %>%
  set_names(seq_along(.)) %>%
  discard(~ is.null(.x))
tictoc::toc()

write_rds(subsets_func, here::here("output", "subsets_func.rds"))


###### split multiple outcome dfs

# QoL

tictoc::tic()
multi_out_qol_splits <- subsets_qol %>%
  keep(~ any(duplicated(.x[["studlab"]]))) %>%
  future_map(split_multi_outc)
tictoc::toc()

## Functional outcome

tictoc::tic()
multi_out_func_splits <- subsets_func %>%
  keep(~ any(duplicated(.x[["studlab"]]))) %>%
  future_map(split_multi_outc)
tictoc::toc()


# Concatenate list of split dfs with original list
subsets_qol_final <- c(
  subsets_qol %>%
    discard(~ any(duplicated(.x[["studlab"]]))),
  multi_out_qol_splits %>% flatten()
)

subsets_func_final <- c(
  subsets_func %>%
    discard(~ any(duplicated(.x[["studlab"]]))),
  multi_out_qol_splits %>% flatten()
)

# Conduct metagen ---------------------------------------------------------
### NÅET HERTIL!


results_cs <- subset_cs_final %>%
  future_map(~ do_metagen(.x),
    .options = future_options(packages = c("meta", "stringr"))
  )
plan(sequential)

plan(multiprocess, workers = 6)
results_prom <- subset_prom_final %>%
  future_map(~ do_metagen(.x),
    .options = future_options(packages = c("meta", "stringr"))
  )
plan(sequential)

plan(multiprocess, workers = 6)
results_qol <- subset_qol_final %>%
  future_map(~ do_metagen(.x),
    .options = future_options(packages = c("meta", "stringr"))
  )
plan(sequential)



# extract results

results_cs_final <- results_cs %>% extract_metagen()
results_prom_final <- results_prom %>% extract_metagen()
results_qol_final <- results_qol %>% extract_metagen()

## gather pvals

pvals_cs <- results_cs_final %>% gather_pvals()
pvals_prom <- results_prom_final %>% gather_pvals()
pvals_qol <- results_qol_final %>% gather_pvals()




# GRAPHING ----------------------------------------------------------------



# geom point


### PROMS
pvals_prom %>% ggplot(aes(x = estimate, y = pval, color = k)) +
  geom_point() +
  scale_color_viridis_c() +
  scale_y_log10() +
  geom_hline(yintercept = 0.05)

### QOL
pvals_qol %>% ggplot(aes(x = estimate, y = pval, color = k)) +
  geom_point() +
  scale_color_viridis_c() +
  scale_y_log10() +
  geom_hline(yintercept = 0.05)


# evt køre geom_pointdensity på alle for at vise hvor størstedelen af resultaterne ligger?
# pval_gathered %>%  ggplot(aes(x = estimate, y = pval)) +
#  geom_pointdensity() +
#  scale_color_viridis_c() +
#  geom_hline( yintercept = 0.05) +
#  scale_y_log10()