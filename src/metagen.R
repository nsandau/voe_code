### NOTER

## TODO:
## unit testing
## når laver imputation - i make_bin tage højde for hvilket outcome der er imputed
## I funktion der fjerner null int/out combs også fjerne for plate_tb og arthro??
## lave func der flytter filer ud til erda


## Udgået:
# zv: publ_status, databases, outcom_rep_as
# sample_n: none smaller than min requirement (15)
# time to follow-up: ingen under 12 mdr

# ONLY ERDA ---------------------------------------------------------------

lib <- .libPaths("/home/kmd592_ku_dk/modi_mount/R_lib")
install.packages("pacman", repos = "https://cloud.r-project.org/", lib = lib[1])


##### LIBRARIES
pacman::p_load(
  esc,
  conflicted,
  janitor,
  furrr,
  testthat,
  readxl,
  here,
  lubridate,
  purrr,
  benchmarkme,
  tictoc,
  data.table,
  tidyverse,
  arrow,
  lobstr
)

# PRINT INFO
cores <- future::availableCores()
ram <- benchmarkme::get_ram()
cat("Using", cores, "cores, and", round(ram / 1024 / 1024 / 1024), "gb ram", "\n")
cat("Current dir:", getwd(), "\n")

# settings
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

data_qol <- data_cont %>%
  filter(outcome %in% qol_outcomes) %>%
  make_binary() %>%
  select(studlab, interv, outcome, follow_up, smd, se, starts_with("bin_")) %>%
  as.data.table()

data_func <- data_cont %>%
  filter(outcome %in% c("cs", prom_outcomes)) %>%
  make_binary() %>%
  select(studlab, interv, outcome, follow_up, smd, se, starts_with("bin_")) %>%
  as.data.table()


# Create selection grids  ---------------------------------------------------


tictoc::tic("QoL selection grid")
sel_grid_qol <- data_qol %>%
  make_sel_grid(outcome_type = "qol")
tictoc::toc()
cat("Mem usage:", mem_used() / 1024 / 1024)

tic("Writing sel_grid_qol")
write_feather(sel_grid_qol, "output/sel_grid_qol.feather")
toc()

tictoc::tic("Functional outcome selection grid")
sel_grid_func <- data_func %>%
  make_sel_grid(outcome_type = "func")
tictoc::toc()
cat("Mem usage:", mem_used() / 1024 / 1024)



tic("Writing sel_grid_func")
write_feather(sel_grid_func, "output/sel_grid_func.feather")
toc()

# Subset data -------------------------------------------------------------

##### QoL
tic("Subset of QoL")
subsets_qol <- sel_grid_qol %>%
  future_pmap(
    .l = .,
    .f = do_subset,
    df = data_qol
  ) %>%
  set_names(seq_along(.)) %>%
  discard(~ is.null(.x))
toc()
cat("Mem usage:", mem_used() / 1024 / 1024)

# tic("Writing QOL subsets")
# write_rds(subsets_qol, here::here("output", "subsets_qol.rds"))
# toc()

#### FUNCTIONAL OUTCOME
tic("Subset of functional outcome")
subsets_func <- sel_grid_func %>%
  future_pmap(
    .l = .,
    .f = do_subset,
    df = data_func
  ) %>%
  set_names(seq_along(.)) %>%
  discard(~ is.null(.x))
toc()
cat("Mem usage:", mem_used() / 1024 / 1024)

# tic("Writing FUNC subsets")
# write_rds(subsets_func, here::here("output", "subsets_func.rds"))
# toc()

###### split multiple outcome dfs

## QOL
tictoc::tic("Multi outcome split QOL")
multi_out_qol_splits <- subsets_qol %>%
  keep(~ any(duplicated(.x[["studlab"]]))) %>%
  future_map(split_multi_outc)

# concat lsits
subsets_qol <- c(
  subsets_qol %>%
    discard(~ any(duplicated(.x[["studlab"]]))),
  multi_out_qol_splits %>% flatten()
)
tictoc::toc()

rm(multi_out_qol_splits)

cat("Mem usage:", mem_used() / 1024 / 1024)


# tic("Writing subsets_qol_final")
# write_rds(subsets_func, here::here("output", "subsets_qol.rds"))
# toc()

## FUNCTIONAL OUTCOME

tictoc::tic("Multi outcome split func")
multi_out_func_splits <- subsets_func %>%
  keep(~ any(duplicated(.x[["studlab"]]))) %>%
  future_map(split_multi_outc)

subsets_func <- c(
  subsets_func %>%
    discard(~ any(duplicated(.x[["studlab"]]))),
  multi_out_qol_splits %>% flatten()
)
tictoc::toc()
rm(multi_out_func_splits)
cat("Mem usage:", mem_used() / 1024 / 1024)

# tic("Writing subsets_func")
# write_rds(subsets_func, here::here("output", "subsets_func.rds"))
# toc()


# Conduct metagen ---------------------------------------------------------

# QOL
results_qol <- subsets_qol %>%
  future_map(~ do_metagen(.x))

results_qol_df <- results_qol %>% extract_metagen()
pvals_qol <- results_qol_df %>% gather_pvals()

tic("Writing results QOL")
write_feather(results_qol_df, here::here("output", "results_qol_df.feather"))
write_feather(pvals_qol, here::here("output", "pvals_qol.feather"))
toc()


# FUNCTIONAL OUTCOME
results_func <- subsets_func %>%
  future_map(~ do_metagen(.x))

results_func_df <- results_func %>% extract_metagen()
pvals_func <- results_func_df %>% gather_pvals()

tic("Writing results FUNC")
write_feather(results_func_df, here::here("output", "results_func_df.feather"))
write_feather(pvals_func, here::here("output", "pvals_func.feather"))
toc()


# COPY FILES TO erda storage