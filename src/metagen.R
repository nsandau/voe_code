### NOTER

## TODO:
## unit testing
## når laver imputation - i make_bin tage højde for hvilket outcome der er imputed
## I funktion der fjerner null int/out combs også fjerne for plate_tb og arthro??
## Fix paths when copying
## identify highest cardinality features and remove null combos

# DEN LAVER DEN HER ERROR PÅ ERDA
# Subsets: 4655.883 sec elapsed
# Mem usage: 10079.65 mb
# Error in mcfork(detached) :
#  unable to fork, possible reason: Cannot allocate memory
# Calls: %>% ... run.MulticoreFuture -> do.call -> <Anonymous> -> mcfork
# Execution halted
# det er nok fordi jeg inde i function laver split_dfs. Som så skal deles ud til alle workers?
# kan enten lave dopar inde i func eller splitte til dfs inden jeg kører future_map?

## Udgået:
# zv: publ_status, databases, outcom_rep_as
# sample_n: none smaller than min requirement (15)
# time to follow-up: ingen under 12 mdr

### ARGUMENTS
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("Need to supply outcome", call. = FALSE)
}
# define outcome
testthat::expect_true(args[1] %in% c("qol", "func"))
OUTCOME <- args[1]

# if second argument given: conduct dev-run
if (is.na(args[2])) {
  DEV_RUN <- FALSE
} else {
  DEV_RUN <- TRUE
}

BASE_PATH <- "/home/kmd592_ku_dk"
ERDA_PATH <- file.path(BASE_PATH, "erda_mount")
MODI_PATH <- file.path(BASE_PATH, "modi_mount")
DATE <- format(Sys.time(), "%d-%m-%y_%H-%M")
LIB_PATHS <- .libPaths(file.path(MODI_PATH, "R_lib"))

install.packages("pacman", repos = "https://cloud.r-project.org/", lib = LIB_PATHS[1])

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
  lobstr,
  meta
)

# PRINT INFO
cores <- future::availableCores()
ram <- benchmarkme::get_ram()
cat("Using", cores, "cores, and", round(ram / 1024 / 1024 / 1024), "gb ram", "\n")
cat("Current dir:", getwd(), "\n")

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

# write_rds(data_extract, here("data", data_extract.rds))

# STRINGS FOR FILTERING ---------------------------------------------------

func_outcomes <- c(
  "cs",
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

cont_outcomes <- c(func_outcomes, qol_outcomes)

OUTCOME_VARS <- if (OUTCOME == "qol") {
  qol_outcomes
} else if (OUTCOME == "func") {
  func_outcomes
}

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

data <- data_cont %>%
  filter(outcome %in% OUTCOME_VARS) %>%
  make_binary() %>%
  select(studlab, interv, outcome, follow_up, smd, se, starts_with("bin_")) %>%
  as.data.table()

# Create selection grids  ---------------------------------------------------

tictoc::tic("Selection grid")
sel_grid <- data %>%
  make_sel_grid(outcome_type = OUTCOME)
tictoc::toc()
cat("Mem usage:", mem_used() / 1024 / 1024, "mb", "\n")

tic("Writing sel_grid")
write_feather(sel_grid, here::here("output", paste0("sel_grid_", OUTCOME, ".feather")))
toc()

# Subset data -------------------------------------------------------------

if (DEV_RUN == TRUE) {
  sel_grid <- sel_grid %>% slice_sample(n = 20000)
}

plan(multicore, workers = cores)

tic("Subsets")
subsets <- sel_grid %>%
  future_pmap(
    .l = .,
    .f = do_subset,
    df = data
  ) %>%
  set_names(seq_along(.)) %>%
  discard(~ is.null(.x))
toc()
cat("Mem usage:", mem_used() / 1024 / 1024, "mb", "\n")

plan(sequential)

# tic("Writing QOL subsets")
# write_rds(subsets, here::here("output", paste0("subsets_", OUTCOME, ".rds")))
# toc()

###### split multiple outcome dfs

subsets_multi_outc <- subsets %>%
  keep(~ any(duplicated(.x[["studlab"]])))

plan(multicore, workers = cores)
tic("Multi outcome split ")
multi_out_splits <- subsets_multi_outc %>%
  future_map(split_multi_outc)
plan(sequential)
toc()
rm(subsets_multi_outc)
cat("Mem usage:", mem_used() / 1024 / 1024, "mb", "\n")


tic("Concat subsets")
# concat lists
subsets <- c(
  subsets %>%
    discard(~ any(duplicated(.x[["studlab"]]))),
  multi_out_splits %>% flatten()
)
toc()

rm(multi_out_splits)

cat("Mem usage:", mem_used() / 1024 / 1024, "mb", "\n")

# tic("Writing subsets_final")
# write_rds(subsets, here::here("output", paste0("subsets_final_", OUTCOME, ".rds")))
# toc()

# Conduct metagen ---------------------------------------------------------
tic("Metagen")
plan(multicore, workers = cores)
results <- subsets %>%
  future_map(~ do_metagen(.x))
plan(sequential)
toc()

results_df <- results %>% extract_metagen()
pvals <- results_df %>% gather_pvals()

tic("Writing results ")
write_feather(results_df, here::here("output", paste0("results_df_", OUTCOME, ".feather")))
write_feather(pvals, here::here("output", paste0("pvals_", OUTCOME, ".feather")))
toc()

# COPY FILES TO erda storage
from_path <- here::here("output")
to_path <- file.path(ERDA_PATH, "VOE_OUTPUT", OUTCOME, DATE)
dir.create(to_path, recursive = T)
file_paths <- list.files(from_path, ".rds$|.feather$") %>%
  str_subset(OUTCOME)
file.copy(file_paths, to_path)