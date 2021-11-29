library(argparser)

p <- arg_parser("metagen")

p <- add_argument(p, "outcome", help = "define outcome", type = "character")
p <- add_argument(p, "protocol", help = "define protocol", type = "character")
p <- add_argument(p, "n_splits", help = "No. of splits", type = "integer")
p <- add_argument(p, "split_no", help = "Split no for this run", type = "integer")
p <- add_argument(p, "--dev_run", help = "conduct dev_run", flag = T)

p <- add_argument(p, "--most_disc", help = "Output methodological choices for most discordant results", flag = T)

args <- parse_args(p)

# args <- list(outcome = "qol", protocol = "handoll", "n_splits" = 1, "split_no" = 1, dev_run = FALSE, most_disc = FALSE)

OUTCOME <- args$outcome
testthat::expect_true(OUTCOME %in% c("qol", "func", "bin"))

PROTOCOL <- args$protocol
testthat::expect_true(PROTOCOL %in% c("handoll", "beks", "skou", "none", "rct", "skou_rct"))

N_SPLITS <- args$n_splits

SPLIT_NO <- args$split_no
testthat::expect_true(SPLIT_NO %in% as.character(1:N_SPLITS))

DEV_RUN <- args$dev_run

MOST_DISC <- args$most_disc

##### LIBRARIES

pkgs <- c(
  "esc",
  "conflicted",
  "janitor",
  "furrr",
  "testthat",
  "readxl",
  "here",
  "lubridate",
  "tictoc",
  "data.table",
  "arrow",
  "lobstr",
  "meta",
  "tidyverse",
  "tidytable",
  "rrapply"
)
purrr::walk(pkgs, ~ library(.x, character.only = T, quietly = T))
tic("Total runtime:")

# import functions
source(here("src", "functions.R"))

# PRINT INFO
cores <- future::availableCores()
# ram <- benchmarkme::get_ram()
print(R.version)
cat("Outcome is:", OUTCOME, "\n")
cat("PROTOCOL IS:", PROTOCOL, "\n")
cat("DEV_RUN:", DEV_RUN, "\n")
cat("N_SPLITS:", N_SPLITS, "\n")
cat("SPLIT_NO:", SPLIT_NO, "\n")
cat("Using", cores, "cores", "\n")
# cat("Using", cores, "cores, and", round(ram / 1024 / 1024 / 1024), "gb ram", "\n")

conflict_prefer("filter", "dplyr", quiet = TRUE)
conflict_prefer("between", "dplyr", quiet = TRUE)
conflict_prefer("year", "lubridate", quiet = TRUE)
conflict_prefer("filter", "dplyr", quiet = T)

# IMPORT ------------------------------------------------------------------

# if running on local import directly from xlsx else import from saved RDS

if (cores < 20) {
  read_excel("/home/nicolai/OneDrive/Forskning/Phd/3 Simulation/02 - Data extract/outcome extract/p3 data extract.xlsx") %>%
    clean_names() %>%
    mutate(
      across(where(is.character), str_to_lower),
      publ_date = as_date(publ_date)
    ) %>%
    rename(studlab = study_identifier) %>%
    select(-notes) %>%
    write_rds("data/data_extract.rd") # named rd so gitignore doesnt catch it and the other rds files doesnt get uploaded
}

data_extract <- read_rds(here("data", "data_extract.rd"))

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
  #  "rct",
  #  "instability",
  #  "malunion",
  #  "stiffness"
  "revision",
  "complications",
  "displacement",
  # "impingement",
  "failure",
  "metalwork",
  "nerveinj",
  "infection",
  "avn",
  "nonunion"
)

# FOR NOW REMOVE BIN OUTCOMES RCT, INSTABILITY, MALUNION, STIFFNESS, IMPINGEMENT
data_extract <- data_extract %>%
  select(-starts_with(
    c("rct", "instability", "malunion", "stiffness", "impingement")
  ))


cont_outcomes <- c(func_outcomes, qol_outcomes)

OUTCOME_VARS <- if (OUTCOME == "qol") {
  qol_outcomes
} else if (OUTCOME == "func") {
  func_outcomes
} else if (OUTCOME == "bin") {
  bin_outcomes
}

OUTCOME_DESELECT <- setdiff(c(func_outcomes, qol_outcomes, bin_outcomes), OUTCOME_VARS)


# REMOVE COLS WITH ZERO VARIANCE--------------------------------------
# char_col <- data_extract %>%
#  select(-starts_with(cont_outcomes), -starts_with(bin_outcomes)) %>%
#  names()

# zv_cols <- data_extract %>%
#  select(all_of(char_col)) %>%
#  map_df(~ tibble(
#    class = class(.),
#    n_dist = n_distinct(.),
#    value = toString(unique(.)),
#  ), .id = "col_name") %>%
#  filter(n_dist == 1) %>%
#  pull(col_name)

# data_extract <- data_extract %>% select(-all_of(zv_cols))

# CALCULATE TOTAL N AND e --------------------------------
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
    ), by = "studlab") %>%
  select(-c(n, lost_to_fu), -starts_with("neer"))

# TEST: NO STUDY HAS < 15 samples
expect_true(all(data_extract$total_n - replace_na(data_extract$total_loss_to_fu, 0) > 15))

# RESHAPE DF --------------------------------------------------------------
cat("Reshaping DF", "\n")

data_cont <- data_extract %>%
  select(
    -starts_with(all_of(OUTCOME_DESELECT))
  ) %>%
  pivot_longer(
    cols = starts_with(all_of(OUTCOME_VARS)),
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
    ),
    by = "studlab"
  ) %>%
  # correct imputed outcome to specific measures and timepoints
  mutate(
    imputed_outcome = case_when(
      str_detect(imputed_vars, outcome) & follow_up == imputed_timepoint ~ imputed_outcome,
      TRUE ~ "no"
    ),
    follow_up = as.integer(follow_up),
    outcome = as_factor(outcome),
    interv = as_factor(interv)
  )

### TESTS ###
testthat::expect_setequal(data_cont %>% filter(is.na(con_n)) %>% nrow(), 0)
testthat::expect_setequal(data_cont %>% filter(is.na(int_n)) %>% nrow(), 0)
####

# REVERSE DASH NUMERIC OUTCOMES--------------------------------------

if (OUTCOME == "func") {
  data_cont <- data_cont %>%
    mutate(
      int_mean = if_else(outcome == "dash", 100 - int_mean, int_mean),
      con_mean = if_else(outcome == "dash", 100 - con_mean, con_mean),
      int_mean = if_else(outcome == "qdash", 100 - int_mean, int_mean),
      con_mean = if_else(outcome == "qdash", 100 - con_mean, con_mean)
    )
}

# CALCULATE SMD for NUMERIC OUTCOMES-----------------------------
cat("Calculating SMD", "\n")

if (OUTCOME %in% c("qol", "func")) {
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
}

# CREATE BINARYS AND DF FOR EACH OUTCOME TYPE -------------------------
cat("Creating binarys", "\n")
if (OUTCOME %in% c("qol", "func")) {
  EFFECT_SIZE_COLS <- c("smd", "se")
} else if (OUTCOME == "bin") {
  EFFECT_SIZE_COLS <- c("int_e", "int_n", "con_e", "con_n")
}

data <- data_cont %>%
  filter(outcome %in% OUTCOME_VARS) %>%
  make_binary(outcome = OUTCOME, protocol = PROTOCOL) %>%
  select(studlab, interv, outcome, follow_up, all_of(EFFECT_SIZE_COLS), starts_with("bin_")) %>%
  as_tidytable()

# Create selection grids  ---------------------------------------------------

if (DEV_RUN == TRUE) {
  data <- data %>%
    slice_sample.(prop = 0.3) %>%
    as.data.table()
}

cat("Creating selection grid ", "\n")
cat("Mem usage:", mem_used() / 1024 / 1024, "mb", "\n")
tictoc::tic("Selection grid")
sel_grid <- data %>%
  make_sel_grid(outcome_type = OUTCOME, protocol = PROTOCOL) %>%
  mutate.(
    row_id = row_number.()
  )
tictoc::toc()
cat("Length of sel_grid before split: ", nrow(sel_grid), "\n")
cat("Mem usage:", mem_used() / 1024 / 1024, "mb", "\n")

if (N_SPLITS > 1) {
  cat("Splitting sel_grid", "\n")
  part <- floor(nrow(sel_grid) / N_SPLITS)
  start <- ((SPLIT_NO - 1) * part + 1)
  stop <- (SPLIT_NO * part)
  if (SPLIT_NO == N_SPLITS) {
    stop <- nrow(sel_grid)
  }
  sel_grid <- sel_grid[start:stop, ]
}
cat("Length of sel_grid after split: ", nrow(sel_grid), "\n")
cat("Mem usage:", mem_used() / 1024 / 1024, "mb", "\n")


### Subset sel_grid for most disc
if (MOST_DISC) {
  results_merged <- read_feather("output/results_merged.feather")
  most_disc <- list()

  results <- results_merged %>%
    filter(k > 1) %>%
    gather_pvals() %>%
    drop_na() %>%
    filter(outcome == OUTCOME, protocol == PROTOCOL, pval < 0.05) %>%
    filter(estimate == max(estimate) | estimate == min(estimate)) %>%
    arrange(iteration)

  most_disc[["results"]] <- results

  idx <- results %>%
    distinct(iteration) %>%
    pull(iteration) %>%
    str_split("_") %>%
    map(~ .x[[1]]) %>%
    unlist() %>%
    as.integer()

  sel_grid <- sel_grid %>%
    filter(row_id %in% idx)

  most_disc[["sel_grid"]] <- sel_grid

  if (nrow(sel_grid) == 0) {
    cat("No rows in this split")
    quit(save = "no")
  }
}


# remove nulls
combs <- list(
  c("follow_up", "outcome"),
  c("intervention", "outcome"),
  c("intervention", "follow_up")
)

nrow_before <- nrow(sel_grid)
cat("Rows before removal of nulls:", nrow_before, "\n")

tic("Remove nulls")
sel_grid <- remove_nulls(df = data, grid = sel_grid, list_of_combos = combs)
toc()

nrow_after <- nrow(sel_grid)
cat("Rows after removal of nulls:", nrow_after, "\n")
nrow_diff <- nrow_before - nrow_after
cat("Rows removed:", nrow_diff, (nrow_after / nrow_before) * 100, "%", "\n")


# tic("Writing sel_grid")
# write_feather(sel_grid, here::here("output", str_c("sel_grid_", OUTCOME, ".feather")))
# toc()

# Subset data -------------------------------------------------------------
cat("Starting subsets", "\n")
if (DEV_RUN == TRUE) {
  sel_grid <- sel_grid %>%
    slice_head.(n = 50000)
}

# reduce cores if sel_grid is too large
mem_usage <- mem_used() / 1024 / 1024

if (mem_usage > 8000) {
  cat("Reducing no of cores", "\n")
  cores <- floor(0.8 * cores)
}

plan(multicore, workers = cores)
tic("Subsets")
subsets <- sel_grid %>%
  future_pmap(
    .l = .,
    .f = do_subset,
    df = data
  ) %>%
  set_names(sel_grid$row_id) %>%
  discard(~ is.null(.x))
plan(sequential)
toc()
cat("Length of subsets:", length(subsets), "\n")
cat("Mem usage:", mem_used() / 1024 / 1024, "mb", "\n")
# tic("Writing subsets")
# write_feather(data.table(subsets = subsets), here::here("output", str_c("subsets_", OUTCOME, ".feather")))
# toc()

###### flatten lists with multi_outcomes nested
tic("Flatten lists")
cat("Mem usage:", mem_used() / 1024 / 1024, "mb", "\n")
subsets <- rrapply(subsets, f = identity, classes = "data.frame", how = "flatten")
toc()
cat("Length of subsets after flatten:", length(subsets), "\n")
cat("Mem usage:", mem_used() / 1024 / 1024, "mb", "\n")

###### REMOVE SUBSETS WITH k = 1
tic("Remove k = 1 subsets")

subsets <- subsets %>% discard(~ nrow(.x) == 1)
toc()
cat("Length of subsets after removal of k = 1:", length(subsets), "\n")
cat("Mem usage:", mem_used() / 1024 / 1024, "mb", "\n")

# tic("Writing subsets_final")
# write_feather(data.table(subsets = subsets), here::here("output", str_c("subsets_final_", OUTCOME, ".feather")))
# toc()

## Save subsets to most_disc list if flag given

if (MOST_DISC) {
  most_disc[["subsets"]] <- subsets
}


# Conduct metagen ---------------------------------------------------------

tic("Meta-analysis")
plan(multicore, workers = 15)
results <- subsets %>%
  future_map(~ do_meta(.x, outcome = OUTCOME))
plan(sequential)
toc()

if (MOST_DISC) {
  most_disc[["ma"]] <- results
}

## EXTRACT RESULTS ---------------------------------------------------
results_df <- results %>%
  extract_meta(outcome = OUTCOME) %>%
  mutate(protocol = PROTOCOL, outcome = OUTCOME)

cat("Results_df rows", nrow(results_df), "\n")

### RECODE PROTOCOL SKOU ONLY USES RANDOM EFFECTS

if (PROTOCOL == "skou") {
  results_df <- results_df %>% select(-contains("fixed"))
}

if (MOST_DISC) {
  most_disc[["results_df"]] <- results_df
  most_disc %>% readr::write_rds(str_c("output/", "most_disc_", OUTCOME, "_", PROTOCOL, "_", SPLIT_NO, ".rds"))
} else {
  tic("Writing results ")
  write_feather(results_df, here("output", str_c("results_", OUTCOME, "_", SPLIT_NO, "_", PROTOCOL, ".feather")))
  toc()
}

# total runtime
toc()