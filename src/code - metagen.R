# LIBRARIES ---------------------------------------------------------------
library(meta)
library(esc)
library(readxl)
library(conflicted)
library(janitor)
library(furrr)
library(testthat)
library(data.table)
library(tidyverse)


#settings

conflict_prefer("filter", "dplyr")
conflict_prefer("between", "dplyr")


#import functions 
source("functions.R")

# IMPORT ------------------------------------------------------------------
data_extract <- read_excel("C:/Data/OneDrive/Forskning/phd/3 Simulation/02 - Data extract/nop v op.xlsx") %>%
  clean_names() %>%
  mutate_if(is.character, str_to_lower) %>% 
  rename(studlab = study_identifier)


#temp fix indtil jeg får udfyldt db

data_extract <- data_extract %>%  
  mutate_if(is.logical, as.character) %>% 
  mutate( databases = "pubmed")

# STRINGS FOR FILTERING ---------------------------------------------------
prom_outcomes <- c("oss", "ases", "qdash", "dash", "sst")
qol_outcomes <- c("euroqol", "qol15d", "sf36pc", "sf36", "vr12", "sf12pc", "sf12")
cont_outcomes <- c(prom_outcomes, "cs", qol_outcomes)
bin_outcomes <- c("revision", "compl")
nrsi_studies <- data_extract %>%
  filter(design == "nrsi") %>%
  pull(studlab) %>%
  unique()

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
  mutate(
    follow_up = as.numeric(follow_up)
  ) %>% 
  left_join(
    (data_extract %>%
       select(studlab,
              intervention_type) %>%
       filter(intervention_type != "nonop") %>% 
       rename(interv = intervention_type)
     )
   ) 


# REVERSE DASH AND CALCULATE SMD  ----------------------------------------------------------------

data_cont <- data_cont  %>% 
  mutate(
    int_mean = if_else(outcome == "dash", 100 - int_mean, int_mean),
    con_mean = if_else(outcome == "dash", 100 - con_mean, con_mean),
    int_mean = if_else(outcome == "qdash", 100 - int_mean, int_mean),
    con_mean = if_else(outcome == "qdash", 100 - con_mean, con_mean)
  ) %>%
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


# CREATE BINARYS AND DF FOR EACH OUTCOME TYPE -----------------------------------------

data_cont_bin <- data_cont %>% make_binary(type = "ma")

#### tests

expect_equal(nrow(data_cont),nrow(drop_na(data_cont %>% select(starts_with("bin_")))))
expect_equal(nrow(data_cont), nrow(data_cont_bin))

####

data_cs <- data_cont_bin %>%
  filter(outcome == "cs") %>%
  select(studlab, follow_up, smd, se, starts_with("bin_")) %>% 
  as.data.table()

data_prom <- data_cont_bin %>%
  filter(outcome %in% prom_outcomes) %>%
  select(studlab, follow_up, smd, se, starts_with("bin_")) %>% 
  as.data.table()

data_qol <- data_cont_bin %>%
  filter(outcome %in% qol_outcomes) %>%
  select(studlab, follow_up, smd, se, starts_with("bin_")) %>% 
  as.data.table()


######## TESTS
#Testing that no studies have multiple outcomes
expect_equal(data_cs %>% multi_outc_ma() %>% filter(multi_out != 1) %>% nrow(), 0)
expect_equal(data_prom %>% multi_outc_ma() %>% filter(multi_out != 1) %>% nrow(), 0)
expect_equal(data_qol %>% multi_outc_ma() %>% filter(multi_out != 1) %>% nrow(), 0)


# Create selection grids  ---------------------------------------------------

grid_cs <- sel_grid(data_cs, type = "ma")
grid_prom <- sel_grid(data_prom, type = "ma")
grid_qol <- sel_grid(data_qol, type = "ma")


# Subset data -------------------------------------------------------------


##### CS
plan(multiprocess, workers = 6) 
subset_cs <- grid_cs %>%
  future_pmap(do_subset,
              data = data_cs,
              type = "ma",
              nrsi_studies = nrsi_studies,
              .options = future_options(packages = c("data.table"))
  ) %>%
  set_names(seq_along(1:length(.)))
plan(sequential)


subset_cs_final <- subset_cs[!duplicated(subset_cs)] %>%
  discard(~ is.null(.x))

##### PROMS

plan(multiprocess, workers = 6) 
subset_prom <- grid_prom %>%
  future_pmap(do_subset,
              data = data_prom,
              type = "ma",
              nrsi_studies = nrsi_studies,
              .options = future_options(packages = c("data.table"))
  ) %>%
  set_names(seq_along(1:length(.)))
plan(sequential)

subset_prom_final <- subset_prom[!duplicated(subset_prom)] %>%
  discard(~ is.null(.x)) 

##### QOL


plan(multiprocess, workers = 6) 
subset_qol <- grid_qol %>% 
  future_pmap(do_subset, 
              data= data_qol,
              type = "ma",
              nrsi_studies = nrsi_studies,
              .options = future_options(packages = c("data.table"))
              ) %>% 
  set_names(seq_along(1:length(.))) 
plan(sequential)


subset_qol_final <- subset_qol[!duplicated(subset_qol)] %>%
  discard(~ is.null(.x)) 

##### TESTS
expect_equal(nrow(grid_cs), length(subset_cs))
expect_equal(nrow(grid_prom), length(subset_prom))
expect_equal(nrow(grid_qol), length(subset_qol))


subset_test <- subset_qol_final %>% keep( ~any(.x$studlab == "roberson 2017" ))


# Conduct metagen ---------------------------------------------------------


plan(multiprocess, workers = 6)
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

results_cs_final <- results_cs %>%  extract_metagen()
results_prom_final <- results_prom %>% extract_metagen()
results_qol_final <- results_qol %>% extract_metagen()

## gather pvals

pvals_cs <- results_cs_final %>% gather_pvals()
pvals_prom <- results_prom_final %>% gather_pvals()
pvals_qol <- results_qol_final %>% gather_pvals()




# GRAPHING ----------------------------------------------------------------



#geom point


### PROMS
pvals_prom %>%  ggplot(aes(x = estimate, y = pval, color = k)) +
  geom_point() +
  scale_color_viridis_c() + 
  scale_y_log10() +
  geom_hline( yintercept = 0.05)

### QOL
pvals_qol  %>%  ggplot(aes(x = estimate, y = pval, color = k)) +
  geom_point() +
  scale_color_viridis_c() + 
  scale_y_log10() +
  geom_hline( yintercept = 0.05)


# evt køre geom_pointdensity på alle for at vise hvor størstedelen af resultaterne ligger?
#pval_gathered %>%  ggplot(aes(x = estimate, y = pval)) +
#  geom_pointdensity() + 
#  scale_color_viridis_c() +
#  geom_hline( yintercept = 0.05) + 
#  scale_y_log10() 



