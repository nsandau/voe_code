
library(netmeta)
library(readxl)
library(conflicted)
library(janitor)
library(furrr)
library(testthat)
library(data.table)
library(tidyverse)

#Settings

conflict_prefer("filter", "dplyr")
conflict_prefer("between", "dplyr")

source("functions.R")

# IMPORT ------------------------------------------------------------------


nop_v_op <- read_excel("C:/Data/OneDrive/Forskning/phd/3 Simulation/02 - Data extract/nop v op.xlsx") %>%
  clean_names() %>%
  mutate_if(is.character, str_to_lower) 


kir_v_kir <- read_excel("C:/Data/OneDrive/Forskning/phd/3 Simulation/02 - Data extract/kir v kir.xlsx") %>%
  clean_names() %>%
  mutate_if(is.character, str_to_lower)


data_nma <- full_join(kir_v_kir, nop_v_op)


# remove studies with mixed interventions 

mix_interv_all <- data_nma %>%
  filter(intervention_type == "mixed") %>%
  pull(study_identifier)

mix_interv_keep_nonop <- kir_v_kir %>%
  filter(study_identifier %in% mix_interv_all) %>%
  pull(study_identifier) %>%
  unique()

mix_interv_remove <- setdiff(mix_interv_all, mix_interv_keep_nonop)

data_nma <- data_nma %>% filter(!study_identifier %in% mix_interv_remove,
                                !(study_identifier %in% mix_interv_keep_nonop & intervention_type == "mixed"))

# temp fix indtil jeg får udfyldt db
data_nma <- data_nma %>%
  mutate_if(is.logical, as.character) %>%
  mutate(
    databases = "pubmed"
  )


# STRINGS FOR FILTERING ---------------------------------------------------

prom_outcomes <- c("oss", "ases", "qdash", "dash", "sst")
qol_outcomes <- c("euroqol", "qol15d", "sf36pc", "sf36", "vr12", "sf12pc", "sf12")
cont_outcomes <- c(prom_outcomes, "cs", qol_outcomes)
bin_outcomes <- c("revision", "compl")
nrsi_studies_nma <- data_nma %>%
  filter(design == "nrsi") %>%
  pull(study_identifier) %>%
  unique()


# PREPARE DF---------------------------------------------------------------

data_nma <- data_nma %>%
  select(
    -contains(bin_outcomes) # deselect columns with binary outcome  change when final dataset
  ) %>% 
  pivot_longer(
    cols = starts_with(all_of(cont_outcomes)),
    names_to = c("outcome", "follow_up", "type"),
    names_sep = "_",
    values_drop_na = T
  ) %>% 
  pivot_wider(
    names_from = type,
    values_from = value
  ) %>% mutate(
    follow_up = as.integer(follow_up)
  ) 

# Reverse DASH
data_nma <- data_nma %>%  mutate(
  mean = case_when(
    outcome == "dash" ~ 100 - mean,
    outcome == "qdash" ~ 100 - mean,
    TRUE ~ mean)
) 




# # BEREGN SMD OG transform to contrast based df --------------------------


data_nma <- data_nma %>%
   group_by(outcome, follow_up ) %>%
  nest() %>%
  mutate(
    pairwise = map(data, ~pairwise(
                         treat = .x$intervention_type,
                         n = .x$n,
                         mean = .x$mean,
                         sd = .x$sd,
                         studlab = .x$study_identifier,
                         data = .x,
                         sm = "SMD"
                       )),
  pairwise = map(pairwise, as_tibble)                 
  )  %>% 
  select(pairwise, outcome, follow_up, -data) %>% 
  unnest(pairwise) %>% 
  ungroup() %>% 
  select( -study_identifier) %>% 
  arrange(studlab)


# # make binaries ---------------------------------------------------------


data_nma_bin <- data_nma %>% make_binary(type = "nma")

#### tests

expect_equal(nrow(data_nma),nrow(drop_na(data_nma %>% select(starts_with("bin_")))))
expect_equal(nrow(data_nma), nrow(data_nma_bin))

####


# Select outcome and prepare df -------------------------------------------

data_nma_cs <- data_nma_bin %>%
  filter(outcome == "cs") %>% 
  select(studlab, TE, seTE, treat1, treat2, follow_up, starts_with("bin_")) %>%
  as.data.table()

data_nma_prom <- data_nma_bin %>%
  filter(outcome %in% prom_outcomes) %>% 
  multi_outc_nma() %>% 
  select(studlab, TE, seTE, treat1, treat2, follow_up, starts_with("bin_")) %>%
  as.data.table()

data_nma_qol <- data_nma_bin %>%
  filter(outcome %in% qol_outcomes) %>%
  multi_outc_nma() %>%
  select(studlab, TE, seTE, treat1, treat2, follow_up, starts_with("bin_")) %>%
  as.data.table()

# create selection grid -------------------------------------------------------------

grid_nma_cs <- data_nma_cs %>% sel_grid(type = "nma")
grid_nma_prom <- data_nma_prom %>% sel_grid(type = "nma") 
grid_nma_qol <- data_nma_qol %>% sel_grid(type = "nma")

# Subset data  --------------------------------------------------

##### CS
plan(multiprocess, workers = 6) 
subset_nma_cs <- grid_nma_cs %>%
  future_pmap(do_subset,
              type = "nma",
              data = data_nma_cs,
              nrsi_studies = nrsi_studies_nma,
              .options = future_options(packages = c("data.table"))
  ) %>%
  set_names(seq_along(1:length(.)))
plan(sequential)

subset_nma_cs_final <- subset_nma_cs[!duplicated(subset_nma_cs)]   %>% 
  discard(~is.null(.x)) 

##### PROMS


grid_nma_prom_head <- slice_head(grid_nma_prom, prop = 0.5)
grid_nma_prom_tail <- slice_tail(grid_nma_prom, prop = 0.5)

expect_equal(bind_rows(grid_nma_prom_head, grid_nma_prom_tail), grid_nma_prom)

plan(multiprocess, workers = 6) 

subset_nma_prom_head <- grid_nma_prom_head %>%
  future_pmap(do_subset,
              type = "nma",
              data = data_nma_prom,
              nrsi_studies = nrsi_studies_nma,
              .options = future_options(packages = c("data.table"))
  )
plan(sequential)

plan(multiprocess, workers = 6) 
subset_nma_prom_tail <- grid_nma_prom_tail %>%
  future_pmap(do_subset,
              type = "nma",
              data = data_nma_prom,
              nrsi_studies = nrsi_studies_nma,
              .options = future_options(packages = c("data.table"))
  )
plan(sequential)


subset_nma_prom <- append(subset_nma_prom_head, subset_nma_prom_tail) %>%
  set_names(seq_along(1:length(.)))


subset_nma_prom_final <- subset_nma_prom[!duplicated(subset_nma_prom)]  %>% 
  discard(~is.null(.x))


##### QOL

plan(multiprocess, workers = 6)
subset_nma_qol <- grid_nma_qol %>%
  future_pmap(do_subset,
              type = "nma",
              data = data_nma_qol,
              nrsi_studies = nrsi_studies_nma,
              .options = future_options(packages = c("data.table"))
  ) %>%
  set_names(seq_along(1:length(.)))
plan(sequential)

subset_nma_qol_final <- subset_nma_qol[!duplicated(subset_nma_qol)]  %>% 
  discard(~is.null(.x))

#### SAVE SUBSETS
## temp saving of subsets
#write_rds(subset_nma_prom_final, "subset_nma_prom_final")
subset_nma_prom_final <- read_rds("subset_nma_prom_final")


##### TESTS
expect_equal(nrow(grid_nma_cs), length(subset_nma_cs))
expect_equal(nrow(grid_nma_prom), length(subset_nma_prom))
expect_equal(nrow(grid_nma_qol), length(subset_nma_qol))

subset_test <- subset_final %>% keep( ~any(.x$studlab == "kollig 2003" ) ) 


# conduct  nma -------------------------------------------------------------

# import saved subsets
#subset_nma_prom_final <- read_rds("subset_nma_prom_final")


##### CS 
# identifying  subset with > 1 network
netconn_cs <- subset_nma_cs_final %>% identify_multi_networks()


plan(multiprocess, workers = 6) 
results_nma_cs <- subset_nma_cs_final[!names(subset_nma_cs_final) %in% netconn_cs] %>% 
  future_map(~ do_netmeta(.x),
             .options = future_options(packages = "netmeta")
  )
plan(sequential)  




##### PROMS
# identifying  subset with > 1 network
netconn_prom  <- subset_nma_prom_final %>% identify_multi_networks()
 
plan(multiprocess, workers = 6)
results_nma_prom <- subset_nma_prom_final[!names(subset_nma_prom_final) %in% netconn_prom] %>%
  future_map(~ do_netmeta(.x),
    .options = future_options(packages = "netmeta")
  )
plan(sequential)


##### QOL
# removing subset with > 1 network
netconn_qol <- subset_nma_qol_final %>%   identify_multi_networks()


plan(multiprocess, workers = 6) 
results_nma_qol <- subset_nma_qol_final[!names(subset_nma_qol_final) %in% netconn_qol] %>% 
  future_map(~ do_netmeta(.x),
             .options = future_options(packages = "netmeta")
  )
plan(sequential)  




# extract results ---------------------------------------------------------

results_nma_cs_final <- results_nma_cs %>% extract_netmeta()

results_nma_prom_final <- results_nma_prom %>% extract_netmeta()

results_nma_qol_final <- results_nma_qol %>% extract_netmeta()


# Gather unique random and fixed effect te and pvals


pvals_nma_cs <- results_nma_cs_final %>% gather_pvals_nma()

pvals_nma_prom <- results_nma_prom_final %>% gather_pvals_nma()

pvals_nma_qol <- results_nma_qol_final %>% gather_pvals_nma() 


# GRAPHING ----------------------------------------------------------------



#### PROMS

pvals_nma_prom %>% ggplot(aes(x = estimate, y = pval )) +
  geom_point(aes(color = treatment)) + 
  geom_hline(yintercept = 0.05, linetype = "dashed") + 
  scale_y_log10(labels = scales::label_number() ) 

### eksempel til fondsansøgning
pvals_nma_prom %>% ggplot(aes(x = estimate, y = pval )) +
  geom_point() + 
  geom_hline(yintercept = 0.05, linetype = "dashed") + 
  scale_y_log10(labels = scales::label_number() ) + 
  theme(legend.position = "none") + 
  labs(x = "Effekt estimat (SMD)", y = "P-værdi") +
  theme_bw()


#color: no studies included, facetwrap by treatment
pvals_nma_prom %>% ggplot(aes(x = estimate, y = pval )) +
  geom_point(aes(color = factor(k))) + 
  geom_hline(yintercept = 0.05) + 
  scale_y_log10() +
  facet_wrap(~treatment)



#### QOL
pvals_nma_qol %>% ggplot(aes(x = estimate, y = pval) ) +
  geom_point(aes(color = treatment)) + 
  geom_hline(yintercept = 0.05) + 
  scale_y_log10()
















