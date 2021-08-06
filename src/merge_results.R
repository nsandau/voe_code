library(tidyverse)
library(arrow)
library(tictoc)
library(here)
library(furrr)

tic("Writing merged results")
output_dir <- here("output")

df_paths <- output_dir %>%
    list.files()

results_dfs <- df_paths %>%
    str_subset("results_") %>%
    str_subset("merged", negate = T)


cat("Files identified:", results_dfs, sep = " \n")

results_merged <- results_dfs %>%
    future_map_dfr(~ read_feather(file.path(output_dir, .x)))

write_feather(
    results_merged,
    here::here("output", str_c("results_", "merged", ".feather"))
)


# pvals_merged <- df_paths %>%
#     str_subset("pvals_") %>%
#     str_subset("merged", negate = T) %>%
#     future_map_dfr(~ read_feather(file.path(output_dir, .x)))

# write_feather(
#     pvals_merged,
#     here::here("output", str_c("pvals_", "merged", ".feather"))
# )

toc()