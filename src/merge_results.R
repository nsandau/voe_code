library(arrow)
library(tictoc)
library(here)
library(magrittr)
library(furrr)
library(stringr)
tic("Writing merged results")
output_dir <- here("output")

df_paths <- output_dir %>%
    list.files()

for (OUTCOME in c("bin", "func")) {
    pvals_merged <- df_paths %>%
        str_subset(OUTCOME) %>%
        str_subset("pvals_") %>%
        future_map_dfr(~ read_feather(file.path(output_dir, .x)))

    results_merged <- df_paths %>%
        str_subset(OUTCOME) %>%
        str_subset("results_") %>%
        future_map_dfr(~ read_feather(file.path(output_dir, .x)))

    write_feather(
        results_merged,
        here::here("output", str_c("results_", OUTCOME, "_", "merged", ".feather"))
    )
    write_feather(
        pvals_merged,
        here::here("output", str_c("pvals_", OUTCOME, "_", "merged", ".feather"))
    )
}

toc()