pkgs <- c(
    "tidyverse",
    "arrow",
    "tictoc",
    "here",
    "furrr",
    "tidytable",
    "data.table"
)

purrr::walk(pkgs, ~ library(.x, character.only = T, quietly = T))

tic("Writing merged results")
output_dir <- here("output")

df_paths <- output_dir %>%
    list.files()

results_dfs <- df_paths %>%
    str_subset("results_") %>%
    str_subset("merged", negate = T)
# %>% str_subset("handoll")

cat("Files identified:", results_dfs, sep = " \n")

n_dfs <- floor(length(results_dfs) / 2)

plan(multicore, workers = n_dfs)
results_merged <- results_dfs %>%
    future_map(
        ~ read_feather(file.path(output_dir, .x), as_data_frame = F) %>%
            as.data.table()
    ) %>%
    rbindlist(fill = T)
plan(sequential)

## n_simulations

n_simulations <- results_merged %>%
    count.(protocol, outcome)

write_feather(
    n_simulations,
    here::here("output", str_c("n_", "simulations", ".feather"))
)

## Most discordant

# results_merged %>%
#     filter.(
#         pval.fixed < 0.05 | pval.random < 0.05,
#         .by = c(protocol, outcome)
#     ) %>%
#     pivot_longer.(cols = c(te.fixed, te.random), values_to = "estimates", names_to = "method") %>%
#     filter.(estimates == max(estimates) | estimates == min(estimates), .by = c(protocol, outcome)) %>%
#     slice_sample(n = 100) %>%
#     filter.(k > 1) %>%
#     View()



## Merge distinct results
results_merged() <- results_merged %>%
    distinct.(-starts_with("iteration"), .keep_all = T)

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