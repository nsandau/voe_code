
df_paths %>%
    str_subset(OUTCOME) %>%
    str_subset("pvals_") %>%
    str_subset("merged", negate = T)


read_feather(here("output", "results_func_merged.feather"))

# if OUTCOME %in% c("func", "bin") & SPLIT_NO == N_SPLITS

output_dir <- here("output")

df_paths <- output_dir %>%
    list.files()

# skal lave en col der siger om det er fra provider eller ccg

pvals_merged <- df_paths %>%
    str_subset(OUTCOME) %>%
    str_subset("pvals_") %>%
    map_dfr(~ read_feather(file.path(output_dir, .x)))



results_merged <- df_paths %>%
    str_subset(OUTCOME) %>%
    str_subset("results_") %>%
    map_dfr(~ read_feather(file.path(output_dir, .x)))



write_parquet(pvals, here::here("output", str_c("pvals_", OUTCOME, "_", SPLIT_NO, ".parquet")), version = "2.0")

df

### test expand_grid.

df <- data

outcome_type <- "qol"

## FUNC START (DF, OUTCOME_TYPE)

if (outcome_type == "qol") {
    outcome_vals <- c(unique(df$bin_outcome), 98) # 98: QoLs
} else if (outcome_type == "func") {
    outcome_vals <- c(unique(df$bin_outcome), 98:99) # 98: PROMS + CS,99 PROMS
} else if (outcome_type == "bin") {
    outcome_vals <- unique(df$bin_outcome)
}

# neer
neer_vals <- unique(df$bin_neer)
neer_vals <- neer_vals[neer_vals %in% 2:4]

# outcome
interv_vals <- as.factor(c(levels(fct_drop(df$interv)), "plate_tb", "artro", "all"))

# create grid
grid <- expand_grid.(
    language = unique(df$bin_lang),
    year = unique(df$bin_year),
    design = unique(df$bin_design),
    age = unique(df$bin_age),
    neer_34part = unique(df$bin_34part),
    toi = unique(df$bin_toi),
    doctreat = unique(df$bin_doctreat),
    loss_fu = unique(df$bin_loss_fu),
    outcome = outcome_vals,
    follow_up = c(unique(df$follow_up), 98:99), # 98: longest fu from each study, 99: periods of fu
    fu_period = unique(df$bin_fu_period),
    outcome_analysis = c(unique(df$bin_oa), 98), # 98: both types of oa
    intervention = interv_vals,
    neer = c(neer_vals, 98, 99), # 98: 3-part + 4-part, 99 ALL
    imputed = unique(df$bin_imputed),
    ttfu = unique(df$bin_ttfu)
)


grid %>% mutate.(
    across.(where(is.numeric), as.integer),
    across.(where(is.character), as_factor)
)




### SPLIT SEL GRID
sel_grid <- read_parquet(here("output", "sel_grid_qol.parquet"))
dim(sel_grid)

OUTCOME <- "bin"
args <- c("bin", "1")

### ARGS
N_SPLITS <- 4

testthat::expect_true(args[2] %in% as.character(1:N_SPLITS))

SPLIT_NO <- as.integer(args[2])


# start split
if (OUTCOME %in% c("func", "bin")) {
    part <- floor(nrow(sel_grid) / N_SPLITS)

    start <- ((SPLIT_NO - 1) * part + 1)
    stop <- (SPLIT_NO * part)

    if (SPLIT_NO == N_SPLITS) {
        stop <- nrow(sel_grid)
    }

    sel_grid <- sel_grid[start:stop, ]
}

dim(sel_grid)



########

outcomes <- unique(df[duplicated(df[["studlab"]])][["outcome"]])
outcomes

df[]

loop_out <- list()
for (outc in outcomes) {
    prim_df <- df[outcome == outc]

    prim_studlabs <- unique(prim_df[["studlab"]])

    rest_df <- df %>%
        filter.(!studlab %in% prim_studlabs)

    if (!any(duplicated(rest_df$studlab))) {
        if (nrow(rest_df) > 0) {
            loop_out[[outc]] <- list(prim_df, rest_df)
        }
        else {
            loop_out[[outc]] <- list(prim_df)
        }
    } else {
        split_list <- split_multi_outc(rest_df)
        loop_out[[outc]] <- c(list(prim_df), flatten(split_list))
    }
}


identical(loop_out, loop_out_old)

bench::mark(
    df %>%
        filter.(!studlab %in% prim_studlabs),
    df[!studlab %in% prim_studlabs],
    check = F
)

df %>%
    filter.(!studlab %in% prim_studlabs)


df[!studlab %in% prim_studlabs]

prim_studlabs






df[, outcome := NULL]

df
df


df[-outcome]

df %>% filter.(outcome == "eq5d")

df["outcome" == "eq5d"]
x <- 1:2
y <- 1:2

expand_grid.(x, y)

expand_grid.(stuff = x, y)



df <- data
list_of_combos <- list(
    c("follow_up", "outcome"),
    c("intervention", "outcome"),
    c("intervention", "follow_up")
)


grid <- sel_grid


# helper func
helper <- function(var1, var2, val1, val2) {
    if (nrow(df[get(var1) == val1 & get(var2) == val2]) == 0) {
        grid <- grid[!(get(var1) == val1 & get(var2) == val2)]
    }
}


### START FUNC

df <- df %>%
    select.(-outcome) %>%
    rename.(
        outcome = bin_outcome,
        intervention = interv
    )

combo_grids <- list()
for (i in seq_along(list_of_combos)) {
    combo_grid <- expand_grid(
        var1 = list_of_combos[[i]][1],
        var2 = list_of_combos[[i]][2],
        val1 = as.numeric(unique(df[[var1]])),
        val2 = as.numeric(unique(df[[var2]]))
    )
    combo_grids[[i]] <- combo_grid
}

combo_grid <- rbindlist(combo_grids)

combo_grid %>%
    pwalk(helper)


df[get("follow_up") == 12]




remove_nulls <- function(df, grid, list_of_combos) {
    df <- df %>%
        select.(-outcome) %>% # ingen grund til at selecte?
        rename.(
            outcome = bin_outcome,
            intervention = interv
        )

    combo_grids <- list()
    for (i in seq_along(list_of_combos)) {
        combo_grid <- expand.grid(
            var1 = list_of_combos[[i]][1],
            var2 = list_of_combos[[i]][2],
            var1_vals = unique(df[[var1]]),
            var2_vals = unique(df[[var2]])
        )
        combo_grids[[i]] <- combo_grid
    }
    combo_grid %>%
        pmap(function(var1, var2, val1, val2) {
            if (nrow(df[get(var1) == val1 & get(var2) == val2]) == 0) {
                grid <- grid[!(get(var1) == val1 & get(var2) == val2)]
            }
        })

    return(grid)
}





nrow_before <- nrow(grid)
cat("Rows before removal of nulls:", nrow_before, "\n")

# remove null combinations




grid <- remove_nulls(df, grid, combs)
nrow_after <- nrow(grid)
cat("Rows after removal of nulls:", nrow_after, "\n")
nrow_diff <- nrow_before - nrow_after
cat("Rows removed:", nrow_diff, round(nrow_diff / nrow_before) * 100, "%", "\n")



### func til split outcomes inde i subsets


multi_df <- data_cont %>% filter(studlab == "rangan 2015", follow_up == 6)

split_multi_outc <- function(df) {
    outcomes <- df %>%
        filter.(n() > 1, .by = "studlab") %>%
        distinct.(outcome) %>%
        pull(outcome)

    loop_out <- list()
    for (outc in outcomes) {
        prim_df <- df %>%
            filter.(outcome == outc)

        prim_studlabs <- prim_df %>%
            distinct.(studlab) %>%
            pull.(studlab)

        rest_df <- df %>%
            filter.(!studlab %in% prim_studlabs)

        if (!any(duplicated(rest_df$studlab))) {
            if (nrow(rest_df) > 0) {
                loop_out[[outc]] <- list(prim_df, rest_df)
            }
            else {
                loop_out[[outc]] <- list(prim_df)
            }
        } else {
            split_list <- split_multi_outc(rest_df)
            loop_out[[outc]] <- c(list(prim_df), flatten(split_list))
        }
    }

    return(loop_out)
}

new_func <- split_multi_outc(multi_df)

new_func

orig_func


subsets[[1]][["smd"]]


## ORIG SUBSETS
smd_orig <- subsets_orig %>% map(~ .x[["smd"]])
smd_orig_null <- smd_orig %>% keep(~ length(.x) == 0)

smd_orig_null %>% length()

subsets_orig["11171"]

##

smd <- subsets %>% map(~ .x[["smd"]])
smd_null <- smd %>% keep(~ length(.x) == 0)

smd_null %>% length()
smd_null %>% head()
subsets["11171"]

length(subsets)
length(subsets_orig)


rr_lists <- rrapply(subsets_orig, f = identity, classes = "list", how = "flatten")
rr_dfs <- rrapply(subsets_orig, f = identity, classes = "data.frame", how = "flatten")


lest <- c(
    rr_lists %>% flatten(),
    rr_dfs
)

lest %>%
    map(~ .x[["smd"]]) %>%
    keep(~ length(.x) == 0)

lest["eq5d"]

rrapply_list <- rrapply(subsets, f = identity, classes = "list", how = "prune") %>%
    flatten()


rrapply_dfs <- rrapply(subsets, f = identity, classes = "data.frame", how = "prune")


merged_list <- c(rrapply_dfs, rrapply_list)

testthat::expect_equal(keep_list, rrapply_list)


length(merged_list)

length(rrapply_list)
length(rrapply_dfs)

flatten(subsets[1:11])

flatten2 <- function(x) {
    len <- sum(rapply(x, function(x) 1L))
    y <- vector("list", len)
    i <- 0L
    rapply(x, function(x) {
        i <<- i + 1L
        y[[i]] <<- x
    })
    y
}


flatten2(subsets[1:11])


list.flatten <- function(x, use.names = TRUE, classes = "ANY") {
    len <- sum(rapply(x, function(x) 1L, classes = classes))
    y <- vector("list", len)
    i <- 0L
    items <- rapply(x, function(x) {
        i <<- i + 1L
        y[[i]] <<- x
        TRUE
    }, classes = classes)
    if (use.names && !is.null(nm <- names(items))) {
          names(y) <- nm
      }
    y
}

list.flatten(subsets[1:11], classes = "list")

class(subsets)


# og så skal den vel bare flattens?

####
df <- subsets_multi_outc[["14371"]]

df %>% filter.(n() > 1, .by = "studlab")

### FUNC STARTER: input: DF
split_outc <- function(df) {
    outcomes <- df %>%
        filter.(n() > 1, .by = "studlab") %>%
        distinct.(outcome) %>%
        pull(outcome)

    loop_out <- list()
    for (outc in outcomes) {
        prim_df <- df %>% filter.(outcome == outc)

        prim_studlabs <- prim_df %>%
            distinct.(studlab) %>%
            pull.(studlab)

        rest_df <- df %>% filter.(!studlab %in% prim_studlabs)
        rest_multi_studlab <- rest_df %>% filter.(n() > 1, .by = "studlab")

        if (nrow(rest_multi_studlab) == 0) {
            if (nrow(rest_df) > 0) {
                loop_out[[outc]] <- list(prim_df, rest_df)
            }
            else {
                loop_out[[outc]] <- list(prim_df)
            }
        } else {
            split_list <- split_outc(rest_df)
            bind_df <- c(list(prim_df), flatten(split_list))
            loop_out[[outc]] <- bind_df
        }
    }

    return(loop_out)
}

test_subs <- subsets_multi_outc["14371"]

test_splits <- subsets_multi_outc %>% map(split_outc)


test_splits %>%
    map(~ map(.x, rbindlist)) %>%
    flatten()

test <- test_splits %>%
    flatten() %>%
    map(rbindlist)

test_list[[2]]