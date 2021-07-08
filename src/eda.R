
remove_nulls <- function(df, grid, list_of_combos) {
    df <- df %>%
        rename(
            outcome = bin_outcome,
            intervention = interv
        )

    # for each combo:
    # create selection_grid
    # pmap over rows using var1, var2

    for (i in seq_along(list_of_combos)) { # må kunne gøres bedre. Med sel grid?
        var1 <- list_of_combos[[i]][1]
        var2 <- list_of_combos[[i]][2]
        var1_vals <- unique(df[[var1]])
        var2_vals <- unique(df[[var2]])

        for (val1 in var1_vals) {
            for (val2 in var2_vals) {
                n_rows <- df[get(var1) == val1 & get(var2) == val2] %>%
                    nrow()
                if (n_rows == 0) {
                    grid <- grid[!(get(var1) == val1 & get(var2) == val2)]
                }
            }
        }
    }
    return(grid)
}

df <- data
combs <- list(
    c("follow_up", "outcome"),
    c("intervention", "outcome"),
    c("intervention", "follow_up")
)

expand_grid(
    var1 = unique(df[combs[[1]][1]]),
    var2 = unique(df[combs[[1]][2]])
)



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