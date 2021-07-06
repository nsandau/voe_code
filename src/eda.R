### func til split outcomes inde i subsets


subset <- subsets_multi_outc[[1]]



if (any(duplicated(subset[["studlab"]]))) {
    subset <- split_multi_outc(subset)
}


# og så skal den vel bare flattens?

#### 
df <- subsets_multi_outc[['14371']]

df %>% filter.(n() >1, .by = "studlab")

### FUNC STARTER: input: DF
split_outc <- function(df) {

outcomes <- df %>% 
filter.(n() > 1, .by = "studlab") %>% 
distinct.(outcome) %>%
 pull(outcome)

loop_out <- list()
for (outc in outcomes) {
    prim_df <- df %>% filter.(outcome == outc)

    prim_studlabs <- prim_df %>% distinct.(studlab) %>% pull.(studlab)

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

test_subs <- subsets_multi_outc['14371']

test_splits <- subsets_multi_outc %>% map(split_outc)


test_splits %>% map(~map(.x, rbindlist)) %>% flatten()

test <- test_splits %>% flatten() %>% map(rbindlist)

test_list[[2]]

### FUNC START. TAGER EN DF AF GANGEN
multi_split_fun <- function(df) {

df <- lazy_dt(df) %>% group_by(studlab)

multi_df <- df  %>% filter(n() > 1) %>% as.data.table()

unq_df <- df %>% filter(n() <= 1) %>% as.data.table()

split_list <- multi_df %>% 
split(by = "studlab", drop = T) %>%
 map(~split(.x, by = "outcome", drop = T))

idx <- split_list %>% map(~seq_along(.x))

grid <- exec("expand_grid", !!!idx, .name_repair = "minimal")

list_out <- list()
for (row in 1:nrow(grid)) {
            df_list <- list()
    for (col in 1:ncol(grid)) {

        df_idx <- grid[[row,col]]
        df_list[[col]] <- split_list[[col]][[df_idx]]
    }
    df_list[[col+1]] <- unq_df # kan evt indsætte tjek for om unq_df har 0 rows
    list_out[[row]] <- df_list
}
return (list_out)
}


test_out %>% flatten() %>%  map(~rbindlist(.x))


row
col

list_out[row][[col]]

list_out

ncol(grid)

1:ncol(grid)
1:nrow(grid)

list_out[row][[col]] <- "test1"



grid %>% pmap(.f = test_fun)

grid




expand_grid(studlab_idx = idx$name, outcome_idx = idx$value)

outcomes %>% map(~ multi_df %>% filter(outcome != .x))

outcomes <- multi_df %>% distinct(outcome) %>% pull(outcome) 
studlabs <- multi_df %>% distinct(studlab) %>% pull(studlab)


expand_grid(outcomes, studlabs)



ttfu_test <- data_cont %>%
  group_by(studlab, outcome) %>%
  mutate(bin_ttfu = case_when(
    max(follow_up) >= 12 ~ 1,
    between(max(follow_up), 6, 11.999) ~ 2,
    TRUE ~ 3
  ))



ttfu_test %>% select(follow_up, bin_ttfu) %>% filter(bin_ttfu > 1)



subsets_multi_outc


data <- results

outcome <- OUTCOME

  res <- data %>%
        map_dbl("TE.fixed") %>%
        enframe(name = "iteration", "te.fixed") %>%
        mutate(
            sete.fixed = map_dbl(data, "seTE.fixed"),
            lower.fixed = map_dbl(data, "lower.fixed"),
            upper.fixed = map_dbl(data, "upper.fixed"),
            pval.fixed = map_dbl(data, "pval.fixed"),
            te.random = map_dbl(data, "TE.random"),
            sete.random = map_dbl(data, "seTE.random"),
            lower.random = map_dbl(data, "lower.random"),
            upper.random = map_dbl(data, "upper.random"),
            pval.random = map_dbl(data, "pval.random"),
            i2 = map_dbl(data, "I2"),
            q = map_dbl(data, "Q"),
            pval.q = map_dbl(data, "pval.Q"),
            k = map_dbl(data, "k"),
            studlab = map_chr(data, ~ str_flatten(.x$studlab, collapse = " "))
        ) 

    if (outcome == "bin") {
        res <- res %>%
        mutate(
            across(starts_with(c("te.", "sete.", "lower.", "upper.")), exp)

        )
    }

res %>% filter(k > 1)





install.packages("pacman")

test <- "test"

pwr.t.test()

pwr.t.test(d = 1.5, power = 0.8, type = "two.sample", alternative = "two.sided")


?require

require(pacman)

if (!require(pacman)) {
      install.packages(pacman, dependencies = TRUE)
      library(pacman)


pacman::p_load(esc)
### METABIN

test_df <- data_extract %>% select(-(starts_with(cont_outcomes)))

test_df %>%
    filter(studlab == "fjalestad 2014") %>%
    glimpse()

test_df %>%
    select(starts_with(OUTCOME_VARS)) %>%
    names() %>%
    tibble(cols = .) %>%
    mutate(
        n_underscores = str_count(cols, "_")
    ) %>%
    filter(n_underscores != 2)


### imputation binary

data_cont %>%
    group_by(studlab) %>%
    mutate(bin_impute = case_when(
        imputed_outcome == "no" ~ 1,
        str_detect(imputed_outcome, "yes|wrong") & !str_detect(imputed_vars, as.character(outcome)) ~ 1,
        imputed_outcome == "yes" & str_detect(imputed_vars, as.character(outcome)) ~ 2,
        imputed_outcome == "wrong" & str_detect(imputed_vars, as.character(outcome)) ~ 3
    )) %>%
    select(studlab, outcome, contains("impute")) %>%
    View()



data_cont %>% glimpse()




## multi split outcome func

split_multi_outc <- function(split_dfs) {
    loop_list <- list()
    for (df_idx in seq_along(split_dfs)) {
        loop_df <- split_dfs[[df_idx]]
        sub_idx <- df_idx + 1
        while (sub_idx <= length(split_dfs)) {
            loop_df <- bind_rows(loop_df, split_dfs[[sub_idx]] %>%
                filter(!studlab %in% loop_df[["studlab"]]))
            sub_idx <- sub_idx + 1
        }

        sub_idx <- 1
        while (sub_idx < df_idx) {
            loop_df <- bind_rows(loop_df, split_dfs[[sub_idx]] %>%
                filter(!studlab %in% loop_df[["studlab"]]))
            sub_idx <- sub_idx + 1
        }
        loop_list <- c(loop_list, list(loop_df))
    }
    return(loop_list)
}

pacman::p_load(dtplyr)


subsets_multi_outc <- subsets %>%
    keep(~ any(duplicated(.x[["studlab"]]))) %>%
    map(~ .x %>%
        group_by(studlab, outcome) %>%
        group_split())

plan("multicore")
subsets_multi_outc <- subsets_multi_outc %>%
    future_map(~ split_multi_outc(.x))
plan("sequential")


tic("Concat subsets")
# concat lists
subsets <- c(
    subsets %>%
        discard(~ any(duplicated(.x[["studlab"]]))),
    subsets_multi_outc %>% flatten()
)
toc()


cat("Mem usage:", mem_used() / 1024 / 1024, "mb", "\n")


subset_splits <- subsets_multi_outc %>%
    subset_splits[[1]]

length(subset_splits)


subsets <- c(
    subsets %>%
        discard(~ any(duplicated(.x[["studlab"]]))),
    multi_out_splits %>% flatten()
)


df <- data
grid <- read_feather("output/sel_grid_qol_full.feather")

rows_bef <- nrow(grid)

combs <- list(
    c("follow_up", "outcome"),
    c("intervention", "outcome"),
    c("intervention", "follow_up")
)

grid_out <- remove_nulls(df, grid, combs)

nrow(grid_out)

nrow(grid)

### start of func

remove_nulls <- function(df, grid, list_of_combos) {
    df <- df %>%
        select(follow_up, bin_outcome, interv) %>%
        rename(
            outcome = bin_outcome,
            intervention = interv
        )


    for (i in seq_along(combs)) {
        var1 <- combs[[i]][1]
        var2 <- combs[[i]][2]
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







grid

list_of_combs <- idf_null_outc_fu(data)
null_fus <- list_of_combs[["null_fus"]]
null_outcs <- list_of_combs[["null_outcs"]]

for (i in seq_along(null_fus)) {
    grid <- grid[!(follow_up == null_fus[[i]] & outcome == null_outcs[[i]])]
}
nrow_after <- nrow(grid)

nrow_after



###

# HIGH CARDINALITY

data %>%
    tibble() %>%
    summarize(across(everything(), n_distinct))


lobstr::mem_used() / 1024 / 1024

gc()

obj_sizes(subsets_qol)

lobstr::mem_used()

### MOVE DATA OUT FUNCTION _ NOT IMPLEMENTED

from_path <- here::here("data")
to_path <- file.path("/home/nicolai/Desktop/voe_output", DATE)
dir.create(to_path)

file_paths <- list.files(from_path, ".rds$|.feather$")
file.copy(file_paths, to_path)


getwd()

test_list <- subsets_qol %>% discard(~ is.null(.x))
test_tb <- tibble(col1 = test_list)





###

null_outcs <- list()
null_intervs <- list()
for (int in unique(data_func$interv)) {
    for (outc in unique(data_func$bin_outcome)) {
        n_rows <- data_func %>%
            filter(interv == int, bin_outcome == outc) %>%
            nrow()
        cat(int, outc, n_rows, "\n")

        if (n_rows == 0) {
            null_outcs <- c(null_outcs, list(outc))
            null_intervs <- c(null_intervs, list(int))
        }
    }
}

null_intervs <- unlist(null_intervs)
null_outcs <- unlist(null_outcs)

null_list <- idf_null_int_out(data_qol)

null_intervs <- null_list[["null_intervs"]]

null_outcs <- null_list[["null_outcs"]]

for (i in seq_along(null_intervs)) {
    if (i == 1) {
        test_grid <- sel_grid_qol
    }
    cat(null_intervs[[i]], null_outcs[[i]], "\n")
    test_grid <- test_grid[!(intervention == null_intervs[[i]] & outcome == null_outcs[[i]])]
}

all(sel_grid_qol %>% distinct(outcome) == test_grid %>% distinct(outcome))
all(sel_grid_qol %>% distinct(intervention) == test_grid %>% distinct(intervention))

sel_grid_qol %>%
    distinct(intervention) %>%
    sort()
test_grid %>% distinct(intervention)

nrow(sel_grid_qol)
nrow(test_grid)

test_grid %>% distinct(intervention)
test_grid %>% nrow()
test_grid[1:1000, ] %>% View()






null_outcs %>%
    unlist() %>%
    length()
null_intervs %>%
    unlist() %>%
    length()





grid_dropped <- sel_grid_func %>%
    filter(intervention != "k-wires", outcome != 3) %>%
    nrow()

grid_dropped

nrow(sel_grid_func) - grid_dropped

int <- "tension-band"
outc <- 1

data_func %>%
    filter(interv == int, bin_outcome == outc)


null_subs <- subsets_func %>% keep(~ is.null(.x))


length(null_subs)


null_grid <- sel_grid_func[null_subs %>%
    names() %>%
    as.integer(), ]

null_grid[1:100, ] %>% View()

data_func %>% View()



data_qol %>% glimpse()


df <- data_qol
df <- data_func

interv_vals <- as.factor(c(levels(fct_drop(df$interv)), "plate_tb", "artro", "all"))

interv_vals


data_func %>% filter(interv == "k-wires", bin_outcome == 3)


grid_dropped <- sel_grid_func %>%
    filter(intervention != "k-wires", outcome != 3) %>%
    nrow()

grid_dropped

nrow(sel_grid_func) - grid_dropped




levels(data_qol$interv)
test <- data_qol %>% mutate(interv_drop = fct_drop(interv))
levels(test$interv_drop)


levels(data_func$interv)
test <- data_func %>% mutate(interv_drop = fct_drop(interv))
levels(test$interv_drop)


data_func %>% View()


data_cont %>% select(studlab, outcome, imputed_outcome, imputed_vars)


expand_grid(
    language = unique(df$bin_lang),
    year = unique(df$bin_year)
)


neer_vals <- unique(df$bin_neer)
neer_vals <- neer_vals[neer_vals %in% 2:4]

neer_vals


test_neer <- data_cont %>%
    group_by(studlab) %>%
    mutate(
        bin_neer = case_when(
            total_neer_1_part > total_neer_2_part & total_neer_1_part > total_neer_3_part & total_neer_1_part > total_neer_4_part ~ 1,
            total_neer_2_part > total_neer_1_part & total_neer_2_part > total_neer_3_part & total_neer_2_part > total_neer_4_part ~ 2,
            total_neer_3_part > total_neer_1_part & total_neer_3_part > total_neer_2_part & total_neer_3_part > total_neer_4_part ~ 3,
            total_neer_4_part > total_neer_1_part & total_neer_4_part > total_neer_2_part & total_neer_4_part > total_neer_3_part ~ 4,
            is.na(total_neer_1_part) | is.na(total_neer_2_part) | is.na(total_neer_3_part) | is.na(total_neer_4_part) ~ 5
        )
    ) %>%
    select(studlab, contains("neer"))



test_neer_2 <- data_cont %>%
    group_by(studlab) %>%
    mutate(
        bin_neer = case_when(
            all(total_neer_2_part > c(total_neer_1_part, total_neer_3_part, total_neer_4_part)) ~ 2,
            all(total_neer_3_part > c(total_neer_1_part, total_neer_2_part, total_neer_4_part)) ~ 3,
            all(total_neer_4_part > c(total_neer_1_part, total_neer_2_part, total_neer_3_part)) ~ 4,
            TRUE ~ 0
        )
    ) %>%
    select(studlab, contains("neer"))


test_neer_2 %>%
    ungroup() %>%
    distinct(bin_neer)
testthat::expect_equal(test_neer_2, test_neer)





test_grid <- sel_grid_func %>%
    filter(intervention == "all") %>%
    head(n = 5000) %>%
    slice_head(n = 50000) %>%
    future_pmap(.,
        do_subset,
        df = data_func
    ) %>%
    set_names(seq_along(.)) %>%
    discard(~ is.null(.x))




data_cont %>%
    group_by(studlab) %>%
    mutate(
        interv = as.factor(interv),
        interv = fct_expand(interv, c("plate_tb", "arthro"))
    ) %>%
    select(studlab, interv)


bmark_df <- data_cont %>% mutate(
    interv_fct = as.factor(interv),
    interv_int = as.numeric(interv_fct)
)

microbenchmark::microbenchmark(
    bmark_df %>% filter(interv_fct == "k-wires"),
    bmark_df %>% filter(interv_int == 3),
    times = 1000
)


#### method of outcome analysis

data_cont %>%
    group_by(studlab) %>%
    mutate(bin_oa = if_else(method_of_outcome_analysis == "intention-to-treat", 1, 2)) %>%
    select(studlab, method_of_outcome_analysis, bin_oa) %>%
    View()



######### follow-up
data_cont() %>%
    group_by(studlab, outcome) %>%
    mutate(
        bin_fu_longest = case_when(
            follow_up == max(follow_up) ~ 1,
            TRUE ~ 0
        ),
        bin_fu_period = case_when(
            between(follow_up, 0, 11.999) ~ 1,
            between(follow_up, 12, 23.999) ~ 2,
            follow_up >= 24 ~ 3
        )
    ) %>%
    group_by(studlab, outcome, bin_fu_period) %>%
    mutate(bin_fu_period_long = case_when(
        follow_up == max(follow_up) ~ 1,
        TRUE ~ 0
    ))


data_bin %>%
    select(studlab, outcome, follow_up, bin_fu_longest, bin_fu_period, bin_fu_period_long) %>%
    View()


df <- data_func
follow_up <- 99
fu_period <- 1


if (!follow_up %in% c(98, 99)) { # selects identical FUs
    sel_follow_up <- follow_up
    sel_fu_longest <- c(0, 1) # select all
    sel_fu_period <- unique(df$bin_fu_period) # select all
    sel_fu_period_long <- c(0, 1) # select all
} else if (follow_up == 98) { # longest fu from each study
    sel_follow_up <- unique(df$follow_up) # select all fus
    sel_fu_longest <- 1 # select only longest
    sel_fu_period <- unique(df$bin_fu_period) # select all
    sel_fu_period_long <- c(0, 1) # select all
} else if (follow_up == 99) {
    sel_follow_up <- unique(df$follow_up) # select all fus
    sel_fu_longest <- c(0, 1) # select only longest
    sel_fu_period <- fu_period # select specific period
    sel_fu_period_long <- 1 # select longest fu in each period
}


sel_follow_up
sel_fu_longest
sel_fu_period
sel_fu_period_long


df %>%
    filter(
        follow_up %in% sel_follow_up,
        bin_fu_longest %in% sel_fu_longest,
        bin_fu_period %in% sel_fu_period,
        bin_fu_period_long %in% sel_fu_period_long
    ) %>%
    View()



expand_grid(
    fu = c(12, 98, 99),
    fu_period = c(1, 2, 3)
)






###########

test <- data_cont %>% mutate(
    bin_outcome = as.numeric(fct_relevel(outcome, "cs"))
)
test %>% select(outcome, bin_outcome)

df <- test

grid <- expand_grid(
    language = unique(df$bin_lang),
    outcome = c(unique(df$bin_outcome), 51:52)
)

str_subset(unique(data_bin$outcome), fixed("cs"), negate = TRUE)


data_cont %>%
    mutate(
        bin_outcome = as.integer(as.factor(outcome))
    ) %>%
    select(outcome, bin_outcome)


!unique(data_bin$outcome) %in% c("cs")


############################ 3
data_cont %>%
    group_by(studlab) %>%
    mutate(
        pct_loss = (total_loss_to_fu / total_n),
        bin_loss_fu = case_when(
            (total_loss_to_fu / total_n) < 0.15 ~ 1,
            TRUE ~ 2
        )
    ) %>%
    select(studlab, total_n, total_loss_to_fu, pct_loss, bin_loss_fu) %>%
    View()

data_cont %>% count(documentation_for_treatment)


data_cont %>%
    mutate(
        bin_doctreat = if_else(documentation_for_treatment == "yes", 1, 2)
    ) %>%
    select(studlab, documentation_for_treatment, bin_doctreat) %>%
    distinct() %>%
    View()



data_cont %>%
    mutate(
        bin_toi = case_when(
            between(time_of_intervention, 0, 2) ~ 1,
            between(time_of_intervention, 2.0001, 14) ~ 2,
            between(time_of_intervention, 14.0001, 21) ~ 3,
            TRUE ~ 4
        )
    ) %>%
    select(studlab, time_of_intervention, bin_toi) %>%
    distinct() %>%
    View()




data_cont %>%
    mutate(
        bin_34part = case_when(
            total_neer_1_part == 0 & total_neer_2_part == 0 ~ 1,
            TRUE ~ 2
        )
    ) %>%
    select(studlab, contains("neer"), bin_34part) %>%
    View()

data_cont %>%
    mutate(
        bin_34part = if_else(total_neer_1_part == 0 & total_neer_2_part == 0, 1, 2)
    ) %>%
    select(studlab, contains("neer"), bin_34part) %>%
    View()



test_bin <- data_extract %>%
    select(studlab, intervention_type, language) %>%
    mutate(
        intervention_type = as_factor(intervention_type),
        intervention_type = fct_expand(intervention_type, c("orif")),
        language = as_factor(if_else(language == "english", language, "other"))
    )

expand_grid(
    interv = levels(test_bin$intervention_type),
    studlab = unique(test_bin$studlab)
) %>% mutate(across(where(is.character), as.factor))




data_extract %>%
    group_by(studlab) %>%
    summarize(n_sample = total_n - replace_na(total_loss_to_fu, 0)) %>%
    select(n_sample)

data_extract %>%
    mutate(n_sample = total_n - replace_na(total_loss_to_fu, 0)) %>%
    select(studlab, total_n, total_loss_to_fu, n_sample)


unq_df() <- map_df(df, ~ tibble(class = class(.), value = toString(unique(.))))


unq_df %>% filter(class == "character")


data_extract %>%
    select(
        -starts_with(bin_outcomes),
        -starts_with(qol_outcomes),
        -starts_with(prom_outcomes)
    ) %>%
    glimpse()


data_extract %>%
    left_join(data_extract %>%
        group_by(studlab) %>%
        summarize(
            total_n = sum(n),
            total_loss_to_fu = sum(lost_to_fu)
        )) %>%
    select(-n, lost_to_fu)


data_extract %>% left()
total_loss_to_fu

data_extract %>%
    group_by(studlab) %>%
    summarize()

data_extract %>%
    select(starts_with(cont_outcomes)) %>%
    ncol()


data_extract %>%
    select(studlab, starts_with("neer")) %>%
    group_by(studlab) %>%
    summarize(
        total_neer_1_part = sum(neer_1_part),
        total_neer_2_part = sum(neer_2_part),
        total_neer_3_part = sum(neer_3_part),
        total_neer_4_part = sum(neer_4_part)
    )



data_cont %>% map_df(~ count(.x))


data_cont %>% count(databases)


characteristics <- data_extract %>%
    select(2:14, starts_with("total"), -imputed_vars) %>%
    names()

characteristics %>%
    map(~ n_distinct(data_extract[.x])) %>%
    set_names(characteristics) %>%
    enframe() %>%
    unnest(value)


data_extract %>% count(language)


unq_df <- data_extract %>%
    select(characteristics) %>%
    map_df(~ tibble(class = class(.), n_dist = n_distinct(.), value = toString(unique(.))))



data_cont %>% filter(outcome %in% c(prom_outcomes, "cs"))