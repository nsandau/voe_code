




# create binarys ----------------------------------------------------------

make_binary <- function(data, type = NULL ) {
  
  data <- data %>% mutate(
    bin_age = case_when(
      is.na(total_avg_age) ~ 5,
      between(total_avg_age, 18, 60) ~ 4,
      between(total_avg_age, 60.001, 65) ~ 3,
      between(total_avg_age, 65.001, 70) ~ 2,
      total_avg_age > 70 ~ 1,
    ),
    bin_db = case_when(
      str_detect(databases, "embase|pubmed") & str_detect(databases, "psycinf|pedro|wos") ~ 2, # SKAL FIKSES N?R INDHENTET DATA
      str_detect(databases, "embase|pubmed") ~ 1
    ),
    bin_lang = case_when(
      str_detect(language, "english") ~ 1,
      TRUE ~ 2
    ),
    bin_year = case_when(
      between(year, 0, 1999) ~ 2,
      year > 2000 ~ 1,
    ),
    bin_neer = case_when(
      total_neer_1_part > total_neer_2_part & total_neer_1_part > total_neer_3_part & total_neer_1_part > total_neer_4_part ~ 1,
      total_neer_2_part > total_neer_1_part & total_neer_2_part > total_neer_3_part & total_neer_2_part > total_neer_4_part ~ 2, 
      total_neer_3_part > total_neer_1_part & total_neer_3_part > total_neer_2_part & total_neer_3_part > total_neer_4_part ~ 3,
      total_neer_4_part > total_neer_1_part & total_neer_4_part > total_neer_2_part & total_neer_4_part > total_neer_3_part ~ 4,
      is.na(total_neer_1_part) | is.na(total_neer_2_part) | is.na(total_neer_3_part) | is.na(total_neer_4_part) ~ 5
    ),
    bin_sex = case_when( 
      is.na(total_female) | is.na(total_male) ~ 3, 
      total_female / (total_male + total_female) > 0.85 ~ 1, 
      TRUE ~ 2
    ),
    bin_fu_excl = case_when(
      between(follow_up, 3, 6) ~ 4,
      between(follow_up, 6.1, 12) ~ 3,
      between(follow_up, 12.1, 18) ~ 2,
      follow_up > 18 ~ 1
    ), 
    bin_fu_period = case_when(
      between(follow_up, 0, 11.9) ~ 1,
      between(follow_up, 12, 23.9) ~ 2,
      follow_up >= 24 ~ 3
    ),
    bin_imputed = case_when(
      imputed_outcome == "no" ~ 1, 
      imputed_outcome == "yes" ~ 2
    ),
    bin_design = case_when(
      design == "rct" ~ 1, 
      design == "nrsi" ~ 2
    ),
    bin_publication = case_when(
      publication_status == "published" ~ 1, 
      publication_status == "unpublished" ~ 2
    ),
    bin_funding = case_when(
      funding == "nonprofit" ~ 1,
      funding == "none" ~ 1,
      funding == "commercial" ~ 2,
      funding == "nr" ~ 2
    ),
      bin_sel_bias = case_when(
      design == "rct" & rob_selective_reporting == "low"  ~ 1,
      design == "rct" & rob_selective_reporting == "unclear" ~ 2,
      design == "rct" & rob_selective_reporting == "high" ~ 2,
      design != "rct" ~ 2
    ),
    bin_outcome = case_when( #husk at opdatere hvis flere 
      outcome == "oss" ~ 1,
      outcome == "sst" ~ 2, 
      outcome == "dash" ~ 3, 
      outcome == "ases" ~ 4, 
      outcome == "cs" ~5, 
      outcome == "euroqol"~ 6, 
      outcome == "qol15d" ~ 7,
      outcome == "qdash" ~ 8,
      outcome == "sf36" ~ 9,
      outcome == "sf36pc" ~ 10,
      outcome == "vr12" ~ 11,
      outcome == "sf12pc" ~ 12,
      outcome == "sf12" ~ 13
    ),
    pct_loss_to_fu = (total_loss_to_fu / total_n)*100
    ,
    bin_loss_fu = case_when(
      pct_loss_to_fu <= 5 ~ 1, 
      between(pct_loss_to_fu, 5.01, 10 ) ~ 2, 
      between(pct_loss_to_fu, 10.01, 15 ) ~ 3,
      pct_loss_to_fu > 15 ~ 4,
      is.na(pct_loss_to_fu) ~ 5
      
    )
  ) %>%
    group_by(studlab, outcome) %>% # bin_fu_long = 
    mutate(bin_fu_longest = case_when(
      follow_up == max(follow_up) ~ 1,
      TRUE ~ 0
    )) %>%
    group_by(studlab, outcome, bin_fu_period) %>%
    mutate(bin_fu_period_long = case_when(
      follow_up == max(follow_up) ~1, 
      TRUE ~ 0
    ))
  
  
  if (type == "ma") {
   
    # intervention
    data <- data %>% mutate( bin_interv = case_when( # Husk at opdatere hvis flere interventioner!!
      interv == "ha" ~ 1,
      interv == "plate"~ 2,
      interv == "mixed" ~ 3, 
      interv == "tension-band" ~ 4,
      interv == "rsa" ~ 5, 
      interv == "imn" ~ 6,
      interv == "exfix" ~ 7,
      interv == "k-wires" ~ 8
    ))
    
   
    # bin_totaln - total patients
    data <- data %>% group_by(studlab, outcome, follow_up) %>% 
      summarise(n = sum(int_n,con_n)) %>% slice_max(n, n = 1, with_ties = F) %>% 
      transmute(bin_totaln = case_when(
        n >= 25 ~ 1,
        between(n, 20, 24) ~ 2,
        between(n, 1, 19) ~ 3) 
      ) %>% 
      left_join(data, .)
  } 
  
  if( type == "nma") {
    
    #placeholder for studies with multiple outcomes 
    data <- data %>% mutate( bin_interv = 1) # skal være det samme som i sel_grid så de ikke bliver frasorteret
    
    ## bin_totaln max patients analysed
    data <- data %>% group_by(studlab, outcome, follow_up) %>% 
      summarise(n = sum(n1,n2)) %>% slice_max(n, n = 1, with_ties = F) %>% 
      transmute(bin_totaln = case_when(
        n >= 25 ~ 1,
        between(n, 20, 24) ~ 2,
        between(n, 1, 19) ~ 3) 
      ) %>% 
      left_join(data, .)
  
  } 
  
 
  
 data %>% mutate(
    across(starts_with("bin_"), as.integer),
    follow_up = as.integer(follow_up)
  ) %>% ungroup() %>% return()
  

}

# bin_interv used as placeholder for multiple outcomes in NMA -------------

multi_outc_nma <- function(df) {
  df %>%
    group_by(studlab, treat1, treat2, follow_up) %>%
    mutate(
      bin_interv =
        case_when(
          n() > 1 ~ as.integer(row_number() + 1),
          TRUE ~ as.integer(1)
        )
    ) %>% 
    return()
}



# multi_outc_MA - used for testing ----------------------------------------

multi_outc_ma <- function(df) {
  df %>%
    group_by(studlab, bin_interv, follow_up) %>%
    mutate(
      multi_out =
        case_when(
          n() > 1 ~ as.integer(row_number()),
          TRUE ~ as.integer(1)
        )
    ) %>% 
    return()
}


# create selection grid ---------------------------------------------------


sel_grid <- function(data, type = NULL) {
  
  
  if (type == "ma") {interv_vec <- c(unique(data$bin_interv), 51:53)} 
    if (type == "nma") {interv_vec <- unique(data$bin_interv)} 
  
  expand_grid(
    sel_age = unique(data$bin_age),
    sel_db = unique(data$bin_db),
    sel_lang = unique(data$bin_lang),
    sel_year = unique(data$bin_year),
    sel_totaln = unique(data$bin_totaln),
    sel_loss_fu = unique(data$bin_loss_fu),
    sel_outcome = c(unique(data$bin_outcome), 51),
    sel_imputed = unique(data$bin_imputed),
    sel_design = unique(data$bin_design),
    sel_funding = unique(data$bin_funding),
    sel_sel_bias = unique(data$bin_sel_bias),
    sel_publication = unique(data$bin_publication),
    sel_interv = interv_vec,
    sel_fu_excl = c(unique(data$bin_fu_excl), 51:52),
    sel_unq_fu = unique(data$follow_up),
    sel_fu_period = unique(data$bin_fu_period)
  ) %>%
    mutate(across(everything(), as.integer))
}




# subset ------------------------------------------------------------------

do_subset <- function(data, nrsi_studies, type = NULL,
                          sel_age,
                          sel_db,
                          sel_lang,
                          sel_year,
                          sel_totaln,
                          sel_loss_fu,
                          sel_outcome,
                          sel_imputed, 
                          sel_design,
                          sel_funding,
                          sel_sel_bias,
                          sel_publication,
                          sel_interv,
                          sel_fu_excl,
                          sel_unq_fu,
                          sel_fu_period
) {
  
  select_age <- 1:sel_age
  select_db <- 1:sel_db
  select_lang <- 1:sel_lang
  select_year <- 1:sel_year
  select_totaln <- 1:sel_totaln
  select_loss_fu <- 1:sel_loss_fu
  select_imputed <- 1:sel_imputed
  select_design <- 1:sel_design
  select_publication <- 1:sel_publication
  select_funding <- 1:sel_funding
  select_sel_bias <- 1:sel_sel_bias
  
  select_outcome <- if (sel_outcome == 51 ) {unique(data$bin_outcome)} else {sel_outcome}
  
  
  ### select interv. For NMA select_interv placeholder for multiple outcomes
  if (type == "ma") {
    select_interv <-  if (sel_interv == 51) {c(2,4)} else #plate + tension
    if (sel_interv == 52) {c(1,5)} else #rsa + HA
      if (sel_interv == 53) {unique(data$bin_interv)}
  else {sel_interv}
  } 
  
  if (type == "nma") {
    select_interv <- if (sel_outcome == 51) {c(1,sel_interv)} else(unique(data$bin_interv))
    }
  
  
  #follow-up vars
  
  select_fu_excl <- if (sel_fu_excl %in% c(51,52)) {1:4} else {1:sel_fu_excl}
  select_fu_longest <- if (sel_fu_excl %in% c(51,52)) {0:1} else {1} 
  select_fu <- if (sel_fu_excl == 51) {sel_unq_fu} else {unique(data$follow_up)}
  select_fu_period <- if (sel_fu_excl == 52) {sel_fu_period} else {unique(data$bin_fu_period)}
  select_fu_period_long <- if (sel_fu_excl == 52) {1} else {0:1}
  
  #do subset
  
temp_sub <- data[bin_age %in% select_age &
  bin_db %in% select_db &
  bin_lang %in% select_lang &
  bin_year %in% select_year &
  bin_totaln %in% select_totaln &
  bin_loss_fu %in% select_loss_fu &
  bin_imputed %in% select_imputed &
  bin_design %in% select_design &
  bin_publication %in% select_publication &
  bin_funding %in% select_funding &
  bin_sel_bias %in% select_sel_bias &
  bin_outcome %in% select_outcome &
  bin_interv %in% select_interv &
  (bin_fu_excl %in% select_fu_excl & bin_fu_longest %in% select_fu_longest) &
  follow_up %in% select_fu &
  (bin_fu_period %in% select_fu_period & bin_fu_period_long %in% select_fu_period_long)]


# filter out dfs with < 2 studies


# , studies with only NRSI, studies with 1 RCT and 1 NRSI 
# | sum(temp_sub$studlab %in% nrsi_studies) == nrow(temp_sub) | nrow(temp_sub) == 2 & sum(temp_sub$studlab %in% nrsi_studies) > 0


if (nrow(temp_sub) < 2 ) { return(NULL) } 

if (type == "nma") {
  if(!any(temp_sub$treat1 == "nonop" | temp_sub$treat2 == "nonop")) {return(NULL)}
} 
  

{return(temp_sub)}
  
  
}


# do metagen --------------------------------------------------------------


do_metagen <- function(data) {
  metagen(
    TE = smd,
    seTE = se,
    hakn = FALSE,
    method.tau = "DL",
    studlab = studlab,
    data = data)
}


# do netmeta --------------------------------------------------------------

do_netmeta <- function(data) {
  netmeta(
    TE = TE, 
    seTE = seTE,
    treat1 = treat1,
    treat2 = treat2,
    studlab = studlab,
    data = data,
    sm = "SMD",
    comb.fixed = TRUE,
    comb.random = FALSE,
    reference.group = "nonop",
    details.chkmultiarm = TRUE,
    sep.trts = " vs ",
    tol.multiarm = 0.05)
}



# identify studies with > 1 subnetwork ------------------------------------

identify_multi_networks <- function(data) {
  data %>%
    map(~ netconnection(
      .x$treat1,
      .x$treat2,
      .x$studlab,
      data = .x
    )) %>%
    enframe() %>%
    mutate(
      sub_net = map_int(value, ~ .x[["n.subnets"]])
    ) %>%
    filter(sub_net > 1) %>% 
    pull(name)
}



# extract results metagen ---------------------------------------------------------


extract_metagen <- function(data) {
   data %>% map_dbl("TE.fixed") %>% 
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
}



# extract results nma -----------------------------------------------------

extract_netmeta <- function(data) {
  map(data, ~ as_tibble(.x$TE.fixed, rownames = "treatment") %>%
                       select(treatment, nonop) %>%
                       rename(te.fixed = nonop) %>%
                       mutate(
                         sete.fixed = as_tibble(.x$seTE.fixed) %>% pluck("nonop"),
                         pval.fixed = as_tibble(.x$pval.fixed) %>% pluck("nonop"),
                         lower.fixed = as_tibble(.x$lower.fixed) %>% pluck("nonop"),
                         upper.fixed = as_tibble(.x$upper.fixed) %>% pluck("nonop"),
                         te.random = as_tibble(.x$TE.random) %>% pluck("nonop"),
                         sete.random = as_tibble(.x$seTE.random) %>% pluck("nonop"),
                         pval.random = as_tibble(.x$pval.random) %>% pluck("nonop"),
                         lower.random = as_tibble(.x$lower.random) %>% pluck("nonop"),
                         upper.random = as_tibble(.x$upper.random) %>% pluck("nonop"),
                         i2 = .x$I2,
                         q = .x$Q,
                         pval.q = .x$pval.Q,
                         k.trt = .x$k.trts,
                         k = .x$k,
                         studlab = str_flatten(.x$studlab, collapse = " ")
                       ) %>%
                       mutate_all(~ replace(., is.na(.), 0))) %>% 
    bind_rows() %>% 
    filter( !treatment == "nonop")
}


# gather pvals ------------------------------------------------------------

gather_pvals <- function(data) {
  data %>%
    transmute(
      pval = pval.fixed,
      estimate = te.fixed,
      k = k,
      method = "fixed"
    ) %>%
    bind_rows(
      .,
      data %>%
        transmute(
          pval = pval.random,
          estimate = te.random,
          k = k,
          method = "random"
        )
    ) %>%
    distinct()
}




# gather pvals nma --------------------------------------------------------

gather_pvals_nma <- function(data) {
  data %>%
    transmute(
      pval = pval.fixed,
      estimate = te.fixed,
      k = k,
      treatment = treatment,
      method = "fixed"
    ) %>%
    bind_rows(
      .,
      data %>% transmute(
        pval = pval.random,
        estimate = te.random,
        k = k,
        treatment = treatment,
        method = "random"
      )
    )
}





