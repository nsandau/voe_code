
# PREPARE DF  ---------------------------------------------------------------

data_bin <- cov_raw %>% select(
  -contains(cont_outcomes) #deselect columns with cont outcome
) %>% 
   pivot_longer(  
    cols = contains(bin_outcomes), #reshape to long format based on bin outcome
    names_to = "outcome_type",
    values_to = "values"
  )  %>% 
  mutate( #extract info from outcome_type string
    value_type = word(outcome_type,-1),
    outcome = str_to_lower(word(outcome_type, 1)),
    follow_up = as.numeric(str_extract(outcome_type, "[0-9]+"))
  ) %>% 
  mutate(value_type = if_else(value_type == "N", "e",value_types)
  ) %>%   
  select(-c("outcome_type", "Intervention type")
  ) %>% 
  pivot_wider(
    names_from = c(interv_bin, value_type), #reshape back to wide format 
    values_from = values
  )


# MA LOOPS ----------------------------------------------------------------
# DEFINE OUTCOME VARIABLE TO ANALYSE AND DROP NA Filter out studies with NA in continous variables used in MA
data_bin_ma <- data_bin %>% filter(outcome == "revisions") %>%  drop_na(1:ncol(data_bin))

#define list and df for output
res_list_bin<-list()
res_bin <- tibble(iteration = character(),
                  k=numeric(),
                  FE=numeric(),
                  se_FE=numeric(),
                  inf_FE=numeric(),
                  sup_FE=numeric(),
                  pval_FE=numeric(),
                  RE=numeric(),
                  se_RE=numeric(),
                  inf_RE=numeric(),
                  sup_RE=numeric(),
                  pval_RE=numeric(), 
                  Hetero_I2=numeric(),
                  Hetero_Q=numeric(),
                  pval_Q=numeric())
error_bin <- tibble(error = character())

# i = bin_age 1:3
# j = bin_db 1:2
# k = bin_lang 1:2
# l = bin_year 1:2


# nested loops - der skal være et loop mere end antal værdier til "ALLE"
for (i in 1:4){ 
  for (j in 1:3){
    for (k in 1:3){
      for (l in 1:3){
        #iteration counter
        iteration <- paste(i,j,k,l, sep = "")  
        
        # defining selectors  
        select_age <- if (i <=3) {i} else {c(1,2,3)}
        select_db <- if (j <=2) {j} else {c(1,2)}
        select_lang <- if (k <=2) {j} else {c(1,2)}
        select_year <- if (l <=2) {j} else {c(1,2)}
        
        #create subsetted data 
        subset_data <- subset(data_bin_ma,
                              (data_bin_ma$bin_age %in% select_age) & 
                                (data_bin_ma$bin_db %in% select_db) &
                                (data_bin_ma$bin_lang %in% select_lang) &
                                (data_bin_ma$bin_year %in% select_year)
        )
      
      #test if subsetted df df is empty so meta doesnt produce error 
      if (nrow(subset_data) == 0) {error_bin[iteration, "iteration_error"] <- iteration} else {
        
        res_list_bin[[iteration]] <-metabin(n.e = int_n, event.e = int_e,
                                            n.c = con_n, event.c = con_e,
                                         hakn = FALSE, method.tau = "DL",
                                         sm = "RR", data= subset_data)
        
        #extracting results
        res_bin[iteration,"k"] <- res_list_bin[[iteration]]$k
        res_bin[iteration,"FE"] <- res_list_bin[[iteration]]$TE.fixed
        res_bin[iteration,"se_FE"] <- res_list_bin[[iteration]]$seTE.fixed
        res_bin[iteration,"inf_FE"] <- res_list_bin[[iteration]]$lower.fixed
        res_bin[iteration,"sup_FE"] <- res_list_bin[[iteration]]$upper.fixed
        res_bin[iteration,"pval_FE"] <- res_list_bin[[iteration]]$pval.fixed
        res_bin[iteration,"RE"] <- res_list_bin[[iteration]]$TE.random
        res_bin[iteration,"se_RE"] <- res_list_bin[[iteration]]$seTE.random
        res_bin[iteration,"inf_RE"] <- res_list_bin[[iteration]]$lower.random
        res_bin[iteration,"sup_RE"] <- res_list_bin[[iteration]]$upper.random
        res_bin[iteration,"pval_RE"] <- res_list_bin[[iteration]]$pval.random
        res_bin[iteration,"Hetero_I2"] <- res_list_bin[[iteration]]$I2
        res_bin[iteration,"Hetero_Q"] <- res_list_bin[[iteration]]$Q
        res_bin[iteration,"pval_Q"] <- res_list_bin[[iteration]]$pval.Q
        res_bin[iteration,"iteration"] <- iteration
      }
  }}}}

#filtrere resultater ud med kun 1 studie inkluderet
res_bin_final <- res_bin %>% filter( k > 1) 

# GRAPHING ----------------------------------------------------------------

#fra stackexchange - kan bedre li den!!
res_df_filtered %>% ggplot(aes(x=pval_FE, y=FE)) + 
  geom_pointdensity() + 
  scale_color_viridis_c() +
  scale_y_log10()

#FRA VOE ARTIKEL Density plot (y=p-value, x=effect size)


summary(res_df_filtered$FE)
quantile(res_df_filtered$FE,probs=c(0.25,0.50,0.75), type=1)
quantile(res_df_filtered$FE,probs=c(0.99,0.01), type=1)

summary(res_df$pval)
quantile(df_cont_long_NTX$pval,probs=c(0.25,0.50,0.75), type=1)

p1<-ggplot(df_cont_long_NTX,aes(x=ES, y=pval)) + stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='grey50') +  guides(alpha="none") 
p1<- p1 + scale_fill_continuous("Density", low="green",high="red")
p1<- p1 + geom_point(size=1, shape=19) + xlab("Effect size") + ylab("p value (log scale)") + geom_vline(xintercept = 0)+ geom_hline(yintercept = 0.05)
p1<- p1 + scale_y_continuous(breaks=c(1e-10, 1e-8, 1e-5, 0.05, 1), label=c(expression(10^-10), expression(10^-8), expression(10^-5), "0.05", "1.00") ,trans='log10',limits = c(1e-10, 10))
p1<- p1 + scale_x_continuous(breaks=c(-0.4, -0.2, 0.0, 0.2), limits = c(-0.4, 0.2))
p1<- p1 + theme_classic(base_size = 20) + theme(axis.line.x = element_line(colour = "grey50"), axis.line.y = element_line(colour = "grey50"))
p1<- p1 + geom_vline(xintercept = 0.08595219, linetype="dashed", color = "blue") + geom_vline(xintercept = -0.36792418, linetype="dashed", color = "blue")
p1<- p1 + annotate("text", x = 0.07, y=1e-10, color= "blue", label = "P99") + annotate("text", x = -0.38, y=1e-10, color= "blue", label = "P1")
p1


