
# GRAPHING ----------------------------------------------------------------



# geom point


### PROMS
pvals_prom %>% ggplot(aes(x = estimate, y = pval, color = k)) +
    geom_point() +
    scale_color_viridis_c() +
    scale_y_log10() +
    geom_hline(yintercept = 0.05)

### QOL
pvals_qol %>% ggplot(aes(x = estimate, y = pval, color = k)) +
    geom_point() +
    scale_color_viridis_c() +
    scale_y_log10() +
    geom_hline(yintercept = 0.05)


# evt køre geom_pointdensity på alle for at vise hvor størstedelen af resultaterne ligger?
# pval_gathered %>%  ggplot(aes(x = estimate, y = pval)) +
#  geom_pointdensity() +
#  scale_color_viridis_c() +
#  geom_hline( yintercept = 0.05) +
#  scale_y_log10()