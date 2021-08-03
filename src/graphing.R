library(arrow)
library(tidyverse)

data_dir <- here("output", )


pvals_qol_none <- read_feather(here::here("output", "pvals_qol_1_none.feather")) %>% as_tibble()

pvals_qol_beks <- read_feather(here::here("output", "pvals_qol_1_beks.feather")) %>% as_tibble()


type(pvals_qol_beks)

pvals_qol_beks %>% distinct()
pvals_qol_none %>%distinct()
# GRAPHING ----------------------------------------------------------------

# geom point

pvals_qol_none %>% ggplot(aes(x = estimate, y = pval, color = k)) +
    geom_point() +
    scale_color_viridis_c() +
    scale_y_log10() +
    geom_hline(yintercept = 0.05)

### QOL
pvals_qol_beks %>% ggplot(aes(x = estimate, y = pval, color = k)) +
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