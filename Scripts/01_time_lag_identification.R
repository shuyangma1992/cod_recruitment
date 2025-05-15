library(tidyverse)
library(ggpubr)

#load function
load("Utils/cross_correlation_figure.R")

# 1 Physical --------------------------------------------------------------
# read data
physical <- read_rds("Data/physical.rds") %>%
  filter(year >= 1981 & year <= 2022)

#Atlantic inflow vs. sst
f_atlantic_inflow_sst <- cross_correlation_figure(physical$atlantic_inflow, physical$sst)

#NAO vs. sst
f_nao_sst <- cross_correlation_figure(physical$nao, physical$sst)

#AMO vs. sst
f_amo_sst <- cross_correlation_figure(physical$amo, physical$sst)

#climate change vs. sst
f_climate_change_sst <- cross_correlation_figure(physical$climate_change, physical$sst)

#NAO vs. Atlantic inflow
f_nao_atlantic_inflow <- cross_correlation_figure(physical$nao, physical$atlantic_inflow)

#AMO vs. Atlantic inflow
f_amo_atlantic_inflow <- cross_correlation_figure(physical$amo, physical$atlantic_inflow)

#climate change vs. Atlantic inflow
f_climate_change_atlantic_inflow <- cross_correlation_figure(physical$climate_change, physical$atlantic_inflow)

#combine figures
ggarrange(f_atlantic_inflow_sst, f_nao_sst, f_amo_sst, f_climate_change_sst,
          f_nao_atlantic_inflow, f_amo_atlantic_inflow, f_climate_change_atlantic_inflow,
          ncol = 3, nrow = 3, align = "hv", labels = c("a", "b", "c", "d", "e", "f", "g"), font.label = list(family = "Arial")
)

ggsave("Figures/cross_correlation.pdf", device = cairo_pdf, width = 9, height = 6)






