library(tidyverse)
library(ggsci)
library(scales)
library(readxl)
library(ggpubr)

# Figure settings
theme_set(theme_classic()) # theme
windowsFonts(A = windowsFont("Times New Roman"), B = windowsFont("Arial")) # font
show_col(pal_lancet()(9))
mypal <- pal_lancet()(9) # colour
mytheme <- theme(
  # title
  plot.title = element_text(family = "Arial"),
  plot.subtitle = element_text(face = 3, family = "Arial"),
  # axis
  axis.text = element_text(family = "Arial"),
  axis.title = element_text(family = "Arial"),
  # legend
  legend.title = element_text(family = "Arial"),
  legend.text = element_text(family = "Arial")
)

# 1 Physical data ---------------------------------------------------------
#read data
temperature <- read_rds("Data/HadISST_Barents_Sea_annual_mean")
atlantic_inflow <- read_excel("Data/Physical_Atlantic_inflow.xlsx", sheet = "Data") %>%
  rename(year = 1, atlantic_inflow = 2)
nao <- read_excel("Data/Physical_NAO.xlsx", sheet = "Data") %>%
  select(Year, Annual) %>%
  rename(year = 1, nao = 2)
amo <- read_excel("Data/Physical_AMO.xlsx", sheet = "Data") %>%
  select(Year, Annual) %>%
  rename(year = 1, amo = 2)
climate_change <- read_excel("Data/Physical_Climate_change.xlsx", sheet = "Data") %>% 
  select(Year, Annual) %>%
  rename(year = 1, climate_change = 2)

#combine and filter year after 1981
physical <- left_join(temperature, atlantic_inflow) %>% 
  left_join(nao) %>% 
  left_join(amo) %>% 
  left_join(climate_change) 

# save data
write_rds(physical, "Data/physical.rds")

physical <- physical %>% 
  filter(year >= 1981)

# cross correlation
# on sst
ccf(physical$climate_change, physical$sst, na.action = na.pass) # lag = 0
ccf(physical$amo, physical$sst, na.action = na.pass) # lag = 3 or lag = 1
ccf(physical$nao, physical$sst, na.action = na.pass) # lag = 3, not significant
ccf(physical$atlantic_inflow, physical$sst, na.action = na.pass) # lag = 1 or lag = 0

# on atlantic_inflow
ccf(physical$climate_change, physical$atlantic_inflow, na.action = na.pass) # lag = 0
ccf(physical$amo, physical$atlantic_inflow, na.action = na.pass) # lag = 2
ccf(physical$nao, physical$atlantic_inflow, na.action = na.pass) # lag = 4, not significant

# plot
f_sst <- ggplot(physical, aes(x = year, y = sst)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "loess", colour = mypal[4], fill = mypal[4]) +
  # ggtitle("Barents Sea sea surface temperature") +
  annotate("text",
    label = "Barents Sea sea surface temperature",
    x = 1980, hjust = 0.05, y = Inf, vjust = 1,
    family = "Arial"
  ) +
  scale_x_continuous("Year", limits = c(1980, 2022)) +
  scale_y_continuous("SST (\u00B0C)") +
  mytheme

f_anlantic_inflow <- ggplot(physical, aes(x = year, y = atlantic_inflow)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "loess", colour = mypal[4], fill = mypal[4]) +
  # ggtitle("Atlantic inflow - Bear Island Section temperature") +
  annotate("text",
    label = "Atlantic inflow - Bear Island Section temperature",
    x = 1980, hjust = 0.05, y = Inf, vjust = 1,
    family = "Arial"
  ) +
  scale_x_continuous("Year", limits = c(1980, 2022)) +
  scale_y_continuous("Temperature (\u00B0C)") +
  mytheme

f_nao <- ggplot(physical, aes(x = year, y = scale(nao), fill = ifelse(nao >= 0, "positive", "negative"))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  # ggtitle("North Atlantic Oscillation")+
  annotate("text",
    label = "North Atlantic Oscillation",
    x = 1980, hjust = 0.05, y = Inf, vjust = 1,
    family = "Arial"
  ) +
  scale_fill_manual(values = c("positive" = mypal[2], "negative" = mypal[1])) +
  scale_x_continuous("Year", limits = c(1980, 2022)) +
  scale_y_continuous("NAO") +
  mytheme

f_amo <- ggplot(physical, aes(x = year, y = scale(amo), fill = ifelse(amo >= 0, "positive", "negative"))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  # ggtitle("Atlantic Multidecadal Oscillation")+
  annotate("text",
    label = "Atlantic Multidecadal Oscillation",
    x = 1980, hjust = 0.05, y = Inf, vjust = 1,
    family = "Arial"
  ) +
  scale_fill_manual(values = c("positive" = mypal[2], "negative" = mypal[1])) +
  scale_x_continuous("Year", limits = c(1980, 2022)) +
  scale_y_continuous("AMO") +
  mytheme

f_climate_change <- ggplot(physical, aes(x = year, y = climate_change)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "loess", colour = mypal[4], fill = mypal[4]) +
  # ggtitle("Global mean temperature") +
  annotate("text",
    label = "Global mean temperature",
    x = 1980, hjust = 0.05, y = Inf, vjust = 1,
    family = "Arial"
  ) +
  scale_x_continuous("Year", limits = c(1980, 2022)) +
  scale_y_continuous("Temperature (\u00B0C)") +
  mytheme

ggarrange(f_sst, f_anlantic_inflow, f_nao, f_amo, f_climate_change,
  ncol = 1, align = "hv", labels = c("a", "b", "c", "d", "e"), font.label = list(family = "Arial")
)

ggsave("Figures/physical.pdf", device = cairo_pdf, width = 4, height = 10)









