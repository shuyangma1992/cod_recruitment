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


# 1 Fishing data ----------------------------------------------------------
fishing <- read_excel("Data/Fishing_fishing_mortality.xlsx", sheet = "Data") %>% 
  rename(year = 1, fm = 2, fm_low = 3, fm_high = 4) 

# save data
write_rds(fishing, "Data/fishing.rds")

fishing <- fishing %>% 
  filter(year >= 1981)

# plot
f_fishing <- ggplot(fishing, aes(x = year, y = fm)) +
  geom_hline(aes(yintercept = 0.74), color = mypal[4], linetype = "solid") +
  geom_hline(aes(yintercept = 0.40), color = mypal[4], linetype = "dashed") +
  geom_ribbon(aes(ymax = fm_low, ymin = fm_high), alpha = 0.25) +
  geom_line() +
  geom_point() +
  # ggtitle("Barents Sea sea surface temperature") +
  annotate("text",
           label = "NEA cod fishing mortality (ages 5-10)",
           x = 1980, hjust = 0.05, y = Inf, vjust = 1,
           family = "Arial"
  ) +
  annotate("text",
           label = "Flim = 0.74",
           x = 1980, hjust = 0.15, y = 0.74, vjust = 1.25,
           family = "Arial"
  ) +
  annotate("text",
           label = "Fpa = 0.40",
           x = 1980, hjust = 0.15, y = 0.40, vjust = 1.25,
           family = "Arial"
  ) +
  scale_x_continuous("Year") +
  scale_y_continuous("Fishing mortality") +
  mytheme

# ggsave("Figures/fishing.pdf", device = cairo_pdf, width = 4, height = 2)

# 2 Pollution data (salmon and trout aquaculture production)----------------------------
pollution <- read_excel("Data/Pollution_aquaculture_production.xlsx", sheet = "Data") %>% 
  select(1, 4) %>% 
  rename(year = 1, production = 2)

# save data
write_rds(pollution, "Data/pollution.rds")

# plot
f_pollution <- ggplot(pollution, aes(x = year, y = production/1000)) +
  geom_line() +
  geom_point() +
  # ggtitle("Barents Sea sea surface temperature") +
  annotate("text",
           label = "Atlantic salmon, rainbow trout and trout\nin Troms, Finnmark and Nordland",
           x = 1980, hjust = 0.05, y = Inf, vjust = 1,
           family = "Arial"
  ) +
  scale_x_continuous("Year") +
  scale_y_continuous("Live stocks (million inds)") +
  mytheme


ggarrange(f_fishing, f_pollution,
          ncol = 1, align = "hv", labels = c("a", "b"), font.label = list(family = "Arial")
)

ggsave("Figures/human_pressure.pdf", device = cairo_pdf, width = 4, height = 4)


# 3 Pollution proxy comparison --------------------------------------------
aquaculture <- read_excel("Data/Pollution_aquaculture_production.xlsx", sheet = "Data") %>% 
  select(1, 2, 4) %>% 
  rename(year = 1, total_production = 2, north_production =3)

copper <- read_excel("Data/Pollution_copper.xlsx") %>% 
  rename(year = 1, cu = 2)

pollution <- left_join(aquaculture, copper)

# two production proxies
cor.test(pollution$total_production, pollution$north_production)

#remove data after 2020
cor.test(pollution$north_production[-c(28,29)], pollution$cu[-c(28,29)])



