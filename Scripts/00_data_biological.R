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


# 1 Biological data -------------------------------------------------------
# read data
mackerel <- read_excel("Data/Biological_mackerel.xlsx", sheet = "Data") %>%
  select(Year, SSB, "Low SSB", "High SSB") %>%
  rename(year = 1, mackerel_ssb = 2, mackerel_ssb_low = 3, mackerel_ssb_high = 4) 
herring <- read_excel("Data/Biological_herring.xlsx", sheet = "Data") %>%
  select(Year, TBiomass, "Low TBiomass", "High TBiomass") %>%
  rename(year = 1, herring_tsb = 2, herring_tsb_low = 3, herring_tsb_high = 4)
capelin <- read_excel("Data/Biological_capelin.xlsx", sheet = "Data") %>%
  select(Year, "Total stock biomass") %>%
  rename(year = 1, capelin_tsb = 2) 
cannibalism <- read_excel("Data/Biological_cannibalism.xlsx", sheet = "Data") %>%
  select(Year, Age1_ratio, Age2_ratio, Age3_ratio) %>%
  rename(year = 1, canni_age1 = 2, canni_age2 = 3, canni_age3 = 4)
cannibalism_mortality <- read_excel("Data/Biological_cannibalism_mortality.xlsx", sheet = "Data") %>%
  rename(year = 1, canni_m_age3 = 2)
tfi <- read_excel("Data/Biological_total_fullness_index.xlsx", sheet = "Data") %>%
  select(1:7)
ss_mean_age <- read_excel("Data/Biological_SS_mean_age.xlsx", sheet = "Data") %>% 
  rename(year = 1, mean_age = 2)
zooplankton <- read_excel("Data/Biological_zooplankton.xlsx", sheet = "Data")
mackerel_K <- read_excel("Data/Biological_mackerel_K.xlsx", sheet = "Data") %>% 
  rename(year = 1, K = 2, low_K = 3, high_K = 4)

# combine data
Year <- data.frame(year = 1950:2023)
biological <- left_join(Year, mackerel) %>% 
  left_join(herring) %>% 
  left_join(capelin) %>% 
  left_join(cannibalism) %>% 
  left_join(cannibalism_mortality) %>% 
  left_join(tfi) %>% 
  left_join(ss_mean_age) %>% 
  left_join(zooplankton)

# save data
write_rds(biological, "Data/biological.rds")

biological <- biological %>% 
  filter(year >= 1981)

# 2 Plot predation part ---------------------------------------------------
f_mackerel <- ggplot(mackerel, aes(x = year, y = mackerel_ssb / 1000000)) +
  geom_ribbon(aes(x = year, ymax = mackerel_ssb_high / 1000000, ymin = mackerel_ssb_low / 1000000), alpha = 0.25) +
  geom_line() +
  geom_point() +
  # ggtitle("Barents Sea sea surface temperature") +
  annotate("text",
    label = "NEA mackerel spawning stock biomass",
    x = 1980, hjust = 0.05, y = Inf, vjust = 1,
    family = "Arial"
  ) +
  scale_x_continuous("Year", limits = c(1980, 2022)) +
  scale_y_continuous("SSB (million ton)") +
  mytheme

f_herring <- ggplot(herring, aes(x = year, y = herring_tsb / 1000000)) +
  geom_ribbon(aes(x = year, ymax = herring_tsb_high / 1000000, ymin = herring_tsb_low / 1000000), alpha = 0.25) +
  geom_line() +
  geom_point() +
  # ggtitle("Barents Sea sea surface temperature") +
  annotate("text",
    label = "NSS herring total stock biomass",
    x = 1980, hjust = 0.05, y = Inf, vjust = 1,
    family = "Arial"
  ) +
  scale_x_continuous("Year", limits = c(1980, 2022)) +
  scale_y_continuous("TSB (million ton)") +
  mytheme

f_cannibalism_age1 <- ggplot(cannibalism, aes(x = year, y = canni_age1 / 1000)) +
  geom_line() +
  geom_point() +
  # ggtitle("Barents Sea sea surface temperature") +
  annotate("text",
           label = "Cannibalism on age 1",
           x = 1980, hjust = 0.05, y = Inf, vjust = 1,
           family = "Arial"
  ) +
  scale_x_continuous("Year", limits = c(1980, 2022)) +
  scale_y_continuous("Number (billion)") +
  mytheme

f_cannibalism_age2 <- ggplot(cannibalism, aes(x = year, y = canni_age2 / 1000)) +
  geom_line() +
  geom_point() +
  # ggtitle("Barents Sea sea surface temperature") +
  annotate("text",
           label = "Cannibalism on age 2",
           x = 1980, hjust = 0.05, y = Inf, vjust = 1,
           family = "Arial"
  ) +
  scale_x_continuous("Year", limits = c(1980, 2022)) +
  scale_y_continuous("Number (billion)") +
  mytheme

f_cannibalism_age3 <- ggplot(cannibalism, aes(x = year, y = canni_age3 / 1000)) +
  geom_line() +
  geom_point() +
  # ggtitle("Barents Sea sea surface temperature") +
  annotate("text",
           label = "Cannibalism on age 3",
           x = 1980, hjust = 0.05, y = Inf, vjust = 1,
           family = "Arial"
  ) +
  scale_x_continuous("Year", limits = c(1980, 2022)) +
  scale_y_continuous("Number (billion)") +
  mytheme

cor.test(biological$canni_age3, biological$canni_m_age3) #highly correlated with cannibalism age3
f_canni_m_age3 <- ggplot(cannibalism_mortality, aes(x = year, y = canni_m_age3)) +
  geom_line() +
  geom_point() +
  # ggtitle("Barents Sea sea surface temperature") +
  annotate("text",
           label = "Cannibalism mortality on age 3",
           x = 1980, hjust = 0.05, y = Inf, vjust = 1,
           family = "Arial"
  ) +
  scale_x_continuous("Year", limits = c(1980, 2022)) +
  scale_y_continuous("Mortality", limits = c(0, 0.35)) +
  mytheme

ggarrange(f_mackerel, f_cannibalism_age1,
  f_herring, f_cannibalism_age2,
  f_canni_m_age3, f_cannibalism_age3,
  ncol = 2, nrow = 3, align = "hv", labels = c("a", "d", "b", "e", "c", "f"),
  font.label = list(family = "Arial")
)

ggsave("Figures/biological_predation.pdf", device = cairo_pdf, width = 8, height = 6)


# 3 Plot prey part -------------------------------------------------------------
f_capelin <- ggplot(capelin, aes(x = year, y = capelin_tsb / 1000)) +
  geom_line() +
  geom_point() +
  # ggtitle("Barents Sea sea surface temperature") +
  annotate("text",
    label = "Barents Sea capelin",
    x = 1980, hjust = 0.05, y = Inf, vjust = 1,
    family = "Arial"
  ) +
  scale_x_continuous("Year", limits = c(1980, 2022)) +
  scale_y_continuous("TSB (million ton)") +
  mytheme

cor_age1 <- cor.test(tfi$tfi_age1_winter, tfi$tfi_age1_autumn)
f_tfi_age1 <- ggplot(tfi, aes(x = year)) +
  geom_line(aes(y = tfi_age1_winter), color = mypal[1]) +
  geom_point(aes(y = tfi_age1_winter), color = mypal[1]) +
  geom_line(aes(y = tfi_age1_autumn), color = mypal[6]) +
  geom_point(aes(y = tfi_age1_autumn), color = mypal[6]) +
  annotate("text",
    label = "Total Fullness Index of age 1",
    x = 1980, hjust = 0.05, y = Inf, vjust = 1,
    family = "Arial"
  ) +
  annotate("text",
    label = paste0("r = ", round(cor_age1$estimate,2), ", p = ", round(cor_age1$p.value, 2)),
    x = 1980, hjust = 0.05, y = -Inf, vjust = -1,
    family = "Arial"
  ) +
  scale_x_continuous("Year", limits = c(1980, 2022)) +
  scale_y_continuous("TFI") +
  mytheme

cor_age2 <- cor.test(tfi$tfi_age2_winter, tfi$tfi_age2_autumn)
f_tfi_age2 <- ggplot(tfi, aes(x = year)) +
  geom_line(aes(y = tfi_age2_winter), color = mypal[1]) +
  geom_point(aes(y = tfi_age2_winter), color = mypal[1]) +
  geom_line(aes(y = tfi_age2_autumn), color = mypal[6]) +
  geom_point(aes(y = tfi_age2_autumn), color = mypal[6]) +
  annotate("text",
    label = "Total Fullness Index of age 2",
    x = 1980, hjust = 0.05, y = Inf, vjust = 1,
    family = "Arial"
  ) +
  annotate("text",
    label = paste0("r = ", round(cor_age2$estimate, 2), ", p = ", round(cor_age2$p.value, 2)),
    x = 1980, hjust = 0.05, y = -Inf, vjust = -1,
    family = "Arial"
  ) +
  scale_x_continuous("Year", limits = c(1980, 2022)) +
  scale_y_continuous("TFI") +
  mytheme

cor_age3 <- cor.test(tfi$tfi_age3_winter, tfi$tfi_age3_autumn)
f_tfi_age3 <- ggplot(tfi, aes(x = year)) +
  geom_line(aes(y = tfi_age3_winter), color = mypal[1]) +
  geom_point(aes(y = tfi_age3_winter), color = mypal[1]) +
  geom_line(aes(y = tfi_age3_autumn), color = mypal[6]) +
  geom_point(aes(y = tfi_age3_autumn), color = mypal[6]) +
  annotate("text",
    label = "Total Fullness Index of age 3",
    x = 1980, hjust = 0.05, y = Inf, vjust = 1,
    family = "Arial"
  ) +
  annotate("text",
           label = paste0("r = ", round(cor_age3$estimate, 2), ", p = ", round(cor_age3$p.value, 2)),
           x = 1980, hjust = 0.05, y = -Inf, vjust = -1,
           family = "Arial"
  ) +
  scale_x_continuous("Year", limits = c(1980, 2022)) +
  scale_y_continuous("TFI") +
  mytheme

cor.test(zooplankton$B0180, zooplankton$B01000)
cor.test(zooplankton$B01000, zooplankton$B02000)
cor.test(zooplankton$B0180, zooplankton$B02000)

cor.test(zooplankton$B0180, zooplankton$B0SUM)
cor.test(zooplankton$B01000, zooplankton$B0SUM)
cor.test(zooplankton$B02000, zooplankton$B0SUM)

f_zooplankton <- ggplot(zooplankton, aes(x = year)) +
  geom_line(aes(y = B0180), color = mypal[1]) +
  geom_point(aes(y = B0180), color = mypal[1]) +
  geom_line(aes(y = B01000), color = mypal[2]) +
  geom_point(aes(y = B01000), color = mypal[2]) +
  geom_line(aes(y = B02000), color = mypal[3]) +
  geom_point(aes(y = B02000), color = mypal[3]) +
  geom_line(aes(y = B0SUM), color = mypal[4]) +
  geom_point(aes(y = B0SUM), color = mypal[4]) +
  annotate("text",
           label = "Mesozooplankton",
           x = 1980, hjust = 0.05, y = Inf, vjust = 1,
           family = "Arial"
  ) +
  # annotate("text",
  #          label = paste0("r = ", round(cor_age3$estimate, 2), ", p = ", round(cor_age3$p.value, 2)),
  #          x = 1980, hjust = 0.05, y = -Inf, vjust = -1,
  #          family = "Arial"
  # ) +
  scale_x_continuous("Year", limits = c(1980, 2022)) +
  scale_y_continuous("Mesozooplankton biomass (g/m2)") +
  mytheme

f_null <- ggplot()

ggarrange(f_capelin, f_tfi_age1,
  f_zooplankton, f_tfi_age2,
  f_null, f_tfi_age3,
  ncol = 2, nrow = 3, align = "hv", labels = c("a", "c", "c", "d"),
  font.label = list(family = "Arial")
)

ggsave("Figures/biological_prey.pdf", device = cairo_pdf, width = 8, height = 6)

