library(tidyverse)
library(readxl)
library(ggsci)
library(scales)
library(MetBrewer)
library(psych)
library(ggpubr)

# Figure settings
theme_set(theme_classic()) # theme
windowsFonts(A = windowsFont("Times New Roman"), B = windowsFont("Arial")) # font
show_col(pal_lancet()(9))
show_col(met.brewer("Johnson"))
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



# 1 Age group abundance ---------------------------------------------------
# read data
mackerel <- read_excel("Data/Mackerel_age_specific_number.xlsx", sheet = "Data")

# calculate age 5+ to 10+
mackerel <- mackerel %>%
  mutate(`4+` = `4` + `5` + `6` + `7` + `8` + `9` + `10` + `11` + `12`,
         `5+` = `5` + `6` + `7` + `8` + `9` + `10` + `11` + `12`,
         `6+` = `6` + `7` + `8` + `9` + `10` + `11` + `12`,
         `7+` = `7` + `8` + `9` + `10` + `11` + `12`,
         `8+` = `8` + `9` + `10` + `11` + `12`,
         `9+` = `9` + `10` + `11` + `12`)
  
mackerel <- mackerel %>% 
  select(Year, ends_with("+"))

# mackerel ssb
mackerel_ssb <- read_excel("Data/Biological_mackerel.xlsx", sheet = "Data") %>%
  select(Year, SSB) 

# combine data
mackerel <- left_join(mackerel, mackerel_ssb )

# correlation 
corr.test(mackerel)

# scale and change to long data
mackerel[,-1] <- scale(mackerel[,-1])

mackerel <- mackerel %>% 
  pivot_longer(-Year, names_to = "group", values_to = "value")

# plot
f_age_group <- ggplot(mackerel) +
  geom_line(aes(x = Year, y = value, color = group, linewidth = group)) +
  scale_linewidth_manual(
    name = "Age group",
    values = c(
      "SSB" = 1,
      "4+" = 0.5,
      "5+" = 0.5,
      "6+" = 0.5,
      "7+" = 0.5, 
      "8+" = 0.5
    )
  ) +
  scale_color_manual(
    name = "Age group",
    values = c(
      "SSB" = mypal[9],
      "4+" = mypal[1], 
      "5+" = mypal[2], 
      "6+" = mypal[3], 
      "7+" = mypal[4], 
      "8+" = mypal[5]
    )
  ) +
  annotate("text",
    label = "Northeast Atlantic mackerel age groups",
    x = 1980, hjust = 0.05, y = Inf, vjust = 1,
    family = "Arial"
  ) +
  scale_x_continuous("Year", breaks = seq(1980, 2020, 10)) +
  scale_y_continuous("Scaled value") +
  mytheme +
  theme(legend.position = c(0.4, 0.65),
        legend.key.height = unit(0.25, "cm"))

# ggsave("Figures/mackerel_age_group.pdf", device = cairo_pdf, width = 6, height = 3)



# 2 IESSNS survey CPUE ----------------------------------------------------
data <- read_rds("Data/Mackerel_figure_Kotaro/Mackerel_CPUE.RDS")

# 10-30E, 67-72N
data_focus <- data %>% 
  filter(LON<=30 & LON>=10 & LAT <= 72 & LAT >= 67)

# other area
data_rest <- data %>% 
  anti_join(data_focus)

# mean and SD
data_focus <- data_focus %>% 
  group_by(YEAR) %>% 
  summarise(cpue_mean = mean(CPUE),
            cpue_median = median(CPUE),
            cpue_25 = quantile(CPUE, 0.25),
            cpue_75 = quantile(CPUE, 0.75)) %>% 
  filter(YEAR <= 2022) %>% 
  mutate(area = "focus")

data_rest <- data_rest %>% 
  group_by(YEAR) %>% 
  summarise(cpue_mean = mean(CPUE),
            cpue_median = median(CPUE),
            cpue_25 = quantile(CPUE, 0.25),
            cpue_75 = quantile(CPUE, 0.75)) %>% 
  filter(YEAR <= 2022) %>% 
  mutate(area = "rest")

data_f <- bind_rows(data_focus, data_rest)

f_cpue <- ggplot(data_f) +
  geom_line(aes(x = YEAR, y = log(cpue_median/1000), color = area), position = position_dodge(width = 1), show.legend = FALSE) +
  geom_point(aes(x = YEAR, y = log(cpue_median/1000), color = area), position = position_dodge(width = 1), show.legend = FALSE) +
  geom_errorbar(aes(x = YEAR, ymin = log(cpue_25/1000+0.1), ymax = log(cpue_75/1000), color = area), 
                width = 0, linewidth = 0.25, position = position_dodge(width = 1), show.legend = FALSE) +
  annotate("text",
           label = "Northeast Atlantic mackerel CPUE",
           x = 1980, hjust = 0.05, y = Inf, vjust = 1,
           family = "Arial"
  ) +
  scale_color_manual(values = c("focus" = "black", "rest" = "grey75")) +
  scale_x_continuous("Year", limits = c(1980, 2022), breaks = seq(1980, 2020, 10)) +
  scale_y_continuous("ln CPUE (t/km2)") +
  mytheme

# 
# ggarrange(f_age_group, f_cpue,
#           ncol = 1, nrow = 2, align = "hv", labels = c("(a)", "(b)"),
#           font.label = list(family = "Arial")
# )
# 
# ggsave("Figures/mackerel_age_group_cpue.pdf", device = cairo_pdf, width = 4, height = 4)


# 3 Ono et al., 2024, predicted -------------------------------------------
data <- read_rds("Data/Mackerel_figure_Kotaro/Mackerel_predicted.rds") %>% 
  select(2:6)

# focused area 67-72, 10-30
data_focus <- data %>% 
  filter(LON<=30 & LON>=10 & LAT<=72 & LAT>=67) %>% 
  group_by(YEAR, AGE) %>% 
  summarize(total_focus = sum(Pred))

# all area
data_all <- data %>% 
  group_by(YEAR, AGE) %>% 
  summarize(total_all = sum(Pred))

# proportion
proportion <- left_join(data_focus, data_all)

proportion <- proportion %>% 
  mutate(proportion = total_focus/total_all) %>% 
  mutate(YEAR = as.numeric(as.character(YEAR)), AGE = as.character(AGE)) %>% 
  filter(YEAR <= 2022) %>% 
  filter(AGE %in% c(4, 5, 6, 7, 8))

f_proportion <- ggplot(proportion) + 
  geom_line(aes(x = YEAR, y = proportion, color = AGE)) +
  annotate("text",
           label = "Proportion of biomass in overlapping area to all area",
           x = 1980, hjust = 0.05, y = Inf, vjust = 1,
           family = "Arial"
  ) + 
  scale_color_manual(
    name = "Age group",
    values = c(
      "4" = mypal[1], 
      "5" = mypal[2], 
      "6" = mypal[3], 
      "7" = mypal[4], 
      "8" = mypal[5]
    )
  ) +
  scale_x_continuous("Year", limits = c(1980, 2022), breaks = seq(1980, 2020, 10)) +
  scale_y_continuous("Proportion", breaks = c(0.1, 0.2)) +
  theme(legend.position = c(0.4, 0.65),
        legend.key.height = unit(0.25, "cm"))



ggarrange(f_age_group, f_cpue, f_proportion,
          ncol = 1, nrow = 3, align = "hv", labels = c("(a)", "(b)", "(c)"),
          font.label = list(family = "Arial")
)

ggsave("Figures/mackerel_age_group_cpue.pdf", device = cairo_pdf, width = 4, height = 6)



# mean proportion
mean_proportion <- proportion %>% 
  group_by(AGE) %>% 
  summarise(mean_proportion = mean(proportion))

