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


# 1 total fullness index correlation with cod -----------------------------
#total fullness index (tfi) data
tfi <- read_excel("Data/Biological_total_fullness_index.xlsx", sheet = "Data")

#cod data
cod <- read_excel("Data/NEA_cod_SSB_age1_2_3.xlsx") %>%
  rename(
    "year" = 1,
    "rec" = 2, "rec_low" = 3, "rec_up" = 4,
    "ssb" = 5, "ssb_low" = 6, "ssb_up" = 7,
    "age1" = 8, "age2" = 9, "age3" = 10
  )

#combine data
data <- left_join(cod, tfi)

#age1
cor.test(data$age1, data$tfi_age1_autumn, na.action = na.pass)
cor.test(data$age1, data$tfi_age1_winter, na.action = na.pass)
cor.test(data$age1, data$tfi_age1, na.action = na.pass)

#age2
cor.test(data$age2, data$tfi_age2_autumn, na.action = na.pass)
cor.test(data$age2, data$tfi_age2_winter, na.action = na.pass)
cor.test(data$age2, data$tfi_age2, na.action = na.pass)

#age3
cor.test(data$age3, data$tfi_age3_autumn, na.action = na.pass)
cor.test(data$age3, data$tfi_age3_winter, na.action = na.pass)
cor.test(data$age3, data$tfi_age3, na.action = na.pass)

#cannot use mean, maybe use both into the DSEM, as they do not correlate with each other.




















