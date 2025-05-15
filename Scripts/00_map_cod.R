library(tidyverse)
library(ggOceanMaps)
library(ggspatial)
library(ggsci)
library(scales)
library(MetBrewer)

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


# 1 Map -------------------------------------------------------------------
#Barents Sea
theme_set(theme_bw()) # theme
f <- basemap(limits = c(5, 40, 65, 81), bathy.style = "rcb", land.col = "black") +
  scale_x_continuous("Longitude", breaks = seq(-10, 30, 10), expand = c(0, 0)) +
  scale_y_continuous("Latitude", breaks = seq(65, 85, 5), expand = c(0, 0)) +
  mytheme

ggsave("Figures/cod_map.pdf", device = cairo_pdf, width = 6, height = 5)


# 2 Map cod-mackerel spatial match -------------------------------------------------------------------
#Barents Sea
theme_set(theme_bw()) # theme
f <- basemap(limits = c(5, 35, 65, 77), land.col = "black") +
  scale_x_continuous("Longitude", breaks = seq(-10, 30, 10), expand = c(0, 0)) +
  scale_y_continuous("Latitude", breaks = seq(65, 85, 5), expand = c(0, 0)) +
  mytheme

ggsave("Figures/mackerel_cod_map.pdf", device = cairo_pdf, width = 5, height = 5)
