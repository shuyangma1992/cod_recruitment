library(tidyverse)
library(maps)
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


# read data
data <- read.table(file = "Data/Mackerel_figure_Thassya/Data July_map2004-2021_corrected_mod.txt", header = T)

# filter data
data <- data %>% 
  filter(Sex.Sp != "NA") %>% 
  filter(Stdmatstage >= 4)

# Get world map data
world_map <- map_data("world")

# plot
f_map_mackerel <- ggplot() +
  geom_map(
    map = world_map, data = world_map,
    aes(x = long, y = lat, map_id = region),
    fill = "black", color = NA
  ) +
  geom_point(data = data, aes(x = Lon, y = Lat), color = mypal[2], shape = 3, size = 0.5) +
  facet_wrap(~Year, nrow = 5) +
  scale_x_continuous("Longitude", limits = c(-45, 30)) +
  scale_y_continuous("Latitude", limits = c(57.5, 77.5)) +
  mytheme +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, margin = margin(b = -12, l = 5), color = "white", family = "Arial"),
    strip.clip = "off",
    panel.spacing = unit(0, "lines"),
    panel.border = element_rect(color = "black", fill = NA)
  )

ggsave("Figures/mackerel_map.pdf", device = cairo_pdf, width = 6, height = 6)

