library(tidyverse)
library(ggsci)
library(ggOceanMaps)
library(scales)
library(ncdf4)
library(tidync)
library(lubridate)
library(ggspatial)

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

# 1 Polygon map -----------------------------------------------------------
# SST polygon
BS_polygon <- data.frame(
  lon = c(20, 51, 51, 20),
  lat = c(70, 70, 80, 80)
)
BS_polygon <- transform_coord(lon = BS_polygon$lon, lat = BS_polygon$lat)

# Atlantic Inflow point
BS_point <- data.frame(
  lon = c(20),
  lat = c(73)
)

# Northeast Atlantic
f_polygon <- basemap(c(-10, 30, 50, 85), bathymetry = T) +
  # annotate("text",x=0,y=60,label="gdsgasdgasdgasdgad")+
  geom_polygon(
    data = BS_polygon, aes(x = lon, y = lat),
    color = mypal[2], fill = NA, linewidth = 1
  ) +
  ggspatial::geom_spatial_point(
    data = BS_point, aes(x = lon, y = lat),
    color = mypal[3], size = 2,
  ) +
  ggtitle("Barents Sea SST and Atlantic Inflow") +
  scale_x_continuous("Longitude", breaks = seq(-10, 30, 10)) +
  scale_y_continuous("Latitude", breaks = seq(50, 80, 10)) +
  mytheme

ggsave("Figures/polygon_map.pdf", device = cairo_pdf, width = 6, height = 6)


# 2 HadISST  --------------------------------------------------------------
# load HadISST nc file to see the information
hadisst <- nc_open("Data/HadISST_global_190001_202402.nc")

# use tidync
hadisst <- tidync("Data/HadISST_global_190001_202402.nc")

# Barents Sea polygon
hadisst_bs <- hadisst %>%
  hyper_filter(
    latitude = latitude >= 70 & latitude <= 80,
    longitude = longitude >= 20 & longitude <= 51
  )

# change to tibble
hadisst_bs <- hadisst_bs %>%
  hyper_tibble() %>%
  mutate(time = as_date(time / 60 / 60 / 24, origin = "1970-01-01"))

# add year and month variables
hadisst_bs <- hadisst_bs %>%
  mutate(
    year = year(time),
    month = month(time)
  )

# filter year 2004, replace sst -1000 by NA
hadisst_bs <- hadisst_bs %>%
  filter(year < 2024) %>%
  mutate(sst = na_if(sst, -1000))

# calculate annual average
hadisst_bs <- hadisst_bs %>%
  group_by(year) %>%
  summarise(sst = mean(sst, na.rm = T)) %>%
  ungroup()

# save data
write_rds(hadisst_bs, file = "Data/HadISST_Barents_Sea_annual_mean")

# plot
f_sst_bs <- ggplot(hadisst_bs) +
  geom_line(aes(x = year, y = sst), colour = mypal[2]) +
  geom_smooth(aes(x = year, y = sst), method = "loess", colour = mypal[2]) +
  ggtitle("Barents Sea SST",
    subtitle = "HadISST dataset, annual mean"
  ) +
  scale_x_continuous("Year", limits = c(1950, 2024), breaks = seq(1950, 2020, 10)) +
  scale_y_continuous("Sea surface temperature (\u00B0C)") +
  mytheme

ggsave("Figures/barents_sea_sst.pdf", device = cairo_pdf, width = 6, height = 3)
