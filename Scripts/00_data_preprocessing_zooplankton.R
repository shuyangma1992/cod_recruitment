library(tidyverse)
library(terra)
library(sf)
library(tidyterra)
library(readxl)
library(ggcorrplot)
library(rnaturalearth)
library(ggsci)
library(scales)

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

# 1 Zooplankton correlation -----------------------------------------------
# read zooplankton data
zooplankton <- read_excel("Data/Raw_Zooplankton.xlsx", sheet = "Data")

# make it wide format
zooplankton <- zooplankton %>%
  rename(micro = 4, meso = 5, macro = 6, sum = 7) %>% 
  pivot_longer(c(4, 5, 6, 7), names_to = "size", values_to = "dryweight") %>%
  pivot_wider(names_from = c(season, watermass, size), values_from = dryweight)

# cod age1 data
cod <- read_excel("Data/NEA_cod_SSB_age1_2_3.xlsx") %>%
  rename(
    "rec" = 2, "rec_low" = 3, "rec_up" = 4,
    "ssb" = 5, "ssb_low" = 6, "ssb_up" = 7,
    "age1" = 8, "age2" = 9, "age3" = 10
  )
cod <- cod %>%
  select(Year, rec, age1, age2)

# combine data
data <- left_join(zooplankton, cod)

# load function correlation_autocorrelation
load("Utils/correlation_autocorrelation.R")

#correlation results
cor_results <- correlation_autocorrelation(data)

# cor plot
f1 <- ggcorrplot(cor_results$r,
  p.mat = cor_results$p, sig.level = 0.05,
  insig = "blank", type = "lower", method = c("square"),
  outline.color = "white", lab = T, lab_size = 1,
  colors = c("darkred", "white", "darkgreen"),
  legend.title = "Correlation\ncoefficients"
)

ggsave("Figures/zooplankton_correaltion.PDF", device = cairo_pdf, width = 12, height = 12)



# 2 Zooplankton data polygon --------------------------------------------------
#read data from Espen
load("Data/BS_biomass_by_size_and_polygon.Rda")

#polygons
Bear_Island_Trench <- df_poly_wide$geometry[1]
Central_Bank <- df_poly_wide$geometry[2]
Great_Bank <- df_poly_wide$geometry[3]
Hopen_Deep <- df_poly_wide$geometry[4]
South_West <- df_poly_wide$geometry[5]
Svalbard_South <- df_poly_wide$geometry[6]
Thor_Iversen_Bank <- df_poly_wide$geometry[7]

# world data (rnaturalearth package)
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(world) +
  geom_sf(fill = "black") +
  geom_spatvector(data = Bear_Island_Trench, fill = NA, colour = mypal[1], linewidth = 0.5) +
  geom_spatvector_text(data = Bear_Island_Trench, label="Bear Island Trench")+
  geom_spatvector(data = Central_Bank, fill = NA, colour = mypal[1], linewidth = 0.5) +
  geom_spatvector_text(data = Central_Bank, label="Central Bank")+
  geom_spatvector(data = Great_Bank, fill = NA, colour = mypal[1], linewidth = 0.5) +
  geom_spatvector_text(data = Great_Bank, label="Great Bank")+
  geom_spatvector(data = Hopen_Deep, fill = NA, colour = mypal[1], linewidth = 0.5) +
  geom_spatvector_text(data = Hopen_Deep, label="Hopen Deep")+
  geom_spatvector(data = South_West, fill = NA, colour = mypal[1], linewidth = 0.5) +
  geom_spatvector_text(data = South_West, label="South West")+
  geom_spatvector(data = Svalbard_South, fill = NA, colour = mypal[1], linewidth = 0.5) +
  geom_spatvector_text(data = Svalbard_South, label="Svalbard South")+
  geom_spatvector(data = Thor_Iversen_Bank, fill = NA, colour = mypal[1], linewidth = 0.5) +
  geom_spatvector_text(data = Thor_Iversen_Bank, label="Thor Iversen Bank")+
  scale_x_continuous("Longitude", limits = c(10, 50)) +
  scale_y_continuous("Latitude", limits = c(68, 80))+
  mytheme

ggsave("Figures/zooplankton_polygon.pdf", device = cairo_pdf, width = 6, height = 6)


# 3 Zooplankton area weighted ---------------------------------------------
#read data from Espen
load("Data/BS_biomass_by_size_and_polygon.Rda")

#area-weighted mean
data_zooplankton <- df_poly_wide %>% 
  select(-observations, -geometry) %>% 
  pivot_longer(-c(YEAR, stratum, area_km2), names_to = "zoo_type", values_to = "value")

data_zooplankton <- data_zooplankton %>% 
  mutate(area_km2 = as.numeric(area_km2)) %>% 
  mutate(value_area = value*area_km2)

total_area <- sum(unique(data_zooplankton$area_km2))

data_zooplankton <- data_zooplankton %>% 
  group_by(YEAR, zoo_type) %>% 
  summarise(area_weighted = sum(value_area)/total_area) %>% 
  ungroup()

data_zooplankton <- data_zooplankton %>% 
  pivot_wider(names_from = zoo_type, values_from = area_weighted)

#save data
write_csv(data_zooplankton, "data_zooplankton.csv")

