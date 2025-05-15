library(tidyverse)
library(readxl)
library(dsem)
library(phylopath)
library(psych)
library(scales)
library(MetBrewer)
library(ggsci)
library(ggpubr)

# plot setting
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

# 1 Read data -------------------------------------------------------------
# read data
cod <- read_rds("Data/cod.rds") %>%
  select(year, rec, ssb, age1, age2)

physical <- read_rds("Data/physical.rds")

biological <- read_rds("Data/biological.rds") %>%
  select(
    year,
    mackerel_ssb, herring_tsb, capelin_tsb,
    canni_age1, canni_age2, canni_age3, canni_m_age3,
    starts_with("tfi"), B0SUM
  )

fishing <- read_rds("Data/fishing.rds") %>%
  select(year, fm)

pollution <- read_rds("Data/pollution.rds") %>%
  rename(pollution = 2)

# combine data
data <- left_join(cod, physical) %>%
  left_join(biological) %>%
  left_join(fishing) %>%
  left_join(pollution)


# 2 DSEM fixed and selected part ------------------------------------------
# fixed part
sem_fixed <- "

  # link, lag, param_name
  
  # life history
  ssb -> age1, 1, ssb_to_age1
  age1 -> age2, 1, age1_to_age2
  age2 -> rec, 1, age2_to_rec
  
  # physical drivers
  
  # biological drivers

  # anthropogenic drivers
  
"

# selected part
sem_selected <- "

  # link, lag, param_name
  
  # physical drivers
  sst -> ssb, 0, sst_to_ssb
  sst -> age1, 0, sst_to_age1
  sst -> age2, 0, sst_to_age2
  sst -> rec, 0, sst_to_rec
  atlantic_inflow -> sst, 1, atlantic_inflow_to_sst
  nao -> sst, 3, nao_to_sst
  nao -> atlantic_inflow, 2, nao_to_atlantic_inflow
  amo -> sst, 3, amo_to_sst
  amo -> atlantic_inflow, 2, amo_to_atlantic_inflow
  climate_change -> sst, 0, climate_change_to_sst
  climate_change -> atlantic_inflow, 0, climate_change_to_atlantic_inflow

  # biological drivers
  # predator
  mackerel_ssb -> age1, 0, mackerel_ssb_to_age1_lag0
  mackerel_ssb -> age1, 1, mackerel_ssb_to_age1_lag1
  herring_tsb -> age1, 0, herring_tsb_to_age1_lag0
  herring_tsb -> age1, 1, herring_tsb_to_age1_lag1
  canni_m_age3 -> rec, 0, canni_m_age3_to_rec
  # prey
  B0SUM -> age1, 0, B0SUM_to_age1
  capelin_tsb -> ssb, 0, capelin_tsb_to_ssb
  capelin_tsb -> age2, 0, capelin_tsb_to_age2
  capelin_tsb -> rec, 0, capelin_tsb_to_rec
  tfi_age1_autumn -> age1, 0, tfi_age1_autumn_to_age1
  tfi_age1_winter -> age1, 0, tfi_age1_winter_to_age1
  tfi_age2_autumn -> age2, 0, tfi_age1_autumn_to_age2
  tfi_age2_winter -> age2, 0, tfi_age1_winter_to_age2
  tfi_age3_autumn -> rec, 0, tfi_age1_autumn_to_rec
  tfi_age3_winter -> rec, 0, tfi_age1_winter_to_rec
  
  # anthropogenic drivers
  pollution -> ssb, 0, pollution_to_ssb
  pollution -> age1, 0, pollution_to_age1
  pollution -> age2, 0, pollution_to_age2
  pollution -> rec, 0, pollution_to_rec
  fm -> ssb, 0, fishing_to_ssb

"

# make it to be a vector
sem_selected_vector <- c(
  
  # physical drivers
  "sst -> ssb, 0, sst_to_ssb",
  "sst -> age1, 0, sst_to_age1",
  "sst -> age2, 0, sst_to_age2",
  "sst -> rec, 0, sst_to_rec",
  "atlantic_inflow -> sst, 1, atlantic_inflow_to_sst",
  "nao -> sst, 3, nao_to_sst",
  "nao -> atlantic_inflow, 2, nao_to_atlantic_inflow",
  "amo -> sst, 3, amo_to_sst",
  "amo -> atlantic_inflow, 2, amo_to_atlantic_inflow",
  "climate_change -> sst, 0, climate_change_to_sst",
  "climate_change -> atlantic_inflow, 0, climate_change_to_atlantic_inflow",
  
  # biological drivers
  # predator
  "mackerel_ssb -> age1, 0, mackerel_ssb_to_age1_lag0",
  "mackerel_ssb -> age1, 1, mackerel_ssb_to_age1_lag1",
  "herring_tsb -> age1, 0, herring_tsb_to_age1_lag0",
  "herring_tsb -> age1, 1, herring_tsb_to_age1_lag1",
  "canni_m_age3 -> rec, 0, canni_m_age3_to_rec",
  # prey
  "B0SUM -> age1, 0, B0SUM_to_age1",
  "capelin_tsb -> ssb, 0, capelin_tsb_to_ssb",
  "capelin_tsb -> age2, 0, capelin_tsb_to_age2",
  "capelin_tsb -> rec, 0, capelin_tsb_to_rec",
  "tfi_age1_autumn -> age1, 0, tfi_age1_autumn_to_age1",
  "tfi_age1_winter -> age1, 0, tfi_age1_winter_to_age1",
  "tfi_age2_autumn -> age2, 0, tfi_age1_autumn_to_age2",
  "tfi_age2_winter -> age2, 0, tfi_age1_winter_to_age2",
  "tfi_age3_autumn -> rec, 0, tfi_age1_autumn_to_rec",
  "tfi_age3_winter -> rec, 0, tfi_age1_winter_to_rec",
  
  # anthropogenic drivers
  "pollution -> ssb, 0, pollution_to_ssb",
  "pollution -> age1, 0, pollution_to_age1",
  "pollution -> age2, 0, pollution_to_age2",
  "pollution -> rec, 0, pollution_to_rec",
  "fm -> ssb, 0, fishing_to_ssb"
  
)


# 3 DSEM selection with moving window --------------------------------------------------------
# fiter data
data <- data %>% 
  filter(year %in% c(1961:2022))

# moving window - 31 years, 1976 - 2007
results <- NULL
for (i in 1976:2007) {
  # i <- 1987
  data_loop <- data %>%
    filter(year %in% c((i - 15):(i + 15)))

  # 3-year-moving average
  data_loop <- as.data.frame(zoo::rollmean(data_loop, 3))

  # change data to tsdata
  tsdata_loop <- ts(scale(data_loop)[, -1])

  # forward stepwise selection
  model_selection <- try(stepwise_selection(
    tsdata = tsdata_loop,
    model_options = sem_selected_vector,
    model_shared = sem_fixed,
    estimate_delta0 = FALSE,
    control = dsem_control(quiet = TRUE)
  ))

  if (is.character(model_selection)) {
    print(i)
    next
  } else {
    write_rds(model_selection, file = paste0(
      "Results/Sensitivity_analysis_casual_map_moving_window/central_year_", i, ".rds"
    ))
  }

  print(i)
}


# 4 Results tidy ----------------------------------------------------------
# all the results file name
file_name <- list.files("Results/Sensitivity_analysis_casual_map_moving_window/")
best_model <- NULL
for (i in file_name) {
  
  # i <- file_name[1]
  results_loop <- read_rds(paste0("Results/Sensitivity_analysis_casual_map_moving_window/", i))
  record_loop <- results_loop$record
  final_selection_loop <- record_loop[[length(record_loop)]]
  best_model_loop <- as.data.frame(t(final_selection_loop[1,]))
  colnames(best_model_loop) <- c("AIC", sem_selected_vector)
  best_model_loop <- best_model_loop %>% 
    mutate(year = str_sub(i, -8, -5))
  best_model <- bind_rows(best_model, best_model_loop)
  print(i)
}

# tidy final results
best_model <- best_model %>% 
  select(-1) %>% 
  pivot_longer(-year, names_to = "driver", values_to = "occurrence")  

# total frequency
best_model_frequency <- best_model %>% 
  group_by(driver) %>% 
  summarise(frequency = sum(occurrence)) %>% 
  ungroup()
best_model_frequency <- best_model_frequency %>% 
  mutate(driver = factor(driver, levels = rev(unique(best_model$driver))))

# order
best_model <- best_model %>% 
  mutate(driver = factor(driver, levels = rev(unique(driver))),
         occurrence = as.character(occurrence))


# 5 Results figure --------------------------------------------------------
theme_set(theme_bw())

# moving window results
fa <- ggplot(best_model) + 
  geom_tile(aes(x = year, y = driver, fill = occurrence), show.legend = FALSE, 
            color = "white", linewidth = 0.25) +
  scale_fill_manual(values = c("0" = "white", "1" = "black")) +
  scale_x_discrete("Central year (31-year moving window)") +
  scale_y_discrete("Potential driver", labels = rev(c(
    "SST -> SSB (0)",
    "SST -> Age1 (0)",
    "SST -> Age2 (0)",
    "SST -> R (0)",
    "AI -> SST (1)",
    "NAO -> SST (3)",
    "NAO -> AI (2)",
    "AMO -> SST (3)",
    "AMO -> AI (2)",
    "CC -> SST (0)",
    "CC -> AI (0)",
    "Mackerel -> Age1 (0)",
    "Mackerel -> Age1 (1)",
    "Herring -> Age1 (0)",
    "Herring -> Age1 (1)",
    "Cannibalism -> R (0)",
    "MesoZ -> Age1 (0)",
    "Capelin -> SSB (0)",
    "Capelin -> Age2 (0)",
    "Capelin -> R (0)",
    "TFI_Aut -> Age1 (0)",
    "TFI_Win -> Age1 (0)",
    "TFI_Aut -> Age2 (0)",
    "TFI_Win -> Age2 (0)",
    "TFI_Aut -> R (0)",
    "TFI_Win -> R (0)",
    "Aquaculture -> SSB (0)",
    "Aquaculture -> Age1 (0)",
    "Aquaculture -> Age2 (0)",
    "Aquaculture -> R (0)",
    "FM -> SSB (0)"))) +
  mytheme +
  theme(panel.grid.major =  element_line(color = "grey", linetype = "dashed", linewidth = 0.25),
        panel.grid.minor =  element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45))

# frequency
fb <- ggplot(best_model_frequency) + 
  geom_tile(aes(x = 1, y = driver, fill = frequency), show.legend = FALSE, 
            color = "white", linewidth = 0.25) +
  # scale_fill_gradientn(colors = met.brewer("VanGogh3")) +
  scale_fill_gradient(low = "white", high = mypal[5]) +
  scale_x_discrete("Total frequency", expand = c(0, 0)) +
  scale_y_discrete("Driver") +
  mytheme +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

# best DSEM
model_selection <- read_rds("Results/DESM_model_selection.rds")
record <- model_selection$record
final_selection <- record[[length(record)]]
best_model_used <- as.data.frame(final_selection[1,]) %>% 
  mutate(driver = c("AIC", sem_selected_vector))

best_model_used <- best_model_used %>% 
  slice(-1) %>% 
  rename(occurrence = 1) %>% 
  mutate(occurrence = as.character(occurrence),
         driver = factor(driver, levels = rev(driver)))

# best DSEM
fc <- ggplot(best_model_used) + 
  geom_tile(aes(x = 1, y = driver, fill = occurrence), show.legend = FALSE, 
            color = "white", linewidth = 0.25) +
  scale_fill_manual(values = c("0" = "white", "1" = mypal[5])) +
  scale_x_discrete("Best DSEM", expand = c(0, 0)) +
  scale_y_discrete("Driver") +
  mytheme +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

ggarrange(fa, fb, fc,
          nrow = 1, widths = c(13, 1, 1), align = "h")

ggsave("Figures/sensitivity_causal_map.pdf", device = cairo_pdf, width = 6, height = 6)




# 6 Compare best model and 'sensitivity-best' model ------------------------------------------------------------------

# read data
cod <- read_rds("Data/cod.rds") %>%
  select(year, rec, ssb, age1, age2)

physical <- read_rds("Data/physical.rds")

biological <- read_rds("Data/biological.rds") %>%
  select(
    year,
    mackerel_ssb, herring_tsb, capelin_tsb,
    canni_age1, canni_age2, canni_age3, canni_m_age3,
    starts_with("tfi"), B0SUM
  )

fishing <- read_rds("Data/fishing.rds") %>%
  select(year, fm)

pollution <- read_rds("Data/pollution.rds") %>% 
  rename(pollution = 2)

# focusing on 1981-2022
data <- left_join(cod,physical) %>%
  left_join(biological) %>%
  left_join(fishing) %>% 
  left_join(pollution) %>% 
  filter(year %in% c(1981:2022))

#3-year-moving average
data <- as.data.frame(zoo::rollmean(data, 3))

# change data to tsdata
tsdata <- ts(scale(data)[, -1])

# best model
model_selection <- read_rds("Results/DESM_model_selection.rds")

# Best DSEM
model_selection$model

# fit the best dsem
fit <- dsem(
  sem = model_selection$model,
  tsdata = tsdata,
  estimate_delta0 = FALSE,
  control = dsem_control(quiet = TRUE)
)

# save
write.csv(summary(fit), "best_model.csv")

# Define the 'sensitivity-best' model
# using pathways with frequency equal to and more than 11.
sem <- "

  # link, lag, param_name
  
  # life history
  ssb -> age1, 1, ssb_to_age1
  age1 -> age2, 1, age1_to_age2
  age2 -> rec, 1, age2_to_rec
  
  # physical drivers
  sst -> age1, 0, sst_to_age1
  atlantic_inflow -> sst, 1, atlantic_inflow_to_sst
  nao -> sst, 3, nao_to_sst
  nao -> atlantic_inflow, 2, nao_to_atlantic_inflow
  amo -> sst, 3, amo_to_sst
  amo -> atlantic_inflow, 2, amo_to_atlantic_inflow
  climate_change -> atlantic_inflow, 0, climate_change_to_atlantic_inflow

  # biological drivers
  # predator
  canni_m_age3 -> rec, 0, canni_m_age3_to_rec
  # prey
  B0SUM -> age1, 0, B0SUM_to_age1,
  capelin_tsb -> ssb, 0, capelin_tsb_to_ssb,
  tfi_age1_winter -> age1, 0, tfi_age1_winter_to_age1,
  tfi_age3_autumn -> rec, 0, tfi_age3_autumn_to_rec,
  
  # anthropogenic drivers
  pollution -> ssb, 0, pollution_to_ssb
  pollution -> age1, 0, pollution_to_age1
  pollution -> rec, 0, pollution_to_rec
  fm -> ssb, 0, fishing_to_ssb

"

# Fit model
fit2 <- dsem(
  sem = sem,
  tsdata = tsdata,
  estimate_delta0 = F,
  control = dsem_control(quiet = T)
)

# save
write.csv(summary(fit2), "best_model_2.csv")


