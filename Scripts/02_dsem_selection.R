library(tidyverse)
library(readxl)
library(dsem)
library(phylopath)
library(psych)
library(devtools)
# install_github( "james-thorson-NOAA/dsem@dev" )

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

# focusing on 1981-2022
data <- left_join(cod,physical) %>%
  left_join(biological) %>%
  left_join(fishing) %>% 
  left_join(pollution) %>% 
  filter(year %in% c(1981:2022))

summary(data)

# # total number of data: 924
# 42*22
# # NA number: 49
# sum(is.na(data))
# # NA proportion 5.30%
# 49/924*100

#3-year-moving average
data <- as.data.frame(zoo::rollmean(data, 3))

# change data to tsdata
tsdata <- ts(scale(data)[, -1])


# 2 Full casual map ------------------------------------------------------------
# Define SEM 
sem <- "

  # link, lag, param_name
  
  # life history
  ssb -> age1, 1, ssb_to_age1
  age1 -> age2, 1, age1_to_age2
  age2 -> rec, 1, age2_to_rec
  
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
  fm -> ssb, 0, fishing_to_ssb
  pollution -> ssb, 0, pollution_to_ssb
  pollution -> age1, 0, pollution_to_age1
  pollution -> age2, 0, pollution_to_age2
  pollution -> rec, 0, pollution_to_rec

"

# 3 Model selection -------------------------------------------------------
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
  B0SUM -> age1, 0, B0SUM_to_age1,
  capelin_tsb -> ssb, 0, capelin_tsb_to_ssb,
  capelin_tsb -> age2, 0, capelin_tsb_to_age2,
  capelin_tsb -> rec, 0, capelin_tsb_to_rec,
  tfi_age1_autumn -> age1, 0, tfi_age1_autumn_to_age1,
  tfi_age1_winter -> age1, 0, tfi_age1_winter_to_age1,
  tfi_age2_autumn -> age2, 0, tfi_age2_autumn_to_age2,
  tfi_age2_winter -> age2, 0, tfi_age2_winter_to_age2,
  tfi_age3_autumn -> rec, 0, tfi_age3_autumn_to_rec,
  tfi_age3_winter -> rec, 0, tfi_age3_winter_to_rec,
  
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
  "tfi_age2_autumn -> age2, 0, tfi_age2_autumn_to_age2",
  "tfi_age2_winter -> age2, 0, tfi_age2_winter_to_age2",
  "tfi_age3_autumn -> rec, 0, tfi_age3_autumn_to_rec",
  "tfi_age3_winter -> rec, 0, tfi_age3_winter_to_rec",

  # anthropogenic drivers
  "pollution -> ssb, 0, pollution_to_ssb",
  "pollution -> age1, 0, pollution_to_age1",
  "pollution -> age2, 0, pollution_to_age2",
  "pollution -> rec, 0, pollution_to_rec",
  "fm -> ssb, 0, fishing_to_ssb"
  
)

#problematic pathways
# "canni_age1 -> age1, 0, canni_age1_to_age0",
# "canni_age2 -> age2, 0, canni_age2_to_age2",
# "canni_age3 -> rec, 0, canni_age3_to_rec",

# 4 DSEM selection--------------------------------------------------------------
# forward stepwise selection
model_selection <- stepwise_selection(tsdata = tsdata,
                                      model_options = sem_selected_vector,
                                      model_shared = sem_fixed,
                                      estimate_delta0 = FALSE,
                                      control = dsem_control(quiet = TRUE))

write_rds(model_selection, file = "Results/DESM_model_selection.rds")

model_selection$record
model_selection$step
model_selection$model

# 5 Best DSEM fit ---------------------------------------------------------
model_selection <- read_rds("Results/DESM_model_selection.rds")

# # selection process
# a <- as.data.frame(model_selection$record[[13]])
# write_csv(a, file = "a.csv")

# Best DSEM
model_selection$model

# fit the best dsem
fit <- dsem(
  sem = model_selection$model,
  tsdata = tsdata,
  estimate_delta0 = FALSE,
  control = dsem_control(quiet = TRUE)
)

# #full model
# fit <- dsem(
#   sem = sem,
#   tsdata = tsdata,
#   estimate_delta0 = FALSE,
#   control = dsem_control(quiet = TRUE)
# )

dsem_param <- summary(fit) %>% 
  filter(direction == 1)
a <- fit$obj$env$parList()
b <- a$x_tj

plot(fit, edge_label = "value", style = "igraph")
plot(fit, edge_label = "value", style = "ggraph")

plot(as_fitted_DAG(fit, lag = 0)) +
  expand_limits(x = c(-0.2, 1))
plot(as_fitted_DAG(fit, lag = 1)) +
  expand_limits(x = c(-0.2, 1))
plot(as_fitted_DAG(fit, lag = 2)) +
  expand_limits(x = c(-0.2, 1))
plot(as_fitted_DAG(fit, lag = 3)) +
  expand_limits(x = c(-0.2, 1))

plot(as_fitted_DAG(fit, lag = 0, what = "p_value"))
plot(as_fitted_DAG(fit, lag = 1, what = "p_value"))
plot(as_fitted_DAG(fit, lag = 2, what = "p_value"))
plot(as_fitted_DAG(fit, lag = 3, what = "p_value"))


plot(as_fitted_DAG(fit, lag = 1)) +
  expand_limits(x = c(-0.2, 1))
plot(as_fitted_DAG(fit, lag = 2)) +
  expand_limits(x = c(-0.2, 1))
plot(as_fitted_DAG(fit, lag = 3)) +
  expand_limits(x = c(-0.2, 1))

plot(as_fitted_DAG(fit, what = "p_value"))

fit$obj$env$parList()

a <- fit$sdrep
a$sd
fit$obj$env$parList()

ParHat = fit$obj$env$parList()

ParHat$x_tj[,1]

AIC(fit)

predict(fit)


# sample-based quantile residuals
samples = loo_residuals(fit, what="samples", track_progress=FALSE)
which_use = which(!is.na(tsdata[,1:4]))
fitResp = loo_residuals( fit, what="loo", track_progress=FALSE)[,'est']
simResp = apply(samples, MARGIN=3, FUN=as.vector)[which_use,]

# Build and display DHARMa object
res = DHARMa::createDHARMa(
  simulatedResponse = simResp,
  observedResponse = unlist(tsdata[,1:4])[which_use],
  fittedPredictedResponse = fitResp[which_use] )
plot(res)












