library(tidyverse)
library(readxl)
library(dsem)
library(phylopath)
library(psych)
library(devtools)
library(scales)
library(viridis)
library(ggsci)
library(ggpubr)
library(foreach)
library(doParallel)

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



# 2 Best DSEM fit ---------------------------------------------------------
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


# 3 Scenarios --------------------------------------------------------
# reference data, the means after 2010
reference <- data %>% 
  filter(year >= 2010) %>% 
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))

# Scenario seting
# all scenarios are based on the reference (mean values after 2010)
# SST scenarios: increase or decrease to a maximum of 30%, by 1%
# Mesozooplankton: increase or decrease to a maximum of 30%, by 1%
# In total 61*61 combinations
# reference sst and mesoz
sst_reference <- reference$sst
mesoz_reference <- reference$B0SUM
fm_reference <- reference$fm

# all the scenarios
sst_scenarios <- sst_reference * (1 + seq(-0.3, 0.3, 0.03))
mesoz_scenarios <- mesoz_reference * (1 + seq(-0.3, 0.3, 0.03))
fm_scenarios <- fm_reference * (1 + seq(-0.3, 0.3, 0.03))
scenarios <- expand.grid(sst_scenarios, mesoz_scenarios, fm_scenarios)

# make new data for prediction
data_prediction <- reference %>% 
  slice(rep(1, each = 9261)) # a little bit crasy
data_prediction$sst <- scenarios$Var1
data_prediction$B0SUM <- scenarios$Var2
data_prediction$fm <- scenarios$Var3

# scale new data with old scales
scale_center <- as.data.frame(t(attr(scale(data), 'scaled:center'))) %>% 
  slice(rep(1, each = 9261))
scale_scale <- as.data.frame(t(attr(scale(data), 'scaled:scale'))) %>% 
  slice(rep(1, each = 9261))
data_prediction <- (data_prediction - scale_center) / scale_scale

# set the life history part as NA
data_prediction$ssb <- NA
data_prediction$age1 <- NA
data_prediction$age2 <- NA
data_prediction$rec <- NA

data_prediction <- bind_rows(as.data.frame(scale(data)), data_prediction)

# 4 DSEM prediction ssb--------------------------------------------------------
# estimate function
estimate_ssb <- function(i) {
  
  # i=1
  data_prediction_loop <- data_prediction %>% 
    slice(c(1:40, i+40)) 
  data_prediction_loop$ssb[40] <- mean(data_prediction_loop$ssb[29:40], na.rm = T)
  data_prediction_loop$age1[40] <- mean(data_prediction_loop$ssb[29:40], na.rm = T)
  data_prediction_loop$age2[40] <- mean(data_prediction_loop$ssb[29:40], na.rm = T)
  data_prediction_loop$rec[40] <- mean(data_prediction_loop$ssb[29:40], na.rm = T)
  prediction_results_loop <- predict(fit, newdata = ts(data_prediction_loop[, -1]), type = "response")
  colnames(prediction_results_loop) <- colnames(data)[-1]
  return(prediction_results_loop[41,])
  
}

# run paralell
# how many cores can be used
detectCores()

# use 8 cores
cl <- makeCluster(getOption("cl.cores", 4))

# register cores
registerDoParallel(cl)

# run
prediction_results <- foreach(i = c(1:9261),
                              .combine = "rbind",
                              .packages = c("tidyverse", "dsem")) %dopar% {
                                
                                # i=1
                                try(estimate_ssb(i))
                                
                                }

# stop cluster
stopCluster(cl)

saveRDS(prediction_results, "Results/DSEM_prediction_1_includeF.rds")

# 5 DSEM prediction age1 --------------------------------------------------
# run section 1, 2, read results from section 4
data_prediction <- read_rds("Results/DSEM_prediction_1_includeF.rds")

# set rec age1 and age2 as NA
data_prediction$age1 <- NA
data_prediction$age2 <- NA
data_prediction$rec <- NA

# combine data
data_prediction <- bind_rows(as.data.frame(scale(data)), data_prediction)

# estimate function
estimate_age1 <- function(i) {
  
  data_prediction_loop <- data_prediction %>% 
    slice(c(1:40, i+40)) 
  data_prediction_loop$age1[40] <- mean(data_prediction_loop$ssb[29:40], na.rm = T)
  data_prediction_loop$age2[40] <- mean(data_prediction_loop$ssb[29:40], na.rm = T)
  data_prediction_loop$rec[40] <- mean(data_prediction_loop$ssb[29:40], na.rm = T)
  data_prediction_loop <- data_prediction_loop %>% 
    mutate(ssb=lead(ssb, 1)) # because ssb had a lag 1 year effect on age 1
  prediction_results_loop <- predict(fit, newdata = ts(data_prediction_loop[, -1]), type = "response")
  colnames(prediction_results_loop) <- colnames(data)[-1]
  return(prediction_results_loop[41,])
  print(i)
  
}

# run paralell
# how many cores can be used
detectCores()

# use 8 cores
cl <- makeCluster(getOption("cl.cores", 4))

# register cores
registerDoParallel(cl)

# run
prediction_results <- foreach(i = c(1:9261),
                              .combine = "rbind",
                              .packages = c("tidyverse", "dsem")) %dopar% {
                                
                                # i=1
                                try(estimate_age1(i))
                                
                                }

# stop cluster
stopCluster(cl)

saveRDS(prediction_results, "Results/DSEM_prediction_2_includeF.rds")

# 6 DSEM prediction age2 --------------------------------------------------
# run section 1, 2, read results from section 5
data_prediction <- read_rds("Results/DSEM_prediction_2_includeF.rds")

# set age2 and rec as NA
data_prediction$age2 <- NA
data_prediction$rec <- NA

# combine data
data_prediction <- bind_rows(as.data.frame(scale(data)), data_prediction)

# estimate function
estimate_age2 <- function(i) {
  
  data_prediction_loop <- data_prediction %>% 
    slice(c(1:40, i+40)) 
  data_prediction_loop$age2[40] <- mean(data_prediction_loop$ssb[29:40], na.rm = T)
  data_prediction_loop$rec[40] <- mean(data_prediction_loop$ssb[29:40], na.rm = T)
  data_prediction_loop <- data_prediction_loop %>% 
    mutate(age1=lead(age1, 1)) # because ssb had a lag 1 year effect on age 1
  prediction_results_loop <- predict(fit, newdata = ts(data_prediction_loop[, -1]), type = "response")
  colnames(prediction_results_loop) <- colnames(data)[-1]
  return(prediction_results_loop[41,])
  print(i)
  
}

# run paralell
# how many cores can be used
detectCores()

# use 8 cores
cl <- makeCluster(getOption("cl.cores", 4))

# register cores
registerDoParallel(cl)

# run
prediction_results <- foreach(i = c(1:9261),
                              .combine = "rbind",
                              .packages = c("tidyverse", "dsem")) %dopar% {
                                
                                # i=1
                                try(estimate_age2(i))
                              
                              }

# stop cluster
stopCluster(cl)

saveRDS(prediction_results, "Results/DSEM_prediction_3_includeF.rds")


# 7 DSEM prediction rec --------------------------------------------------
# run section 1, 2, read results from section 6
data_prediction <- read_rds("Results/DSEM_prediction_3_includeF.rds")

# set age2 and rec as NA
data_prediction$rec <- NA

# combine data
data_prediction <- bind_rows(as.data.frame(scale(data)), data_prediction)

# function
estimate_rec <- function(i) {
  
  data_prediction_loop <- data_prediction %>% 
    slice(c(1:40, i+40)) 
  data_prediction_loop$rec[40] <- mean(data_prediction_loop$ssb[29:40], na.rm = T)
  data_prediction_loop <- data_prediction_loop %>% 
    mutate(age2=lead(age2, 1)) # because age2 had a lag 1 year effect on rec
  prediction_results_loop <- predict(fit, newdata = ts(data_prediction_loop[, -1]), type = "response")
  colnames(prediction_results_loop) <- colnames(data)[-1]
  return(prediction_results_loop[41,])
  print(i)
  
}

# run paralell
# how many cores can be used
detectCores()

# use 8 cores
cl <- makeCluster(getOption("cl.cores", 4))

# register cores
registerDoParallel(cl)

# run
prediction_results <- foreach(i = c(1:9261),
                              .combine = "rbind",
                              .packages = c("tidyverse", "dsem")) %dopar% {
                                
                                # i=1
                                try(estimate_rec(i))
                          
                              }

# stop cluster
stopCluster(cl)


saveRDS(prediction_results, "Results/DSEM_prediction_4_includeF.rds")


# 8 Plot ------------------------------------------------------------------
theme_set(theme_bw()) # theme
# predicted results
predicted_results_ssb <- as.data.frame(read_rds("Results/DSEM_prediction_1_includeF.rds")) %>% 
  select(sst, B0SUM, fm, ssb)
predicted_results_age1 <- as.data.frame(read_rds("Results/DSEM_prediction_2_includeF.rds")) %>% 
  select(sst, B0SUM, fm, age1)
predicted_results_age2 <- as.data.frame(read_rds("Results/DSEM_prediction_3_includeF.rds")) %>% 
  select(sst, B0SUM, fm, age2)
predicted_results_rec <- as.data.frame(read_rds("Results/DSEM_prediction_4_includeF.rds")) %>% 
  select(sst, B0SUM, fm, rec)

predicted_results <- left_join(predicted_results_ssb, predicted_results_age1) %>%
  left_join(predicted_results_age2) %>%
  left_join(predicted_results_rec)

#back scale
predicted_results$ssb <- predicted_results$ssb*attr(scale(data), 'scaled:scale')[3] + 
  attr(scale(data), 'scaled:center')[3]
predicted_results$age1 <- predicted_results$age1*attr(scale(data), 'scaled:scale')[4] + 
  attr(scale(data), 'scaled:center')[4]
predicted_results$age2 <- predicted_results$age2*attr(scale(data), 'scaled:scale')[5] + 
  attr(scale(data), 'scaled:center')[5]
predicted_results$rec <- predicted_results$rec*attr(scale(data), 'scaled:scale')[2] + 
  attr(scale(data), 'scaled:center')[2]
predicted_results$sst <- predicted_results$sst*attr(scale(data), 'scaled:scale')[6] + 
  attr(scale(data), 'scaled:center')[6]
predicted_results$B0SUM <- predicted_results$B0SUM*attr(scale(data), 'scaled:scale')[24] + 
  attr(scale(data), 'scaled:center')[24]
predicted_results$fm <- predicted_results$fm*attr(scale(data), 'scaled:scale')[25] + 
  attr(scale(data), 'scaled:center')[25]
# reference data, the means after 2010
reference <- data %>% 
  filter(year >= 2010) %>% 
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))


#calculate ratio to the reference
predicted_results$sst <- predicted_results$sst/reference$sst-1
predicted_results$B0SUM <- predicted_results$B0SUM/reference$B0SUM-1
predicted_results$fm <- predicted_results$fm/reference$fm-1

#calculate ratio to the predicted reference
reference_predicted <- filter(predicted_results, sst==0&B0SUM==0&fm==0)
predicted_results$ssb <- predicted_results$ssb/reference_predicted$ssb-1
predicted_results$age1 <- predicted_results$age1/reference_predicted$age1-1
predicted_results$age2 <- predicted_results$age2/reference_predicted$age2-1
predicted_results$rec <- predicted_results$rec/reference_predicted$rec-1

# possible changes in the late 2020s
# temperature: -12%-4%
# mesozooplankton: -31%-27%
# fishing mortality: 8%-45%

change_in_late_2020s <- predicted_results %>% 
  filter(sst>=-0.12&sst<=0.04) %>% 
  filter(B0SUM>=-0.31&B0SUM<=0.27) %>% 
  filter(fm>=0.08&fm<=0.45)





# plot
library(ggtern)
f_ssb <- ggtern(predicted_results, aes(x = sst, y = B0SUM, z = fm, value = ssb)) +
  stat_interpolate_tern(
    geom = "polygon",
    formula = value ~ x + y,
    n = 100, method = "auto",
    breaks = seq(-0.4, 0.4, 0.01),
    aes(fill = ..level..), expand = T
  ) +
  scale_fill_gradient2(low = mypal[7], mid = "white", high = mypal[3], breaks = seq(-0.4, 0.4, 0.2)) +
  xlab("SST") + ylab("MesoZ") + zlab("FM") +
  scale_T_continuous(breaks = c(0, 1/6, 2/6, 3/6, 4/6, 5/6, 6/6), labels = c("-30%", "-20%", "-10%", "0%", "10%", "20%", "30%"),
                     minor_breaks = seq(0, 1, 1/12)) +
  scale_L_continuous(breaks = c(0, 1/6, 2/6, 3/6, 4/6, 5/6, 6/6), labels = c("-30%", "-20%", "-10%", "0%", "10%", "20%", "30%"),
                     minor_breaks = seq(0, 1, 1/12)) +
  scale_R_continuous(breaks = c(0, 1/6, 2/6, 3/6, 4/6, 5/6, 6/6), labels = c("-30%", "-20%", "-10%", "0%", "10%", "20%", "30%"),
                     minor_breaks = seq(0, 1, 1/12)) +
  theme_bvbg() +
  theme(tern.panel.grid.ontop = TRUE,
        legend.position = c(0.9, 0.9))

f_age1 <- ggtern(predicted_results, aes(x = sst, y = B0SUM, z = fm, value = age1)) +
  stat_interpolate_tern(
    geom = "polygon",
    formula = value ~ x + y,
    n = 100, method = "auto",
    breaks = seq(-1.4, 1.4, 0.01),
    aes(fill = ..level..), expand = T
  ) +
  scale_fill_gradient2(low = mypal[7], mid = "white", high = mypal[3], breaks = seq(-1.5, 1.5, 0.5)) +
  xlab("SST") + ylab("MesoZ") + zlab("FM") +
  scale_T_continuous(breaks = c(0, 1/6, 2/6, 3/6, 4/6, 5/6, 6/6), labels = c("-30%", "-20%", "-10%", "0%", "10%", "20%", "30%"),
                     minor_breaks = seq(0, 1, 1/12)) +
  scale_L_continuous(breaks = c(0, 1/6, 2/6, 3/6, 4/6, 5/6, 6/6), labels = c("-30%", "-20%", "-10%", "0%", "10%", "20%", "30%"),
                     minor_breaks = seq(0, 1, 1/12)) +
  scale_R_continuous(breaks = c(0, 1/6, 2/6, 3/6, 4/6, 5/6, 6/6), labels = c("-30%", "-20%", "-10%", "0%", "10%", "20%", "30%"),
                     minor_breaks = seq(0, 1, 1/12)) +
  theme_bvbg() +
  theme(tern.panel.grid.ontop = TRUE,
        legend.position = c(0.9, 0.9))

f_age2 <- ggtern(predicted_results, aes(x = sst, y = B0SUM, z = fm, value = age2)) +
  stat_interpolate_tern(
    geom = "polygon",
    formula = value ~ x + y,
    n = 100, method = "auto",
    breaks = seq(-0.6, 0.6, 0.01),
    aes(fill = ..level..), expand = T
  ) +
  scale_fill_gradient2(low = mypal[7], mid = "white", high = mypal[3], breaks = seq(-0.6, 0.6, 0.3)) +
  xlab("SST") + ylab("MesoZ") + zlab("FM") +
  scale_T_continuous(breaks = c(0, 1/6, 2/6, 3/6, 4/6, 5/6, 6/6), labels = c("-30%", "-20%", "-10%", "0%", "10%", "20%", "30%"),
                     minor_breaks = seq(0, 1, 1/12)) +
  scale_L_continuous(breaks = c(0, 1/6, 2/6, 3/6, 4/6, 5/6, 6/6), labels = c("-30%", "-20%", "-10%", "0%", "10%", "20%", "30%"),
                     minor_breaks = seq(0, 1, 1/12)) +
  scale_R_continuous(breaks = c(0, 1/6, 2/6, 3/6, 4/6, 5/6, 6/6), labels = c("-30%", "-20%", "-10%", "0%", "10%", "20%", "30%"),
                     minor_breaks = seq(0, 1, 1/12)) +
  theme_bvbg() +
  theme(tern.panel.grid.ontop = TRUE,
        legend.position = c(0.9, 0.9))

f_rec <- ggtern(predicted_results, aes(x = sst, y = B0SUM, z = fm, value = rec)) +
  stat_interpolate_tern(
    geom = "polygon",
    formula = value ~ x + y,
    n = 100, method = "auto",
    breaks = seq(-0.5, 0.5, 0.01),
    aes(fill = ..level..), expand = T
  ) +
  scale_fill_gradient2(low = mypal[7], mid = "white", high = mypal[3], breaks = seq(-0.4, 0.4, 0.2)) +
  xlab("SST") + ylab("MesoZ") + zlab("FM") +
  scale_T_continuous(breaks = c(0, 1/6, 2/6, 3/6, 4/6, 5/6, 6/6), labels = c("-30%", "-20%", "-10%", "0%", "10%", "20%", "30%"),
                     minor_breaks = seq(0, 1, 1/12)) +
  scale_L_continuous(breaks = c(0, 1/6, 2/6, 3/6, 4/6, 5/6, 6/6), labels = c("-30%", "-20%", "-10%", "0%", "10%", "20%", "30%"),
                     minor_breaks = seq(0, 1, 1/12)) +
  scale_R_continuous(breaks = c(0, 1/6, 2/6, 3/6, 4/6, 5/6, 6/6), labels = c("-30%", "-20%", "-10%", "0%", "10%", "20%", "30%"),
                     minor_breaks = seq(0, 1, 1/12)) +
  theme_bvbg() +
  theme(tern.panel.grid.ontop = TRUE,
        legend.position = c(0.9, 0.9))


ggarrange(f_ssb, f_age1, f_age2, f_rec, nrow = 2, ncol = 2, align = "hv")

ggsave("Figures/predictions.pdf", device = cairo_pdf, width = 7, height = 7)


# 9 3D plot ---------------------------------------------------------------
library(plotly)

# ssb
fig_3d_ssb <- plot_ly(predicted_results,
  x = ~sst, y = ~B0SUM, z = ~fm,  
  marker = list(color = ~ssb, 
                colorscale = list(c(0, 0.5, 1), c(mypal[7], "white", mypal[3])), 
                showscale = TRUE))  %>% 
  layout(title = "SSB",
         scene = list(xaxis = list(title = 'SST', 
                                   ticktext = list("-30%", "-20%", "-10%", "0%", "10%", "20%", "30%"),
                                   tickvals = list(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3)),
                      yaxis = list(title = 'MesoZ', 
                                   ticktext = list("-30%", "-20%", "-10%", "0%", "10%", "20%", "30%"),
                                   tickvals = list(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3)),
                      zaxis = list(title = 'FM', 
                                   ticktext = list("-30%", "-20%", "-10%", "0%", "10%", "20%", "30%"),
                                   tickvals = list(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3))))

# age1
fig_3d_age1 <- plot_ly(predicted_results,
                      x = ~sst, y = ~B0SUM, z = ~fm,  
                      marker = list(color = ~age1, 
                                    colorscale = list(c(0, 0.5, 1), c(mypal[7], "white", mypal[3])), 
                                    showscale = TRUE))  %>% 
  layout(title = "Age 1",
         scene = list(xaxis = list(title = 'SST', 
                                   ticktext = list("-30%", "-20%", "-10%", "0%", "10%", "20%", "30%"),
                                   tickvals = list(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3)),
                      yaxis = list(title = 'MesoZ', 
                                   ticktext = list("-30%", "-20%", "-10%", "0%", "10%", "20%", "30%"),
                                   tickvals = list(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3)),
                      zaxis = list(title = 'FM', 
                                   ticktext = list("-30%", "-20%", "-10%", "0%", "10%", "20%", "30%"),
                                   tickvals = list(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3))))

# age2
fig_3d_age2 <- plot_ly(predicted_results,
                       x = ~sst, y = ~B0SUM, z = ~fm,  
                       marker = list(color = ~age2, 
                                     colorscale = list(c(0, 0.5, 1), c(mypal[7], "white", mypal[3])), 
                                     showscale = TRUE))  %>% 
  layout(title = "Age 2",
         scene = list(xaxis = list(title = 'SST', 
                                   ticktext = list("-30%", "-20%", "-10%", "0%", "10%", "20%", "30%"),
                                   tickvals = list(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3)),
                      yaxis = list(title = 'MesoZ', 
                                   ticktext = list("-30%", "-20%", "-10%", "0%", "10%", "20%", "30%"),
                                   tickvals = list(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3)),
                      zaxis = list(title = 'FM', 
                                   ticktext = list("-30%", "-20%", "-10%", "0%", "10%", "20%", "30%"),
                                   tickvals = list(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3))))

# rec
fig_3d_rec <- plot_ly(predicted_results,
                      x = ~sst, y = ~B0SUM, z = ~fm,  
                      marker = list(color = ~rec, 
                                    colorscale = list(c(0, 0.5, 1), c(mypal[7], "white", mypal[3])), 
                                    showscale = TRUE))  %>% 
  layout(title = "R",
         scene = list(xaxis = list(title = 'SST', 
                                   ticktext = list("-30%", "-20%", "-10%", "0%", "10%", "20%", "30%"),
                                   tickvals = list(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3)),
                      yaxis = list(title = 'MesoZ', 
                                   ticktext = list("-30%", "-20%", "-10%", "0%", "10%", "20%", "30%"),
                                   tickvals = list(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3)),
                      zaxis = list(title = 'FM', 
                                   ticktext = list("-30%", "-20%", "-10%", "0%", "10%", "20%", "30%"),
                                   tickvals = list(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3))))


