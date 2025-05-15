library(tidyverse)
library(readxl)
library(dsem)
library(phylopath)
library(psych)
library(ggdist)
library(ggpubr)

# load function
load("Utils/generate_candidate_data.R")

# plot setting
theme_set(theme_classic()) # theme
windowsFonts(A = windowsFont("Times New Roman"), B = windowsFont("Arial")) # font
# mypal <- pal_lancet()(9) # colour
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

# 1 Candidate time series data-------------------------------------------------------------
# Best DSEM
model_selection <- read_rds("Results/DESM_model_selection.rds")
best_dsem <- model_selection$model

# loop 1000 times, use function get candidate data, run DSEM, save parameters (estimate and p)
results <- NULL
for (i in 1:1000) {
  # i=1
  # set seed for reproduce
  set.seed(i)

  # generate candidate data
  data_candidate <- generate_candidate_data()

  # rename the variables
  data_candidate <- data_candidate %>%
    rename(
      rec = rec_candidate,
      ssb = ssb_candidate,
      mackerel_ssb = mackerel_ssb_candidate,
      herring_tsb = herring_tsb_candidate,
      fm = fm_candidate
    )

  # 3-year-moving average
  data_candidate <- as.data.frame(zoo::rollmean(data_candidate, 3))

  # change data to tsdata
  tsdata_candidate <- ts(scale(data_candidate)[, -1])

  # fit model
  fit <- dsem(
    sem = best_dsem,
    tsdata = tsdata_candidate,
    estimate_delta0 = FALSE,
    control = dsem_control(quiet = TRUE)
  )

  # results
  results_loop <- summary(fit) %>%
    mutate(loop_number = i)

  # save results
  results <- bind_rows(results, results_loop)

  # print(i)
  message(paste("This is my fucking stupid loop function, there are still", 1000 - i, "left.", "Time", Sys.time()))
}

#save results
write_rds(results, "Results/Sensitivity_analysis_results_candidate_data.rds")


# 2 Results figure --------------------------------------------------------
# read results
results <- read_rds("Results/Sensitivity_analysis_results_candidate_data.rds") %>%
  filter(direction == 1)

results <- results %>%
  mutate(path = factor(path, levels = rev(c(
    "ssb -> age1",
    "age1 -> age2",
    "age2 -> rec",
    "sst -> ssb",
    "sst -> age1",
    "sst -> rec",
    "atlantic_inflow -> sst",
    "climate_change -> sst",
    "amo -> atlantic_inflow",
    "mackerel_ssb -> age1",
    "capelin_tsb -> ssb",
    "B0SUM -> age1",
    "tfi_age1_winter -> age1",
    "tfi_age3_autumn -> rec",
    "fm -> ssb"
  ))))

# calculate mean and confidence intervals
results_summary <- results %>%
  group_by(path, lag, name) %>%
  summarise(
    estimate_mean = mean(Estimate),
    estimate_0.005 = quantile(Estimate, 0.005),
    estimate_0.025 = quantile(Estimate, 0.025),
    estimate_0.250 = quantile(Estimate, 0.250),
    estimate_0.750 = quantile(Estimate, 0.750),
    estimate_0.975 = quantile(Estimate, 0.975),
    estimate_0.995 = quantile(Estimate, 0.995),
    p_value_0.950 = quantile(p_value, 0.950),
    p_value_0.990 = quantile(p_value, 0.990)
  )

theme_set(theme_bw()) # theme

# figure estimate
f_estimate <- ggplot(results, aes(x = path, y = Estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_halfeye(
    fill_type = "segments", alpha = 0.5, 
    height = 1, scale = 0.8, normalize = "groups",
    show.legend = FALSE
  ) +
  stat_interval(show.legend = FALSE) +
  stat_summary(geom = "point", fun = median) +
  scale_x_discrete("Pathway", labels = rev(c(
    "SSB -> Age1 (1)",
    "Age1 -> Age2 (1)",
    "Age2 -> R (1)",
    "SST -> SSB (0)",
    "SST -> Age1 (0)",
    "SST -> R (0)",
    "AI -> SST (1)",
    "CC -> SST (0)",
    "AMO -> AI (2)",
    "Mackerel -> Age1 (0)",
    "Capelin -> SSB (0)",
    "MesoZ -> Age1 (0)",
    "TFI_Win -> Age1 (0)",
    "TFI_Aut -> R (0)",
    "FM -> SSB (0)"
  ))) +
  scale_y_continuous("Coefficient") +
  scale_color_manual(values = MetBrewer::met.brewer("VanGogh3")) +
  coord_flip(clip = "off") +
  mytheme +
  theme(panel.grid.major =  element_line(color = "grey", linetype = "dashed", linewidth = 0.25),
        panel.grid.minor =  element_blank(),
        axis.ticks = element_blank())

# figure p 
f_p <- ggplot(results, aes(x = path, y = p_value)) +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  stat_halfeye(
    fill_type = "segments", alpha = 0.5,
    height = 1, scale = 0.8, normalize = "groups",
    show.legend = FALSE
  ) +
  stat_interval(show.legend = FALSE) +
  stat_summary(geom = "point", fun = median) +
  scale_x_discrete("Pathway", labels = rev(c(
    "SSB -> Age1 (1)",
    "Age1 -> Age2 (1)",
    "Age2 -> R (1)",
    "SST -> SSB (0)",
    "SST -> Age1 (0)",
    "SST -> R (0)",
    "AI -> SST (1)",
    "CC -> SST (0)",
    "AMO -> AI (2)",
    "Mackerel -> Age1 (0)",
    "Capelin -> SSB (0)",
    "MesoZ -> Age1 (0)",
    "TFI_Win -> Age1 (0)",
    "TFI_Aut -> R (0)",
    "FM -> SSB (0)"
  ))) +
  scale_y_continuous("P value", limits = c(0, 0.055)) +
  scale_color_manual(values = MetBrewer::met.brewer("OKeeffe2")) +
  coord_flip(clip = "off") +
  mytheme+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())+
  theme(panel.grid.major =  element_line(color = "grey", linetype = "dashed", linewidth = 0.25),
        panel.grid.minor =  element_blank(),
        axis.ticks = element_blank())

#combine two figures
ggarrange(f_estimate, f_p, ncol = 2, align = "h", widths = c(2.5, 1))

ggsave("Figures/sensitivity_uncertatinty.pdf", device = cairo_pdf, width = 6, height = 5)









