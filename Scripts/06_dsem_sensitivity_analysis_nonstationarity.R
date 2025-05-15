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


# 2 Moving window analysis 1976 - 2007, 31-year moving window-------------------
# filter data
data <- data %>% 
  filter(year %in% c(1961:2022))

# best DSEM
model_selection <- read_rds("Results/DESM_model_selection.rds")
best_dsem <- model_selection$model

# moving window - 31 years, 1976 - 2007
results <- NULL
for (i in 1976:2007) {
  # i = 1982
  data_loop <- data %>%
    filter(year %in% c((i - 15):(i + 15)))

  # 3-year-moving average
  data_loop <- as.data.frame(zoo::rollmean(data_loop, 3))

  # change data to tsdata
  tsdata_loop <- ts(scale(data_loop)[, -1])

  # fit model
  fit <- try(dsem(
    sem = best_dsem,
    tsdata = tsdata_loop,
    estimate_delta0 = FALSE,
    control = dsem_control(quiet = TRUE)
  ))
  if (is.character(fit)) {
    next
  } else {
    # results
    results_loop <- summary(fit) %>%
      mutate(
        central_year = i
      )

    # save results
    results <- bind_rows(results, results_loop)
  }

  print(i)
}

# save results
write_rds(results, "Results/Sensitivity_analysis_results_moving_window.rds")


# 3 Results figure --------------------------------------------------------
results <- read_rds("Results/Sensitivity_analysis_results_moving_window.rds") %>%
  filter(direction == 1) %>%
  mutate(path = factor(path, levels = c(
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
  ))) %>% 
  mutate(significance = ifelse(p_value <= 0.05, "significant", "not significant"))

# figure a: life history
supp.labs <- c(
  "SSB -> Age1 (1)",
  "Age1 -> Age2 (1)",
  "Age2 -> R (1)"
)
names(supp.labs) <- c(
  "ssb -> age1",
  "age1 -> age2",
  "age2 -> rec"
)

# figure a: life history
theme_set(theme_bw()) # theme
fa <- ggplot(filter(results, path %in% c("ssb -> age1", 
                                         "age1 -> age2", 
                                         "age2 -> rec"))) +
  geom_linerange(aes(x = central_year, ymin = -Inf, ymax = Inf, color = significance),
                 linewidth = 0.75, show.legend = FALSE
  ) +
  scale_color_manual(values = c(
    "significant" = alpha("white", 0.25),
    "not significant" = alpha(mypal[5], 0.25)
  )) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(x = central_year, ymin = Estimate - 2 * Std_Error, ymax = Estimate + 2 * Std_Error),
              alpha = 0.125
  ) +
  geom_line(aes(x = central_year, y = Estimate)) +
  facet_wrap(~path, ncol = 3, labeller = labeller(path = supp.labs)) +
  scale_x_continuous("Central year (31-year moving window)", limits = c(1980, 2007)) +
  scale_y_continuous("Coefficient", breaks = c(-1, 0, 1)) +
  mytheme +
  theme(
    strip.text = element_text(family = "Arial"),
    strip.background = element_blank(),
    strip.clip = "off",
    panel.grid.major =  element_line(color = "grey", linetype = "dashed", linewidth = 0.25),
    panel.grid.minor =  element_blank(),
    axis.ticks = element_blank()
  )


# figure b: physical
supp.labs <- c(
  "SST -> SSB (0)",
  "SST -> Age1 (0)",
  "SST -> R (0)",
  "AI -> SST (1)",
  "CC -> SST (0)",
  "AMO -> AI (2)"
)
names(supp.labs) <- c(
  "sst -> ssb",
  "sst -> age1",
  "sst -> rec",
  "atlantic_inflow -> sst",
  "climate_change -> sst",
  "amo -> atlantic_inflow"
)

fb <- ggplot(filter(results, path %in% c("sst -> ssb", 
                                         "sst -> age1", 
                                         "sst -> rec",
                                         "atlantic_inflow -> sst", 
                                         "climate_change -> sst", 
                                         "amo -> atlantic_inflow"))) +
  geom_linerange(aes(x = central_year, ymin = -Inf, ymax = Inf, color = significance),
                 linewidth = 0.75, show.legend = FALSE
  ) +
  scale_color_manual(values = c(
    "significant" = alpha("white", 0.25),
    "not significant" = alpha(mypal[5], 0.25)
  )) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(x = central_year, ymin = Estimate - 2 * Std_Error, ymax = Estimate + 2 * Std_Error),
              alpha = 0.125, fill = mypal[2]
  ) +
  geom_line(aes(x = central_year, y = Estimate), color = mypal[2]) +
  facet_wrap(~path, ncol = 3, labeller = labeller(path = supp.labs)) +
  scale_x_continuous("Central year (31-year moving window)", limits = c(1980, 2007)) +
  scale_y_continuous("Coefficient", breaks = c(-1, 0, 1)) +
  # scale_color_manual(values = MetBrewer::met.brewer("OKeeffe2")) +
  mytheme +
  theme(
    strip.text = element_text(family = "Arial"),
    strip.background = element_blank(),
    strip.clip = "off",
    panel.grid.major =  element_line(color = "grey", linetype = "dashed", linewidth = 0.25),
    panel.grid.minor =  element_blank(),
    axis.ticks = element_blank()
  )


# figure c: biological
supp.labs <- c(
  "Mackerel -> Age1 (0)",
  "Capelin -> SSB (0)",
  "MesoZ -> Age1 (0)",
  "TFI_Win -> Age1 (0)",
  "TFI_Aut -> R (0)"
)
names(supp.labs) <- c(
  "mackerel_ssb -> age1",
  "capelin_tsb -> ssb",
  "B0SUM -> age1",
  "tfi_age1_winter -> age1",
  "tfi_age3_autumn -> rec"
)

fc <- ggplot(filter(results, path %in% c("mackerel_ssb -> age1", 
                                         "capelin_tsb -> ssb", 
                                         "B0SUM -> age1", 
                                         "tfi_age1_winter -> age1", 
                                         "tfi_age3_autumn -> rec"))) +
  geom_linerange(aes(x = central_year, ymin = -Inf, ymax = Inf, color = significance),
                 linewidth = 0.75, show.legend = FALSE
  ) +
  scale_color_manual(values = c(
    "significant" = alpha("white", 0.25),
    "not significant" = alpha(mypal[5], 0.25)
  )) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(x = central_year, ymin = Estimate - 2 * Std_Error, ymax = Estimate + 2 * Std_Error),
              alpha = 0.125, fill = mypal[3]
  ) +
  geom_line(aes(x = central_year, y = Estimate), color = mypal[3]) +
  facet_wrap(~path, ncol = 3, labeller = labeller(path = supp.labs)) +
  scale_x_continuous("Central year (31-year moving window)", limits = c(1980, 2007)) +
  scale_y_continuous("Coefficient", breaks = c(-1, 0, 1)) +
  # scale_color_manual(values = MetBrewer::met.brewer("OKeeffe2")) +
  mytheme +
  theme(
    strip.text = element_text(family = "Arial"),
    strip.background = element_blank(),
    strip.clip = "off",
    panel.grid.major =  element_line(color = "grey", linetype = "dashed", linewidth = 0.25),
    panel.grid.minor =  element_blank(),
    axis.ticks = element_blank()
  )

# figure d: human pressure
supp.labs <- c(
  "FM -> SSB (0)"
)
names(supp.labs) <- c(
  "fm -> ssb"
)

fd <- ggplot(filter(results, path %in% c("fm -> ssb"))) +
  geom_linerange(aes(x = central_year, ymin = -Inf, ymax = Inf, color = significance),
                 linewidth = 0.75, show.legend = FALSE
  ) +
  scale_color_manual(values = c(
    "significant" = alpha("white", 0.25),
    "not significant" = alpha(mypal[5], 0.25)
  )) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(x = central_year, ymin = Estimate - 2 * Std_Error, ymax = Estimate + 2 * Std_Error),
              alpha = 0.125, fill = mypal[1]
  ) +
  geom_line(aes(x = central_year, y = Estimate), color = mypal[1]) +
  facet_wrap(~path, ncol = 3, labeller = labeller(path = supp.labs)) +
  scale_x_continuous("Central year (31-year moving window)", limits = c(1980, 2007)) +
  scale_y_continuous("Coefficient", breaks = c(-1, 0, 1)) +
  # scale_color_manual(values = MetBrewer::met.brewer("OKeeffe2")) +
  mytheme +
  theme(
    strip.text = element_text(family = "Arial"),
    strip.background = element_blank(),
    strip.clip = "off",
    panel.grid.major =  element_line(color = "grey", linetype = "dashed", linewidth = 0.25),
    panel.grid.minor =  element_blank(),
    axis.ticks = element_blank()
  )

ggarrange(fa, fb, fc, fd,
          ncol = 1, heights = c(1.15,2,2,1.15), align = "hv")

ggsave("Figures/sensitivity_analytical_moving_window.pdf", device = cairo_pdf, width = 6, height = 9)







