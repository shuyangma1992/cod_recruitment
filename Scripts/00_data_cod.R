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

# 1 cod data cross-correlation --------------------------------------------
cod <- read_excel("Data/NEA_cod_SSB_age1_2_3.xlsx") %>%
  rename(
    "year" = 1,
    "rec" = 2, "rec_low" = 3, "rec_up" = 4,
    "ssb" = 5, "ssb_low" = 6, "ssb_up" = 7,
    "age1" = 8, "age2" = 9, "age3" = 10
  )


# save data
write_rds(cod, "Data/cod.rds")

# cross correlation
ccf(cod$ssb, cod$age1, na.action = na.pass) # lag = 1, weak correlation
ccf(cod$age1, cod$age2, na.action = na.pass) # lag = 1, intermediate correlation
ccf(cod$age2, cod$rec, na.action = na.pass) # lag = 1, intermediate correlation

# data after 1980
cod <- cod %>%
  filter(year >= 1981)

# plot
f_rec <- ggplot(cod, aes(x = year, y = rec / 1000000)) +
  geom_hline(aes(yintercept = mean(rec / 1000000, na.rm = TRUE)), color = mypal[4], linetype = "dashed") +
  geom_ribbon(aes(ymax = rec_up / 1000000, ymin = rec_low / 1000000), alpha = 0.25) +
  geom_line() +
  geom_point() +
  # ggtitle("Barents Sea sea surface temperature") +
  annotate("text",
    label = "NEA cod recruitment",
    x = 1980, hjust = 0.05, y = Inf, vjust = 1,
    family = "Arial"
  ) +
  scale_x_continuous("Year", limits = c(1980, 2022)) +
  scale_y_continuous("Recruitment (billion)") +
  mytheme

f_ssb <- ggplot(cod, aes(x = year, y = ssb / 1000000)) +
  geom_hline(aes(yintercept = mean(ssb / 1000000, na.rm = TRUE)), color = mypal[4], linetype = "dashed") +
  geom_ribbon(aes(ymax = ssb_up / 1000000, ymin = ssb_low / 1000000), alpha = 0.25) +
  geom_line() +
  geom_point() +
  # ggtitle("Barents Sea sea surface temperature") +
  annotate("text",
    label = "NEA cod spawning stock biomass",
    x = 1980, hjust = 0.05, y = Inf, vjust = 1,
    family = "Arial"
  ) +
  scale_x_continuous("Year", limits = c(1980, 2022)) +
  scale_y_continuous("SSB (million ton)") +
  mytheme

f_age1 <- ggplot(cod, aes(x = year, y = age1 / 1000)) +
  # geom_ribbon(aes(ymax = ssb_up / 1000000, ymin = ssb_low / 1000000), alpha = 0.25) +
  geom_hline(aes(yintercept = mean(age1 / 1000, na.rm = TRUE)), color = mypal[4], linetype = "dashed") +
  geom_line() +
  geom_point() +
  # ggtitle("Barents Sea sea surface temperature") +
  annotate("text",
    label = "NEA cod 1 year old",
    x = 1980, hjust = 0.05, y = Inf, vjust = 1,
    family = "Arial"
  ) +
  scale_x_continuous("Year", limits = c(1980, 2022)) +
  scale_y_continuous("Number (billion)") +
  mytheme

f_age2 <- ggplot(cod, aes(x = year, y = age2 / 1000)) +
  # geom_ribbon(aes(ymax = ssb_up / 1000000, ymin = ssb_low / 1000000), alpha = 0.25) +
  geom_hline(aes(yintercept = mean(age2 / 1000, na.rm = TRUE)), color = mypal[4], linetype = "dashed") +
  geom_line() +
  geom_point() +
  # ggtitle("Barents Sea sea surface temperature") +
  annotate("text",
    label = "NEA cod 2 year old",
    x = 1980, hjust = 0.05, y = Inf, vjust = 1,
    family = "Arial"
  ) +
  scale_x_continuous("Year", limits = c(1980, 2022)) +
  scale_y_continuous("Number (billion)") +
  mytheme

ggarrange(f_ssb, f_age1, f_age2, f_rec,
          ncol = 1, align = "hv", labels = c("a", "b", "c", "d"), font.label = list(family = "Arial")
)

ggsave("Figures/nea_cod.pdf", device = cairo_pdf, width = 4, height = 8)




# plot together
# # scale
# cod_scaled <- cod 
# cod_scaled[,2:10] <- scale(cod_scaled[,2:10])
# 
# # prepare data for plot
# cod_scaled <- cod_scaled %>%
#   pivot_longer(-year, names_to = "term", values_to = "value")
# 
# cod_scaled <- cod_scaled %>%
#   separate(term, into = c("term", "type"))
# 
# cod_scaled <- cod_scaled %>%
#   pivot_wider(names_from = type, values_from = value) %>%
#   rename(mean = 3)
# 
# cod_scaled <- cod_scaled %>%
#   filter(!term == "age3") %>%
#   mutate(term = factor(term, levels = c("ssb", "age1", "age2", "rec")))

# ggplot(cod_scaled, aes(x = year, y = mean)) +
#   geom_line(aes(colour = term, linetype = term, linewidth = term)) +
#   ggtitle("NEA cod") +
#   scale_color_manual(
#     name = "Time series", labels = c("SSB", "Age 1", "Age 2", "Recruitment"),
#     values = c("ssb" = mypal[1], "age1" = mypal[2], "age2" = mypal[3], "rec" = "black")
#   ) +
#   scale_linetype_manual(
#     name = "Time series", labels = c("SSB", "Age 1", "Age 2", "Recruitment"),
#     values = c("ssb" = "solid", "age1" = "dotted", "age2" = "dashed", "rec" = "solid")
#   ) +
#   scale_linewidth_manual(
#     name = "Time series", labels = c("SSB", "Age 1", "Age 2", "Recruitment"),
#     values = c("ssb" = 1, "age1" = 0.5, "age2" = 0.5, "rec" = 0.75)
#   ) +
#   scale_x_continuous("Year") +
#   scale_y_continuous("Scaled value") +
#   mytheme
# 
# ggsave("Figures/nea_cod.pdf", device = cairo_pdf, width = 6, height = 3)














# 2 cod recruitment success -----------------------------------------------
# read data
cod <- read_rds("Data/cod.rds")

# SSB lag 3 years
cod <- cod %>% 
  mutate(ssb = lag(ssb, 3),
         ssb_low = lag(ssb_low, 3),
         ssb_up = lag(ssb_up, 3))

cod <- cod %>% 
  select(1:7) %>% 
  na.omit

# calculate recruitment success
cod_rs <- NULL
for (i in 1:1000) {
  
  # i=1
  set.seed(i)
  
  cod_loop <- cod %>% 
    rowwise() %>% 
    mutate(rec_boot = exp(rnorm(1, mean = log(rec), sd = (log(rec_up)-log(rec_low))/4)),
           ssb_boot = exp(rnorm(1, mean = log(ssb), sd = (log(ssb_up)-log(ssb_low))/4)))
  
  cod_rs_loop <- cod_loop %>% 
    mutate(rs = rec_boot/ssb_boot,
           loop = i)
  
  cod_rs <- bind_rows(cod_rs, cod_rs_loop)
  print(i)
  
}

cod_rs_summary <- cod_rs %>% 
  group_by(year) %>% 
  summarise(rs_mean = mean(rs),
            rs_median = median(rs),
            rs_025 = quantile(rs, probs = 0.025),
            rs_100 = quantile(rs, probs = 0.100),
            rs_250 = quantile(rs, probs = 0.250),
            rs_750 = quantile(rs, probs = 0.750),
            rs_900 = quantile(rs, probs = 0.900),
            rs_975 = quantile(rs, probs = 0.975))

# plot
f_rs <- ggplot(cod_rs_summary) +
  geom_ribbon(aes(x = year, ymax = rs_975, ymin = rs_025), fill = mypal[9], alpha = 0.10) +
  geom_ribbon(aes(x = year, ymax = rs_900, ymin = rs_100), fill = mypal[9], alpha = 0.20) +
  geom_ribbon(aes(x = year, ymax = rs_750, ymin = rs_250), fill = mypal[9], alpha = 0.30) +
  geom_point(aes(x = year, y = rs_mean), pch = 1, color = mypal[7]) +
  geom_point(aes(x = year, y = rs_median), pch = 2, color = mypal[7]) +
  # ggtitle("Barents Sea sea surface temperature") +
  annotate("text",
           label = "NEA cod recruitment success",
           x = 1950, hjust = 0.05, y = Inf, vjust = 1,
           family = "Arial"
  ) +
  scale_x_continuous("Year", breaks = seq(1950, 2020, 10)) +
  scale_y_continuous("Recruitment success (thousand/ton)") +
  mytheme

ggsave("Figures/cod_recruitment_success.pdf", device = cairo_pdf, width = 5, height = 3)

mean(filter(cod_rs_summary, year >= 1981)$rs_mean)

max(filter(cod_rs_summary, year >= 1981)$rs_mean)


# 3 Age1 Age2 amd R Success ---------------------------------------------------------------
# read data
cod <- read_rds("Data/cod.rds")

# SSB lag 3 years
cod <- cod %>% 
  mutate(ssb_age1 = lag(ssb, 1),
         ssb_age1_low = lag(ssb_low, 1),
         ssb_age1_up = lag(ssb_up, 1),
         ssb_age2 = lag(ssb, 2),
         ssb_age2_low = lag(ssb_low, 2),
         ssb_age2_up = lag(ssb_up, 2),
         ssb_r = lag(ssb, 3),
         ssb_r_low = lag(ssb_low, 3),
         ssb_r_up = lag(ssb_up, 3))

cod <- cod %>%
  filter(year >= 1981)

# calculate recruitment success
cod_age1_age2_r_s <- NULL
for (i in 1:1000) {
  
  # i=1
  set.seed(i)
  
  cod_loop <- cod %>% 
    rowwise() %>% 
    mutate(rec_boot = exp(rnorm(1, mean = log(rec), sd = (log(rec_up)-log(rec_low))/4)),
           ssb_age1_boot = exp(rnorm(1, mean = log(ssb_age1), sd = (log(ssb_age1_up)-log(ssb_age1_low))/4)),
           ssb_age2_boot = exp(rnorm(1, mean = log(ssb_age2), sd = (log(ssb_age2_up)-log(ssb_age2_low))/4)),
           ssb_r_boot = exp(rnorm(1, mean = log(ssb_r), sd = (log(ssb_r_up)-log(ssb_r_low))/4)))
  
  cod_age1_age2_r_s_loop <- cod_loop %>% 
    mutate(age1_s = age1*1000/ssb_age1_boot,
           age2_s = age2*1000/ssb_age2_boot,
           r_s = rec_boot/ssb_r_boot,
           loop = i)
  
  cod_age1_age2_r_s <- bind_rows(cod_age1_age2_r_s, cod_age1_age2_r_s_loop)
  print(i)
  
}

cod_age1_age2_r_s_summary <- cod_age1_age2_r_s %>% 
  group_by(year) %>% 
  summarise(r_s_mean = mean(r_s),
            r_s_median = median(r_s),
            r_s_025 = quantile(r_s, probs = 0.025),
            r_s_100 = quantile(r_s, probs = 0.100),
            r_s_250 = quantile(r_s, probs = 0.250),
            r_s_750 = quantile(r_s, probs = 0.750),
            r_s_900 = quantile(r_s, probs = 0.900),
            r_s_975 = quantile(r_s, probs = 0.975),
            age1_s_mean = mean(age1_s),
            age1_s_median = median(age1_s),
            age1_s_025 = quantile(age1_s, probs = 0.025, na.rm = TRUE),
            age1_s_100 = quantile(age1_s, probs = 0.100, na.rm = TRUE),
            age1_s_250 = quantile(age1_s, probs = 0.250, na.rm = TRUE),
            age1_s_750 = quantile(age1_s, probs = 0.750, na.rm = TRUE),
            age1_s_900 = quantile(age1_s, probs = 0.900, na.rm = TRUE),
            age1_s_975 = quantile(age1_s, probs = 0.975, na.rm = TRUE),
            age2_s_mean = mean(age2_s),
            age2_s_median = median(age2_s),
            age2_s_025 = quantile(age2_s, probs = 0.025, na.rm = TRUE),
            age2_s_100 = quantile(age2_s, probs = 0.100, na.rm = TRUE),
            age2_s_250 = quantile(age2_s, probs = 0.250, na.rm = TRUE),
            age2_s_750 = quantile(age2_s, probs = 0.750, na.rm = TRUE),
            age2_s_900 = quantile(age2_s, probs = 0.900, na.rm = TRUE),
            age2_s_975 = quantile(age2_s, probs = 0.975, na.rm = TRUE))


# plot
f_r_s <- ggplot(cod_age1_age2_r_s_summary) +
  geom_ribbon(aes(x = year, ymax = r_s_975, ymin = r_s_025), fill = mypal[9], alpha = 0.10) +
  geom_ribbon(aes(x = year, ymax = r_s_900, ymin = r_s_100), fill = mypal[9], alpha = 0.20) +
  geom_ribbon(aes(x = year, ymax = r_s_750, ymin = r_s_250), fill = mypal[9], alpha = 0.30) +
  geom_point(aes(x = year, y = r_s_mean), pch = 1, color = mypal[7]) +
  geom_point(aes(x = year, y = r_s_median), pch = 2, color = mypal[7]) +
  # ggtitle("Barents Sea sea surface temperature") +
  annotate("text",
           label = "NEA cod recruitment success",
           x = 1980, hjust = 0.05, y = Inf, vjust = 1,
           family = "Arial"
  ) +
  scale_x_continuous("Year", breaks = seq(1980, 2020, 10)) +
  scale_y_continuous("Recruitment success (thousand/ton)") +
  mytheme

f_age1_s <- ggplot(cod_age1_age2_r_s_summary) +
  geom_ribbon(aes(x = year, ymax = age1_s_975, ymin = age1_s_025), fill = mypal[9], alpha = 0.10) +
  geom_ribbon(aes(x = year, ymax = age1_s_900, ymin = age1_s_100), fill = mypal[9], alpha = 0.20) +
  geom_ribbon(aes(x = year, ymax = age1_s_750, ymin = age1_s_250), fill = mypal[9], alpha = 0.30) +
  geom_point(aes(x = year, y = age1_s_mean), pch = 1, color = mypal[7]) +
  geom_point(aes(x = year, y = age1_s_median), pch = 2, color = mypal[7]) +
  # ggtitle("Barents Sea sea surface temperature") +
  annotate("text",
           label = "NEA cod age1 success",
           x = 1980, hjust = 0.05, y = Inf, vjust = 1,
           family = "Arial"
  ) +
  scale_x_continuous("Year", breaks = seq(1980, 2020, 10)) +
  scale_y_continuous("Age1 success (thousand/ton)") +
  mytheme

f_age2_s <- ggplot(cod_age1_age2_r_s_summary) +
  geom_ribbon(aes(x = year, ymax = age2_s_975, ymin = age2_s_025), fill = mypal[9], alpha = 0.10) +
  geom_ribbon(aes(x = year, ymax = age2_s_900, ymin = age2_s_100), fill = mypal[9], alpha = 0.20) +
  geom_ribbon(aes(x = year, ymax = age2_s_750, ymin = age2_s_250), fill = mypal[9], alpha = 0.30) +
  geom_point(aes(x = year, y = age2_s_mean), pch = 1, color = mypal[7]) +
  geom_point(aes(x = year, y = age2_s_median), pch = 2, color = mypal[7]) +
  # ggtitle("Barents Sea sea surface temperature") +
  annotate("text",
           label = "NEA cod age2 success",
           x = 1980, hjust = 0.05, y = Inf, vjust = 1,
           family = "Arial"
  ) +
  scale_x_continuous("Year", breaks = seq(1980, 2020, 10)) +
  scale_y_continuous("Age2 success (thousand/ton)") +
  mytheme

ggarrange(f_age1_s, f_age2_s, f_r_s,
          ncol = 1, align = "hv", labels = c("e", "f", "g"), font.label = list(family = "Arial")
)


ggsave("Figures/nea_cod_success.pdf", device = cairo_pdf, width = 4, height = 6)





