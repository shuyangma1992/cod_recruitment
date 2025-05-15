# 1 Pearson correlation analysis considering autocorrelation --------------
# This a function about calculating correlation coefficients and their significance
# with consideration on autocorrealtion in time series.
# Effective degree of freedom is adjusted according to time series' autocorrelation,
# based on Pyper & Peterman, 1998.

# function
correlation_autocorrelation <- function(data) {
  require(zoo)

  r <- data.frame(NA) # correlation coefficients
  p <- data.frame(NA) # significance

  for (i in 1:length(data)) {
    for (k in 1:length(data)) {
      # i=1
      X <- zoo(data[, i])

      # k=3
      Y <- zoo(data[, k])

      # remove NA values and make them the same length
      missY <- is.na(Y)
      X[missY] <- NA
      missX <- is.na(X)
      Y[missX] <- NA
      X <- X[!is.na(X)]
      Y <- Y[!is.na(Y)]

      # basic information for time series
      N <- sum(!is.na(X)) # time series length
      J <- round(length(X) / 5) # autocorrelation orders

      rXX <- acf(X, na.action = na.pass, plot = F)$acf[2:(J + 1)] # autocorrelation coefficients of time series X
      rYY <- acf(Y, na.action = na.pass, plot = F)$acf[2:(J + 1)] # autocorrelation coefficients of time series Y

      # calculate effective degree of freedom
      cross.prod <- NA
      for (j in 1:J) {
        cross.prod[j] <- ((N - j) / N) * rXX[j] * rYY[j]
      }
      Nef <- 1 / ((1 / N) + (2 / N) * sum(cross.prod)) # effective degree of freedom
      if (Nef > N) Nef <- N # If it is greater than the time series length, set it equal to the time series length

      # calculate r and p
      r.ac <- cor(X, Y, use = "pair") # correlation coefficients
      t <- sqrt((Nef * r.ac^2) / (1 - r.ac^2)) # statistic t
      p.ac <- 2 * pt(-abs(t), Nef - 2) # significance

      # save results
      r[i, k] <- r.ac
      p[i, k] <- p.ac
    }
  }

  # change names
  dimnames(r) <- list(colnames(data), colnames(data))
  dimnames(p) <- list(colnames(data), colnames(data))

  return(list(r = r, p = p))
}

# save function
save(correlation_autocorrelation, file = "Utils/correlation_autocorrelation.R")

# function test
data <- data.frame(
  a = rnorm(100),
  b = rnorm(100),
  c = rnorm(100)
)
data$a[91:100] <- NA

correlation_autocorrelation(data)



# 2 cross-correlation figure ----------------------------------------------
# This a function about calculating cross-correlation coefficients with different time lags,
# and return the figure (my style).

cross_correlation_figure <- function(x, y) {
  
  require("tidyverse")
  require("ggplot2")
  require("ggsci")

  # plot setting
  theme_set(theme_classic()) # theme
  windowsFonts(A = windowsFont("Times New Roman"), B = windowsFont("Arial")) # font
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

  #varibale name
  name_var1 <- deparse(substitute(x))
  name_var2 <- deparse(substitute(y))                
  
  # ccf
  ccf_result <- ccf(x, y, lag.max = 10, plot = F, na.action = na.pass)

  # figure data
  figure_data <- data.frame(
    lag = ccf_result$lag,
    coefficient = ccf_result$acf
  )

  # figure
  ccf_figure <- ggplot(figure_data) +
    geom_segment(aes(x = lag, xend = lag, y = 0, yend = coefficient), color = "grey") +
    geom_point(aes(x = lag, y = coefficient), size = 2) +
    geom_hline(yintercept = 0) +
    annotate("text",
      label = paste(name_var1, "&", name_var2),
      x = -7.5, hjust = 0.05, y = Inf, vjust = 1,
      family = "Arial"
    ) +
    scale_x_continuous("Time lag (year)") +
    scale_y_continuous("Correlation coefficient") +
    mytheme

  return(ccf_figure)
  
}

# save function
save(cross_correlation_figure, file = "Utils/cross_correlation_figure.R")

# function test
data <- data.frame(
  test_var1 = rnorm(100),
  test_var2 = rnorm(100)
)

data[1:10,1] <- NA

cross_correlation_figure(data$test_var1, data$test_var2)






# 3 Generate candidate time series data -----------------------------------
# This is a function used to read data and then resample from their distributions to get
# candidate time sereis, then used in the sensitivity analysis on uncertainty of data

generate_candidate_data <- function(){
  
  require(tidyverse)
  require(readxl)
  
  # read data and make candidate time series
  cod <- read_rds("Data/cod.rds") %>%
    rowwise() %>%
    mutate(
      rec_candidate = exp(rnorm(n = 1, mean = log(rec), sd = (log(rec_up) - log(rec_low)) / 4)), # log-normal distribution
      ssb_candidate = exp(rnorm(n = 1, mean = log(ssb), sd = (log(ssb_up) - log(ssb_low)) / 4))
    ) %>%
    select(year, rec_candidate, ssb_candidate, age1, age2)
  
  physical <- read_rds("Data/physical.rds")
  
  biological <- read_rds("Data/biological.rds") %>%
    rowwise() %>%
    mutate(
      mackerel_ssb_candidate = exp(rnorm(n = 1, mean = log(mackerel_ssb), sd = (log(mackerel_ssb_high) - log(mackerel_ssb_low)) / 4)), # log-normal distribution
      herring_tsb_candidate = exp(rnorm(n = 1, mean = log(herring_tsb), sd = (log(herring_tsb_high) - log(herring_tsb_low)) / 4))
    ) %>% 
    select(
      year,
      mackerel_ssb_candidate, herring_tsb_candidate, capelin_tsb,
      canni_age1, canni_age2, canni_age3, canni_m_age3,
      starts_with("tfi"), B0SUM
    )
  
  fishing <- read_rds("Data/fishing.rds") %>%
    rowwise() %>%
    mutate(fm_candidate = exp(rnorm(n = 1, mean = log(fm), sd = (log(fm_high) - log(fm_low)) / 4))) %>% # log-normal distribution
      select(year, fm_candidate)

    pollution <- read_rds("Data/pollution.rds") %>%
      rename(pollution = 2)

    # focusing on 1981-2022
    data <- left_join(cod, physical, by = "year") %>%
      left_join(biological, by = "year") %>%
      left_join(fishing, by = "year") %>%
      left_join(pollution, by = "year") %>%
      filter(year %in% c(1981:2022))

    # change all NaN to NA
    data <- data %>%
      mutate_all(~ ifelse(is.nan(.), NA, .))

    return(data)
}

# save function
save(generate_candidate_data, file = "Utils/generate_candidate_data.R")

# function test
data_candidate <- generate_candidate_data()
