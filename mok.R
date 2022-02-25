
# LIBRARIES
library(foreign)
library(xtable)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(zoo)
library(readxl)

# Read data (Corporate Business Sector)
data <- readxl::read_excel("data-mok.xlsx", sheet = "data")

# Data for plot
d1 <- data %>%
  mutate(
    # rate of profit
    rop = 100*(nos/capstock),
    # Output-capital ratio
    outcap = nva/capstock 
  ) %>%
  mutate(
    # Capital coefficient
    a = 1/outcap,
    # Labour coefficient
    n = 1/lp_person 
  ) %>%
  mutate(
    # Alpha = ratio of real wage rate
    myalpha = (rwage_wk/dplyr::lag(rwage_wk)),
    # --- Marx-Okishio threshold
    # Measure 1, using avg hours per week = 34
    myalpha_star1 = (dplyr::lag(n)/n) + ((dplyr::lag(a)/a)-1)*(a*34/(n*rwage_wk)),
    # Measure 2, using the real profit share
    pshare = (1 - (nos/nva)*(gdpdef/cpi)),
    myalpha_star2 = (dplyr::lag(n)/n) + ((dplyr::lag(a)/a)-1)*(a/(1-pshare)),
    # Alpha - MO threshold
    mydiff1 = (myalpha - myalpha_star1),
    mydiff2 = (myalpha - myalpha_star2),
    # Change in rate of profit
    ropdiff = rop-dplyr::lag(rop)
  ) %>%
  as.data.frame()



# --------------- Figure 6.2 ----------------------- #
# Note: This measure is reported in the book
p2 <- ggplot(d1, aes(x=mydiff2, y=ropdiff)) + 
  geom_point(shape=18, size=4) +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="black") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(
    x="real wage ratio less Marx-Okishio threshold",
    y="change in rate of profit"
  ) +
  theme_minimal()

print(p2)


# ------------------------------------------------ #
# Note: the following are not reported in the book

# Chart (Measure 1)
p1 <- ggplot(d1, aes(x=mydiff1, y=ropdiff)) + 
  geom_point(shape=18, size=4) +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="black") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(
    x="real wage ratio less Marx-Okishio threshold",
    y="change in rate of profit"
  ) +
  theme_minimal()

print(p1)


# --- Average Marx-Okishio threshold
d1 %>%
  dplyr::select(year,myalpha_star1,myalpha_star2) %>%
  dplyr::filter(year>=2000) %>%
  summarise(
    alpha1_m = mean(myalpha_star1),
    alpha2_m = mean(myalpha_star2)
  )