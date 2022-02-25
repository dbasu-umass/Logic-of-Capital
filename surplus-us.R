# LIBRARIES
library(foreign)
library(xtable)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(zoo)
library(readxl)

# Read data (all figures in billions of USD)
data <- readxl::read_excel("profit-us.xlsx", sheet = "data")


# --------- Average profit share
data %>%
  filter(year>=1946) %>%
  mutate(
    pshare_cb = 100*(nos_cb/nva_cb),
    pshare_nfcb = 100*(nos_nfcb/nva_nfcb),
    nlc = ifelse(year>1980,1,0)
  ) %>%
  summarize(
    pshare_cb_m = mean(pshare_cb),
    pshare_nfcb_m = mean(pshare_nfcb)
  )

# ----- Corporate Business Sector --------- #
# Create data 
d2 <- data %>%
  filter(year>=1948) %>%
  mutate(
    Dividend = 100*(divd_cb/nos_cb),
    Tax = 100*(corptax_cb/nos_cb),
    Interest = 100*(int_cb/nos_cb),
    Profit = 100*(reteng_cb/nos_cb),
    Transfer = 100*(transfer_cb/nos_cb)
  ) %>%
  select(year, Profit, Tax, Interest, 
         Dividend, Transfer) %>%
  gather("type","surp", -year) %>%
  as.data.frame()


# --------------- Figure 5.6 --------------- #
# Stacked barplot
ggplot(data=d2, aes(x=year, y=surp, fill=type)) +
  geom_bar(stat="identity") +
  scale_fill_grey(start = 0.3, end=0.9) +
  theme_minimal() +
  labs(
    x="",
    y="% of surplus value",
    fill=""
  ) +
  theme(legend.position = "bottom")



# ----- Nonfinancial Corporate Business Sector --------- #
# Create data
d3 <- data %>%
  filter(year>=1948) %>%
  mutate(
    Dividend = 100*(divd_nfcb/nos_nfcb),
    Tax = 100*(corptax_nfcb/nos_nfcb),
    Interest = 100*(int_nfcb/nos_nfcb),
    Profit = 100*(reteng_nfcb/nos_nfcb),
    Transfer = 100*(transfer_nfcb/nos_nfcb)
  ) %>%
  select(year, Profit, Tax, Interest, 
         Dividend, Transfer) %>%
  gather("type","surp", -year) %>%
  as.data.frame()




# --------------- Figure 5.7 --------------- #
# Stacked barplot
ggplot(data=d3, aes(x=year, y=surp, fill=type)) +
  geom_bar(stat="identity") +
  scale_fill_grey(start = 0.3, end=0.9) +
  theme_minimal() +
  labs(
    x="",
    y="% of surplus value",
    fill=""
  ) +
  theme(legend.position = "bottom")


