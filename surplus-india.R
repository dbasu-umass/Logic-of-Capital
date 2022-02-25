# LIBRARIES
library(foreign)
library(xtable)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(zoo)
library(readxl)

# Read data (all figures in rs lakh)
data <- readxl::read_excel("asi-data.xlsx", sheet = "data")

# Create data for plot
d1 <- data %>%
  # interpolate missing values
  mutate(
    i.pension = zoo::na.approx(pension),
    i.socsec = zoo::na.approx(socsec)
  ) %>%
  # variable capital and surplus value
  mutate(
    comp = emoluments + i.pension + i.socsec,
    surplus = nva - comp,
    profit = surplus - rent - interest) %>%
  mutate(
    # rate of exploitation
    roe = surplus/comp,
    # organic composition of capital     
    occ = (inputs + depr)/comp,
    # rate of profit     
    rop = surplus/(comp + inputs + depr)) %>%
  as.data.frame()

# Save data set
dind <- d1 %>%
  select(year,roe,occ,rop) %>%
  mutate(country="India") %>%
  as.data.frame()


# -------------------------------------------------- #
# ---------------- Figure 3.4 ---------------------- #

# --- Create charts of Three Ratios
# rate of exploitation:s/v
p1 <- ggplot(data = d1, aes(x=year, y=roe)) +
  geom_line() +
  labs(x="",
       y="rate of exploitation (s/v)") +
  theme_minimal()

# organic composition of capital: c/v
p2 <- ggplot(data = d1, aes(x=year, y=occ)) +
  geom_line() +
  labs(x="",
       y="organic composition of capital (c/v)") +
  theme_minimal()

# rate of profit:(s/v)/(1+c/v)
p3 <- ggplot(data = d1, aes(x=year, y=rop)) +
  geom_line() +
  labs(x="",
       y="rate of profit (s/(c+v))") +
  theme_minimal()

p_comb <- ggarrange(p1, p2, p3, 
                    ncol=1, nrow = 3)

print(p_comb)



# ----------------- Figure 5.8 ---------------------- #
# --- Create charts of components of surplus value
d2 <- d1 %>%
  mutate(
    Profit = 100*(profit/surplus),
    Rent = 100*(rent/surplus),
    Interest = 100*(interest/surplus)
  ) %>%
  select(year, Profit, Rent, Interest) %>%
  gather("type","surp", -year) %>%
  as.data.frame()



# Stacked barplot with multiple groups
pscomp <- ggplot(data=d2, aes(x=year, y=surp, fill=type)) +
  geom_bar(stat="identity") +
  scale_fill_grey(start = 0.3, end=0.9) +
  theme_minimal() +
  labs(
    x="",
    y="% of surplus value",
    fill=""
  ) +
  theme(legend.position = "bottom")


print(pscomp)


