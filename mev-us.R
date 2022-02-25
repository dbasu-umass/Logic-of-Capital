# ------------------------------------------------------------------ #
# --------------- Monetary Expression of Value --------------------- #
# --------------- (Figure 3.8) ------------------------------------- #


# Load libraries
library(fredr)
library(tidyverse)
library(ggplot2)
library(foreign)


#------------- READ DATA FROM Fed St Louis --------------------- #
# We use "fredr" and the FRED API to get the relevant data.
# To use fredr and the FRED API in general, you must first obtain a FRED API key.
# See https://cran.r-project.org/web/packages/fredr/vignettes/fredr.html


# Note: All data are at annual frequency

# gdp: gross value added of the business sector (billion $)
d1 <- fredr(
  series_id = "A195RC1Q027SBEA",
  observation_start = as.Date("1964-01-01"),
  frequency = "a"
)%>%
  dplyr::select(date,value) %>%
  dplyr::rename(gdp=value)



# weekhrs: average hours per week of production and nonsupervisory workers
d2 <- fredr(
  series_id = "AWHNONAG",
  observation_start = as.Date("1964-01-01"),
  frequency = "a"
)%>%
  dplyr::select(date,value) %>%
  dplyr::rename(weekhrs=value)


# employees: all employees in total private industries (thousands)
d3 <- fredr(
  series_id = "USPRIV",
  observation_start = as.Date("1964-01-01"),
  frequency = "a"
)%>%
  dplyr::select(date,value) %>%
  dplyr::rename(employees=value)



# Merge variables to create data frame
d4 <- left_join(d1,d2,by="date")
data <- left_join(d3,d4,by="date")



# ---------------- Figure 3.8 ---------------------- #

# Create data for plot
d11 <- data %>%
  mutate(
    # total hours
    hours = employees * weekhrs * 52 * 1000,
    # mev (nominal)     
    mev = (gdp*10^9)/(hours)
  ) %>%
  as.data.frame()


# --- Create chart
ggplot(data = d11, aes(x=date, y=mev)) +
  geom_line(color="blue") +
  labs(x="Year",
       y="MEV (USD/hour)",
       title = "Monetary Expression of Value for the US Economy") +
  theme_minimal()

