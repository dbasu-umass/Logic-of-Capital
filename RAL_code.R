# ------------------------------------------------ #
# Code for Table 3.1, Figure 3.6, Figure 3.7
# In the book, the data stopped in 2016
# Here, the data has been updated till 2018
# That is why the numbers are slightly different
# from what is reported in the book
# ------------------------------------------------- #


# LIBRARIES
library(foreign)
library(xtable)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(readxl)



# ---------------------------------------------------- #
# ---  Total Incarcerated population (prison + jails)
datapr <- read_excel("RAL_Data.xlsx", sheet = "prison")


N <- length(datapr$prispop)
prpop <- numeric(12*N)
for (i in 1:N) {
  prpop[(12*(i-1)+1):(12*i)] <- datapr$prispop[i] 
}
# Create monthly series
prpop.monthly <- ts(prpop, start=c(1980,1), frequency = 12)


## --- NBER PEAK QUARTERS ---- ##
peak <- c(1948.75, 1953.25, 1957.50, 1960.25, 1969.75, 1973.75, 
          1980.00, 1981.50, 1990.50, 2001.00, 2007.75)

trough <- c(1949.75, 1954.25, 1958.25, 1961.00, 1970.75, 1975.00,
            1980.50, 1982.75, 1991.00, 2001.75, 2009.25)



# ------------------------------------------------- #
# ---- Basic data on reserve army of labor -------- #
data <- read_excel("RAL_Data.xlsx", sheet = "RAL_Basic")

# Marginally attached workers
marg <- ts(data$MARG, start=c(1948,1), freq=12)
# Part time workers
prtw <- ts(data$PRTW, start=c(1948,1), freq=12)
# Not in labour force but want job
nlfwj <- ts(data$NLFWJ, start=c(1948,1), freq=12)
# Labour force
lf <- ts(data$LF, start=c(1948,1), freq=12)
# Civilian noninstitutional population
civpop <- ts(data$CIVPOP, start=c(1948,1), freq=12)

# First measure of RAL
ral1 <- ts(data$UNEMP, start=c(1948,1), freq=12)

# Total employed workers
emp <- lf-ral1

# Second measure  of RAL (RAL2 = RAL1 + MARG + PRTW)
ral2 <- ts(data$RAL, start=c(1948,1), freq=12)

# Third measure of RAL
ral3 <- (ral2 - marg) + nlfwj

# Mean ratio of RAL2 to RAL1: 1994M1 onwards
ratio <- mean((ral2[time(ral2)>=1994])/(ral1[time(ral1)>=1994]))

# Use "ratio" to impute RAL2 for period before 1994M1
ral2[time(ral2)<1994] <- (ratio)*(ral1[time(ral1)<1994]) 

# Mean ratio of RAL3 to RAL1: 1994M1 onwards
ratio1 <- mean((ral3[time(ral3)>=1994])/(ral1[time(ral1)>=1994]))

# Use "ratio1" to impute RAL3 for period before 1994M1
ral3[time(ral3)<1994] <- (ratio1)*(ral1[time(ral1)<1994]) 

# Fourth measure of RAL (including incarcerated population)
ral4 <- ral3 + prpop.monthly/1000

# PROPORTION OF LABOUR FORCE
ral1.sh <- 100*(ral1/lf)
ral2.sh <- 100*(ral2/lf)
ral3.sh <- 100*(ral3/lf)
ral4.sh <- 100*(ral4/lf)

# PROPORTION OF CIVILIAN NONINSTITUTIONAL POPULATION
ral1.sh.pop <- 100*(ral1/civpop)
ral2.sh.pop <- 100*(ral2/civpop)
ral3.sh.pop <- 100*(ral3/civpop)
ral4.sh.pop <- 100*(ral4/civpop)



# ----------------------------------------------- #
# ------------------- TABLE 3.1 ----------------- #
# 4 measures
# RAL1: unemployed workers
# RAL2 = RAL1 + marginally attached + part-time workers
# RAL3 = RAL1 + part-time workers + not-in-labour-force but want a job
# RAL4 = RAL3 + prison & jail population

# --- MEAN
# MAGNITUDE
ral.pw.mean <- c(mean(ral1),mean(ral2),mean(ral3),mean(ral4))
ral.ga.mean <- c(mean(ral1[time(ral1)<1980]),mean(ral2[time(ral2)<1980]),mean(ral3[time(ral3)<1980]),mean(ral4[time(ral4)<1980]))
ral.nl.mean <- c(mean(ral1[time(ral1)>=1980]),mean(ral2[time(ral2)>=1980]),mean(ral3[time(ral3)>=1980]),mean(ral4[time(ral4)>=1980]))
# SHARE OF LABOUR FORCE
ral.sh.pw.mean <- c(mean(ral1.sh),mean(ral2.sh),mean(ral3.sh),mean(ral4.sh))
ral.sh.ga.mean <- c(mean(ral1.sh[time(ral1.sh)<1980]),mean(ral2.sh[time(ral2.sh)<1980]),mean(ral3.sh[time(ral3.sh)<1980]),mean(ral4.sh[time(ral4.sh)<1980]))
ral.sh.nl.mean <- c(mean(ral1.sh[time(ral1.sh)>=1980]),mean(ral2.sh[time(ral2.sh)>=1980]),mean(ral3.sh[time(ral3.sh)>=1980]),mean(ral4.sh[time(ral4.sh)>=1980]))
# SHARE OF CIV NONINST POP
ral.sh.pop.pw.mean <- c(mean(ral1.sh.pop),mean(ral2.sh.pop),mean(ral3.sh.pop),mean(ral4.sh.pop))
ral.sh.pop.ga.mean <- c(mean(ral1.sh.pop[time(ral1.sh.pop)<1980]),mean(ral2.sh.pop[time(ral2.sh.pop)<1980]),mean(ral3.sh.pop[time(ral3.sh.pop)<1980]),mean(ral4.sh.pop[time(ral4.sh.pop)<1980]))
ral.sh.pop.nl.mean <- c(mean(ral1.sh.pop[time(ral1.sh.pop)>=1980]),mean(ral2.sh.pop[time(ral2.sh.pop)>=1980]),mean(ral3.sh.pop[time(ral3.sh.pop)>=1980]),mean(ral4.sh.pop[time(ral4.sh.pop)>=1980]))


# ---- MEDIAN
# MAGNITUDE
ral.pw.median <- c(median(ral1),median(ral2),median(ral3),median(ral4))
ral.ga.median <- c(median(ral1[time(ral1)<1980]),median(ral2[time(ral2)<1980]),median(ral3[time(ral3)<1980]),median(ral4[time(ral4)<1980]))
ral.nl.median <- c(median(ral1[time(ral1)>=1980]),median(ral2[time(ral2)>=1980]),median(ral3[time(ral3)>=1980]),median(ral4[time(ral4)>=1980]))
# SHARE OF LABOUR FORCE
ral.sh.pw.median <- c(median(ral1.sh),median(ral2.sh),median(ral3.sh),median(ral4.sh))
ral.sh.ga.median <- c(median(ral1.sh[time(ral1.sh)<1980]),median(ral2.sh[time(ral2.sh)<1980]),median(ral3.sh[time(ral3.sh)<1980]),median(ral4.sh[time(ral4.sh)<1980]))
ral.sh.nl.median <- c(median(ral1.sh[time(ral1.sh)>=1980]),median(ral2.sh[time(ral2.sh)>=1980]),median(ral3.sh[time(ral3.sh)>=1980]),median(ral4.sh[time(ral4.sh)>=1980]))
# SHARE OF CIV NONINST POP
ral.sh.pop.pw.median <- c(median(ral1.sh.pop),median(ral2.sh.pop),median(ral3.sh.pop),median(ral4.sh.pop))
ral.sh.pop.ga.median <- c(median(ral1.sh.pop[time(ral1.sh.pop)<1980]),median(ral2.sh.pop[time(ral2.sh.pop)<1980]),median(ral3.sh.pop[time(ral3.sh.pop)<1980]),median(ral4.sh.pop[time(ral4.sh.pop)<1980]))
ral.sh.pop.nl.median <- c(median(ral1.sh.pop[time(ral1.sh.pop)>=1980]),median(ral2.sh.pop[time(ral2.sh.pop)>=1980]),median(ral3.sh.pop[time(ral3.sh.pop)>=1980]),median(ral4.sh.pop[time(ral4.sh.pop)>=1980]))

# ----- STANDARD DEVIATION
# MAGNITUDE
ral.pw.sd <- c(sd(ral1),sd(ral2),sd(ral3),sd(ral4))
ral.ga.sd <- c(sd(ral1[time(ral1)<1980]),sd(ral2[time(ral2)<1980]),sd(ral3[time(ral3)<1980]),sd(ral4[time(ral4)<1980]))
ral.nl.sd <- c(sd(ral1[time(ral1)>=1980]),sd(ral2[time(ral2)>=1980]),sd(ral3[time(ral3)>=1980]),sd(ral4[time(ral4)>=1980]))
# SHARE of LABOUR FORCE
ral.sh.pw.sd <- c(sd(ral1.sh),sd(ral2.sh),sd(ral3.sh),sd(ral4.sh))
ral.sh.ga.sd <- c(sd(ral1.sh[time(ral1.sh)<1980]),sd(ral2.sh[time(ral2.sh)<1980]),sd(ral3.sh[time(ral3.sh)<1980]),sd(ral4.sh[time(ral4.sh)<1980]))
ral.sh.nl.sd <- c(sd(ral1.sh[time(ral1.sh)>=1980]),sd(ral2.sh[time(ral2.sh)>=1980]),sd(ral3.sh[time(ral3.sh)>=1980]),sd(ral4.sh[time(ral4.sh)>=1980]))
# SHARE OF CIV NONINST POP
ral.sh.pop.pw.sd <- c(sd(ral1.sh.pop),sd(ral2.sh.pop),sd(ral3.sh.pop),sd(ral4.sh.pop))
ral.sh.pop.ga.sd <- c(sd(ral1.sh.pop[time(ral1.sh.pop)<1980]),sd(ral2.sh.pop[time(ral2.sh.pop)<1980]),sd(ral3.sh.pop[time(ral3.sh.pop)<1980]),sd(ral4.sh.pop[time(ral4.sh.pop)<1980]))
ral.sh.pop.nl.sd <- c(sd(ral1.sh.pop[time(ral1.sh.pop)>=1980]),sd(ral2.sh.pop[time(ral2.sh.pop)>=1980]),sd(ral3.sh.pop[time(ral3.sh.pop)>=1980]),sd(ral4.sh.pop[time(ral4.sh.pop)>=1980]))


ral.pw <- rbind(ral.pw.mean, ral.pw.median, ral.pw.sd)/1000
ral.sh.pw <- rbind(ral.sh.pw.mean, ral.sh.pw.median, ral.sh.pw.sd)
ral.sh.pop.pw <- rbind(ral.sh.pop.pw.mean, ral.sh.pop.pw.median, ral.sh.pop.pw.sd)
ral.ga <- rbind(ral.ga.mean, ral.ga.median, ral.ga.sd)/1000
ral.sh.ga <- rbind(ral.sh.ga.mean, ral.sh.ga.median, ral.sh.ga.sd)
ral.sh.pop.ga <- rbind(ral.sh.pop.ga.mean, ral.sh.pop.ga.median, ral.sh.pop.ga.sd)
ral.nl <- rbind(ral.nl.mean, ral.nl.median, ral.nl.sd)/1000
ral.sh.nl <- rbind(ral.sh.nl.mean, ral.sh.nl.median, ral.sh.nl.sd)
ral.sh.pop.nl <- rbind(ral.sh.pop.nl.mean, ral.sh.pop.nl.median, ral.sh.pop.nl.sd)

ral.sumstat <- rbind(ral.pw,ral.sh.pw,
                     ral.ga,ral.sh.ga,
                     ral.nl,ral.sh.nl)
colnames(ral.sumstat) <- c("RAL1","RAL2","RAL3","RAL4")
rownames(ral.sumstat) <- rep(c("Mean", "Median", "Std Dev"),6)
print(xtable(ral.sumstat)) 


# ------------------------------------------------ #
# ------------------- FIGURE 3.6 ----------------- #

# ABSOLUTE MAGNITUDE
low.lim <- min(ral1/1000, na.rm=T)
up.lim <- max(ral4/1000, na.rm=T)
pdf("RAL-level.pdf")
plot(ral1/1000, type="l", col="blue", main="Reserve Army of Labor (Absolute Magnitude)", ylab="million", xlab="", ylim=c(low.lim,up.lim))
for (i in 1:length(peak)){
  abline(v=seq(peak[i],trough[i],by=0.01), col="gray")
}
lines(ral1/1000)
lines(ral2/1000)
lines(ral3/1000)
lines(ral4/1000)
text(2003,22, "RAL4", cex=0.75)
text(1995,4.5, "RAL3", cex=0.75)
text(1985,4.5, "RAL2", cex=0.75)
text(1978,4.5, "RAL1", cex=0.75)
arrows(2004.5,22.5,x1=2010.5,y1=23,length=0.05)
arrows(1995,5,x1=1998,y1=11,length=0.05)
arrows(1985,5,x1=1989,y1=8,length=0.05)
arrows(1978,5,x1=1979,y1=6,length=0.05)
dev.off()



# ------------------------------------------------ #
# ------------------- FIGURE 3.7 ----------------- #

# PROPORTION OF LABOUR FORCE
ral1.sh <- 100*(ral1/lf)
ral2.sh <- 100*(ral2/lf)
ral3.sh <- 100*(ral3/lf)
ral4.sh <- 100*(ral4/lf)

low.lim <- min(ral1.sh, na.rm=T)
up.lim <- max(ral4.sh, na.rm=T)

pdf("RAL-proportion.pdf")
plot(ral1.sh, type="l", col="blue", main="Reserve Army of Labor (Proportion of Labor Force)",
     ylab="percentage (%)", xlab="", ylim=c(low.lim,up.lim))
for (i in 1:length(peak)){
  abline(v=seq(peak[i],trough[i],by=0.01), col="gray")
}
lines(ral1.sh)
lines(ral2.sh)
lines(ral3.sh)
lines(ral4.sh)
text(2003,13.5, "RAL4", cex=0.75)
text(1995,4.5, "RAL3", cex=0.75)
text(1985,4.5, "RAL2", cex=0.75)
text(1978,4.5, "RAL1", cex=0.75)
arrows(2004.5,13.5,x1=2010.5,y1=15,length=0.05)
arrows(1995,5,x1=1998,y1=8,length=0.05)
arrows(1985,5,x1=1989,y1=6.5,length=0.05)
arrows(1978,5,x1=1979,y1=5.75,length=0.05)
dev.off()


