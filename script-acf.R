library(tidyverse)
library(RPostgres)
library(mvtnorm)

dataDir <- "data"

#### Simulation ####

#Correlation
rho <- 0.2
simData <- rmvnorm(1e3, sigma = rbind(c(1,rho), c(rho,1))) 
colnames(simData) <- c("x", "y")
mean(simData[,"y"]*simData[,"x"])
plot(simData[,"x"], simData[,"y"])

#AR(1)
tSim <- arima.sim(list(ar = 0.9), 1e5)
plot(tSim)
acf(tSim)

#### Analysis ####

dfRf <- read_csv(file.path(dataDir, "rf.csv"))
dfCpi <- read_csv(file.path(dataDir, "cpi.csv"))
dfGdp <- read_csv(file.path(dataDir, "gdp.csv"))
dfCompu <- read_csv(file.path(dataDir, "compu.csv"))
dfIndex <- read_csv(file.path(dataDir, "index.csv"))

dfGdp <- dfGdp %>% 
  rename(
    gdp = GDPC1,  # real gdp, seasonality adjusted
    gdp2 = NA000334Q) %>%  # real gdp, non-seasonality adjusted
  mutate(
    growth = gdp / lag(gdp),
    growth2 = gdp2 / lag(gdp2) - 1
  )

dGdp <- dfGdp %>% filter(between(as.numeric(format(dfGdp$DATE, "%Y")), 1980, 2019)) %>%
  pull(gdp2) %>%
  ts(frequency = 4, start = 1980) %>%
  decompose("multiplicative")
acf(dGdp$random, na.action = na.pass)


