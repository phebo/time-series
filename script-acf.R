library(tidyverse)
library(RPostgres)
library(mvtnorm)

dataDir <- "data"

#### Simulation ####

#Correlation
rho <- 0.8
simData <- rmvnorm(1e3, sigma = rbind(c(1,rho), c(rho,1))) 
colnames(simData) <- c("x", "y")
mean(simData[,"y"]*simData[,"x"])
plot(simData[,"x"], simData[,"y"])

x <- rnorm(1e3, sd = 2)
u <- rnorm(1e3, sd = 1)
y <- x + u
var(x) # = 4
var(u)
var(y) # = 5
# So R^2 = 4 / 5 = 0.8
# So cor = sqrt(.8)
sqrt(.8)
cor(y,x)

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

tGdp <- dfGdp %>% filter(between(as.numeric(format(dfGdp$DATE, "%Y")), 1981, 2019)) %>%
  pull(gdp2) %>%
  ts(frequency = 4, start = 1981)
plot(tGdp)
dGdp <- dfGdp %>% filter(between(as.numeric(format(dfGdp$DATE, "%Y")), 1981, 2019)) %>%
  pull(gdp2) %>%
  ts(frequency = 4, start = 1981) %>%
  decompose("multiplicative")
plot(dGdp)
acf(dGdp$random, na.action = na.pass)
1/sqrt(length(dGdp$random)) # S.e. of acf confidence interval
1.96/sqrt(length(dGdp$random)) # 95% intervals
pacf(dGdp$random, na.action = na.pass) 
# Partial autocorrelation at lag k = correlation remaining after regressing out lags 1, ..., k-1

x <- rnorm(100)
acf(ts(x)) # white noise rho_k = 0 for k >= 1

# Correlations in panels
dfCompu
nrow(dfCompu)
length(unique(dfCompu$gvkey))
hist(dfCompu$fyear, breaks=22) # weird plot; unclear why ???
print(dfCompu %>% group_by(fyear) %>% summarize(n=n()), n=30)
dfSub <- dfCompu %>% select(icapt, at, ebit) %>% filter(!is.na(icapt+at+ebit))
pairs(dfSub)
cor(dfSub)

mat <- xtabs(log(at) ~ fyear + gvkey, dfCompu)
mat[1:6,1:6] # firms are in columns = good!
mat[mat == 0] <- NA
mat[,1:6]
tsAT <- ts(mat, frequency = 1, start = 1998)
str(tsAT)
tsAT[,1:6]
acf(tsAT[,1:3], na.action = na.pass, lag.max = 10)

