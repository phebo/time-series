library(tidyverse)
library(rstan)

#AR(1)
lam <- .9
n <- 1e2

tSim <- arima.sim(list(ar = lam), n)
plot(tSim)
sqrt(1/(1-lam^2))
sqrt(var(tSim))

acf(tSim)
lam^40
pacf(tSim)


lam <- 1.01
n <- 500

eps <- rnorm(n)
x <- rep(0, n)
for(tt in 1:(n-1)) {
  x[tt+1] <- lam * x[tt] + eps[tt]
}
x <- ts(x)
plot(x)
acf(x)
pacf(x)

# Simulate OU process
th <- 0.1 # Corresponds with charateristic time scale of T = 1/th
sigma0 <- 1 # Volatility of process
dt <- 1/4 # Sampling interval
Tt <- 20 # Sampling time

alpha <- exp(-th * dt)
sigma <- sigma0 * sqrt(dt)
n <- floor(Tt / dt)

alpha # AR(1) parameter
sqrt(sigma^2 / (1-alpha^2)) # Stationary standard deviation AR(1)
sigma0 / sqrt(2 * th) # Stationary standard deviation of OU


tSim <- ts(arima.sim(list(ar = alpha), n, sd = sigma), frequency = 1/dt)
plot(tSim)

df <- tibble(y=as.vector(tSim), x=dplyr::lag(y))
fit <- lm(y~x, df)
summary(fit)
coef(fit)[2]
sqrt(vcov(fit)[2,2]) #se

fit <- lm(y~x-1, df)
summary(fit)
coef(fit)[1]
sqrt(vcov(fit)) #se

fit <- arima(tSim, order = c(1,0,0))
print(fit)
coef(fit)[1]
sqrt(vcov(fit)[1,1]) #se

fit <- arima(tSim, order = c(1,0,0), include.mean = F)
print(fit)
coef(fit)[1]
sqrt(vcov(fit)[1,1]) #se

dat <- list(n = n, x = tSim)
fit <- stan("model-ar1.stan", data = dat)


# Monte Carlo simulation

th <- 0.5 # Corresponds with charateristic time scale of T = 1/th
sigma0 <- 1 # Volatility of process
dt <- 1/4 # Sampling interval
Tt <- 20 # Sampling time

alpha <- exp(-th * dt)
sigma <- sigma0 * sqrt(dt)
n <- floor(Tt / dt)

N <- 1e3

dfSim <- tibble(i=1:N) %>% rowwise() %>%
  mutate(
    tSim = list(ts(arima.sim(list(ar = alpha), n, sd = sigma), frequency = 1/dt)),
    ols = list({df <- tibble(y=as.vector(tSim), x=dplyr::lag(y)); fit <- lm(y~x-1, df)}),
    #arima = list(arima(tSim, order = c(1,0,0))),
    arima0 = list(tryCatch({arima(tSim, order = c(1,0,0))}, error= function(e) NA)),
    arima = list(tryCatch({arima(tSim, order = c(1,0,0), include.mean = F)}, error= function(e) NA))
    ) %>% ungroup() %>% filter(!is.na(arima), !is.na(arima0))
dfSim2 <- dfSim %>% select(-tSim) %>% pivot_longer(ols:arima) %>% rowwise() %>%
  mutate(
    alpha.est = coef(value)[1],
    alpha.se = sqrt(vcov(value)[1,1]),
    low = alpha.est - 1.96 * alpha.se,
    high = alpha.est + 1.96 * alpha.se) %>% ungroup() %>%
  arrange(name, alpha.est) %>% mutate(i2 = rep(1:nrow(dfSim),3)) %>%
  select(-value)
ggplot(dfSim2, aes(x = i2, y = alpha.est, ymin=low, ymax=high)) +
  facet_wrap(~name,scales = "free_x", ncol=1 ) +
  geom_pointrange() + geom_hline(yintercept = alpha)
dfSim2 %>% 
  mutate(b = between(rep(alpha, nrow(dfSim2)), low, high)) %>%
  group_by(name) %>%
  summarize(
    mean = mean(alpha.est),
    cover = sum(b)/n())


#### MA(1) ####

xt <- arima.sim(list(ma=-0.5), 1000)
plot(xt)
acf(xt) # Just one period
-.5/(1+.5^2) # rho_1
pacf(xt) # Exponential decay
# Roles of ACF and PACF are exactly reversed from AR(1)

#### ARMA(1,1) ####
lam <- 0.9
seps <- 1
sdelta <- 2
n <- 1e5

xt <- arima.sim(list(ar = lam), n) * seps #AR(1)
plot(xt)
var(xt)
seps^2 / (1-lam^2)

yt <- xt + rnorm(n, sd = sdelta) # AR(1) with noise
plot(yt)
fit1 <- arima(yt, order = c(1,0,0), include.mean = F)
df <- tibble(y=as.vector(yt), x=dplyr::lag(y))
fit1b <- lm(y~x, df)
fit2 <- arima(yt, order = c(1,0,1), include.mean = F)
fit1 # Estimate 0.508; 
summary(fit1b) # Estimate 0.508
fit2 # Estimate 0.898
g <- (seps / sdelta)^2 / 2 + 1; -g+sqrt(g^2-1) # Approx MA term, for lam ~ 1

acf(resid(fit1))
acf(resid(fit2))

#### MC simulation ####

# AR(1) regression
sim <- function(lam, n) {
  xt <- arima.sim(list(ar = lam), n)
  reg.data <- tibble(y=as.vector(xt), x=lag(y))
  fit <- lm(y ~ x, reg.data)
  tibble(par = c("cst", "lam"), est = coef(fit), se = sqrt(diag(vcov(fit))))
}
dfSim <- map_dfr(1:100, ~ sim(lam = 0.9, n = 50))

dfLam <- dfSim %>% filter(par == "lam") %>% arrange(est) %>%
  mutate(i = row_number(), ymin = est - 1.96 * se, ymax = est + 1.96 * se)
ggplot(dfLam, aes(x = i, y = est, ymin=ymin, ymax=ymax)) + geom_pointrange() +
  geom_hline(yintercept = 0.9)

# AR(1) Bayesian estimation
n <- 50
x <- arima.sim(list(ar = 0.9), n)
plot(x)
dat <- list(n = n, x = x)
fit1 <- stan("model-ar1.stan", data = dat)
fit1
par.sim <- extract(fit1)
hist(par.sim$lambda)
hist(par.sim$theta)

# ARMA(1,1) regression
sim <- function(lam, sdelta, n) {
  xt <- arima.sim(list(ar = lam), n)
  yt <- xt + rnorm(n, sd = sdelta)
  reg.data <- tibble(y=as.vector(yt), x=lag(y))
  fit <- lm(y ~ x, reg.data)
  tibble(par = c("cst", "lam"), est = coef(fit), se = sqrt(diag(vcov(fit))))
}
dfSim <- map_dfr(1:100, ~ sim(lam = 0.9, sdelta = 1, n = 1e4))

dfLam <- dfSim %>% filter(par == "lam") %>% arrange(est) %>%
  mutate(i = row_number(), ymin = est - 1.96 * se, ymax = est + 1.96 * se)
ggplot(dfLam, aes(x = i, y = est, ymin=ymin, ymax=ymax)) + geom_pointrange() +
  geom_hline(yintercept = lam)

# ARMA(1,1) ML estimation
sim <- function(lam, sdelta, n) {
  xt <- arima.sim(list(ar = lam), n)
  yt <- xt + rnorm(n, sd = sdelta)
  fit <- arima(yt, order = c(1,0,1))
  tibble(par = c("lam", "beta", "cst"), est = coef(fit), se = sqrt(diag(vcov(fit))))
}
dfSim <- map_dfr(1:100, ~ sim(lam = 0.95, sdelta = 0.2, n = 200))

dfLam <- dfSim %>% filter(par == "lam") %>% arrange(est) %>%
  mutate(i = row_number(), ymin = est - 1.96 * se, ymax = est + 1.96 * se)
ggplot(dfLam, aes(x = i, y = est, ymin=ymin, ymax=ymax)) + geom_pointrange() +
  geom_hline(yintercept = lam)

#### AR(2) ####

lam1 <- 0.9
lam2 <- 0.8
alpha1 <- lam1 + lam2
alpha2 <- -lam1 * lam2
c(alpha1, alpha2)

xt <- arima.sim(list(ar = c(alpha1, alpha2)), 1e5)
xt2 <- ts(xt[seq(1, length(xt), 2)])

fit1a <- arima(xt, order = c(2,0,0))
fit1b <- arima(xt, order = c(2,0,1)) # MA(1) term insignificant
fit2a <- arima(xt2, order = c(2,0,0))
fit2b <- arima(xt2, order = c(2,0,1)) # An MA(1) term is added
fit1a
fit1b
fit2a
fit2b

c(lam1^2 + lam2^2, - lam1^2 * lam2^2)

acf(resid(fit1a))
acf(resid(fit2a)) # not good
acf(resid(fit2b)) # good

#### AR(2) models & estimation ####
lam1 <- .95
lam2 <- .5
beta1 <- -.6
beta2 <- -0.3
alpha1 <- lam1 + lam2
alpha2 <- -lam1 * lam2
c(alpha1, alpha2)

xt <- arima.sim(list(ar = c(alpha1, alpha2), ma = c(beta1, beta2)), 1000)
arima(xt, order = c(2,0,2))




#### GDP ####

dfGdp <- read_csv("data/gdp.csv")
dfGdp <- dfGdp %>% 
  rename(
    gdp = GDPC1,  # real gdp, seasonality adjusted
    gdp2 = NA000334Q) %>%  # real gdp, non-seasonality adjusted
  mutate(
    growth = gdp / lag(gdp),
    growth2 = gdp2 / lag(gdp2) - 1
  )
tsGdp <- dfGdp %>% filter(between(as.numeric(format(dfGdp$DATE, "%Y")), 1980, 2019)) %>%
  pull(growth) %>%
  ts(frequency = 4, start = 1980) %>% log()

plot(tsGdp)
plot(decompose(tsGdp)) # No real seasonality left; seems stationary
acf(tsGdp, na.action = na.pass)
pacf(tsGdp)
fit1 <- arima(tsGdp, order = c(1,0,0))
fit1
acf(resid(fit1))
pacf(resid(fit1))
fit1b <- arima(tsGdp, order = c(1,0,1))
fit1b
arima(tsGdp, order = c(2,0,0))
fit1c <- arima(tsGdp, order = c(0,0,2))
fit1c
acf(resid(fit1c))
pacf(resid(fit1c))
fit2 <- arima(tsGdp, order = c(0,0,3))
fit2
acf(resid(fit2))
# Plausible models: AR(1), MA(3), or ARMA(1,1)
# But MA(3) is less parsimonious, higher AIC, and theoretically not plausible => less favored
# AR(1) and ARMA(1,1) are both reasonable models it seems both theoretically and empirically

tsGdp2 <- dfGdp %>% filter(between(as.numeric(format(dfGdp$DATE, "%Y")), 1980, 2019)) %>%
  pull(gdp) %>%
  ts(frequency = 4, start = 1980) %>% log()
plot(tsGdp2)
acf(tsGdp2)
pacf(tsGdp2)
arima(tsGdp2, order = c(1,0,0))
fit3 <- arima(tsGdp2, order = c(1,1,0))
fit3
plot(resid(fit3))
acf(resid(fit3))
fit3b <- arima(tsGdp2, order = c(1,1,1))
fit3b
plot(resid(fit3b))
acf(resid(fit3b))


#### Bayesian example ####
N <- 5
y <- rnorm(N, 3, 5)
fit <- stan("example.stan", data = list(N = N, y = y))
fit
pars <- extract(fit)
hist(pars$mu)
hist(pars$sigma)
pairs(pars)


#### (P)ACFs for various models ####
lam1 <- 0.9
lam2 <- 0.9
alpha1 = lam1 + lam2
alpha2 = -lam1 * lam2
x <- arima.sim(list(ar = c(alpha1, alpha2), ma=c(-0.7,-0.1)), n = 1e4)
acf(x)
pacf(x)
plot(x)
arima(x, c(1,0,0))
arima(x, c(2,0,0))
arima(x, c(2,0,1))
arima(x, c(1,0,1))
arima(x, c(2,0,2))
c(alpha1, alpha2)
# With noise, very hard to find second AR(2) term; often underpowered; need ~10^4 data points

acf(arima.sim(list(ar = 0.93), n = 1e2))
pacf(arima.sim(list(ar = 0.93), n = 1e2))


