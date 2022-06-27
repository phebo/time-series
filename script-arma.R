library(tidyverse)
library(rstan)

#### MA(1) ####

xt <- arima.sim(list(ma=-0.3), 100)
plot(xt)
acf(xt)


#### ARMA(1,1) ####
lam <- 0.9
seps <- 1
sdelta <- 2
n <- 1e5

xt <- arima.sim(list(ar = lam), n) * seps
plot(xt)
var(xt)
seps^2 / (1-lam^2)

yt <- xt + rnorm(n, sd = sdelta)
plot(yt)
fit1 <- arima(yt, order = c(1,0,0), include.mean = F)
fit2 <- arima(yt, order = c(1,0,1), include.mean = F)
fit1
fit2
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
dat <- list(n = n, x = arima.sim(list(ar = 0.9), n))
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
acf(tsGdp, na.action = na.pass)
pacf(tsGdp)
fit1 <- arima(tsGdp, order = c(1,0,0))
fit1
acf(resid(fit1))
arima(tsGdp, order = c(1,0,1))
arima(tsGdp, order = c(2,0,0))
arima(tsGdp, order = c(0,0,2))
fit2 <- arima(tsGdp, order = c(0,0,3))
fit2
acf(resid(fit2))

tsGdp2 <- dfGdp %>% filter(between(as.numeric(format(dfGdp$DATE, "%Y")), 1980, 2019)) %>%
  pull(gdp) %>%
  ts(frequency = 4, start = 1980) %>% log()
plot(tsGdp2)
acf(tsGdp2)
pacf(tsGdp2)
fit1b <- arima(tsGdp, order = c(1,1,0))
fit1b
plot(resid(fit1b))
acf(resid(fit1b))


#### Bayesian example ####
N <- 5
y <- rnorm(N, 3, 5)
fit <- stan("example.stan", data = list(N = N, y = y))
fit
pars <- extract(fit)
hist(pars$mu)
hist(pars$sigma)
pairs(pars)
