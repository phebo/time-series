library(tidyverse)
library(RPostgres)
library(mvtnorm)

writeData <- F
dataDir <- "data"

#### Download data ####

# Economic data from https://fred.stlouisfed.org/
dfRf <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=DGS10&scale=left&cosd=1962-01-02&coed=2021-01-07&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Daily&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2021-01-11&revision_date=2021-01-11&nd=1962-01-02",
                     col_types = list(DGS10 = col_double()))
dfCpi <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CPIAUCSL&scale=left&cosd=1947-01-01&coed=2021-02-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2021-03-26&revision_date=2021-03-26&nd=1947-01-01")
dfGdp <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=968&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=GDP,ND000334Q,NA000334Q,GDPC1&scale=left,left,left,left&cosd=1947-01-01,2002-01-01,1947-01-01,1947-01-01&coed=2022-01-01,2022-01-01,2022-01-01,2022-01-01&line_color=%234572a7,%23aa4643,%2389a54e,%2380699b&link_values=false,false,false,false&line_style=solid,solid,solid,solid&mark_type=none,none,none,none&mw=3,3,3,3&lw=2,2,2,2&ost=-99999,-99999,-99999,-99999&oet=99999,99999,99999,99999&mma=0,0,0,0&fml=a,a,a,a&fq=Quarterly,Quarterly,Quarterly,Quarterly&fam=avg,avg,avg,avg&fgst=lin,lin,lin,lin&fgsnd=2020-02-01,2020-02-01,2020-02-01,2020-02-01&line_index=1,2,3,4&transformation=lin,lin,lin,lin&vintage_date=2022-05-30,2022-05-30,2022-05-30,2022-05-30&revision_date=2022-05-30,2022-05-30,2022-05-30,2022-05-30&nd=1947-01-01,2002-01-01,1947-01-01,1947-01-01",
                  col_types = list(ND000334Q = col_double()))
# Seasonaly adjusted: GDP (nominal, $B), GDPC1 (real, 2012 $B)
# Non-adjusted: NA000334Q (nominal, $M), ND000334Q (real, 2012 $B)

username <- "pwibbens"
wrds <- dbConnect(Postgres(), host='wrds-pgdata.wharton.upenn.edu', port=9737, dbname='wrds', sslmode='require', user=username)

# Compustat annual fundamental data; assets > $10B
res <- dbSendQuery(wrds,
  "select gvkey,conm,datadate,fyear,icapt,at,dltt,dlc,ebit,pi,xint,txt
     from comp.funda where
       indfmt = 'INDL' and consol = 'C' and popsrc = 'D' and datafmt = 'STD' and curcd = 'USD' and
       fyear between '1998' and '2019' and at > 10000 ")
dfCompu <- dbFetch(res, n=-1)
dbClearResult(res)

# CRSP indices
# See: https://wrds-www.wharton.upenn.edu/pages/get-data/center-research-security-prices-crsp/annual-update/index-sp-500-indexes/index-file-on-sp-500/
res <- dbSendQuery(wrds, "select * from crsp.dsp500")
dfIndex <- dbFetch(res, n=-1)
dbClearResult(res)

if(writeData) {
  write_csv(dfRf, file.path(dataDir, "rf.csv"))
  write_csv(dfCpi, file.path(dataDir, "cpi.csv"))
  write_csv(dfGdp, file.path(dataDir, "gdp.csv"))
  write_csv(dfCompu, file.path(dataDir, "compu.csv"))
  write_csv(dfIndex, file.path(dataDir, "index.csv"))
}

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

dfGdp
dfGdp <- dfGdp %>% 
  rename(
    gdp = GDPC1,  # real gdp, seasonality adjusted
    gdp2 = NA000334Q) %>%  # real gdp, non-seasonality adjusted
  mutate(
    growth = gdp / lag(gdp),
    growth2 = gdp2 / lag(gdp2) - 1
  )
ggplot(dfGdp, aes(x = DATE, y = gdp)) + geom_line() + scale_y_log10()
ggplot(dfGdp, aes(x = DATE, y = gdp2)) + geom_line() + scale_y_log10()
ggplot(dfGdp, aes(x = DATE, y = growth)) + geom_line() 
ggplot(dfGdp, aes(x = DATE, y = growth2)) + geom_line()
ggplot(dfGdp, aes(x = growth, y = lag(growth))) + geom_point()
ggplot(dfGdp, aes(x = growth2, y = lag(growth2))) + geom_point()

tsGdp <- dfGdp %>% filter(between(as.numeric(format(dfGdp$DATE, "%Y")), 1980, 2019)) %>%
  pull(growth) %>%
  ts(frequency = 4, start = 1980)

plot(tsGdp)
acf(tsGdp, na.action = na.pass)

tsGdp <- ts(dfGdp$growth[-1], frequency = 4, start = 1947.25)
tsGdp
plot(tsGdp)
acf(tsGdp)

# Decomposition
dGdp <- dfGdp %>% filter(between(as.numeric(format(dfGdp$DATE, "%Y")), 1980, 2019)) %>%
  pull(gdp2) %>%
  ts(frequency = 4, start = 1980) %>%
  decompose("multiplicative")

str(dGdp)
plot(dGdp$seasonal)
plot(dGdp$trend)
plot(dGdp$random)
acf(dGdp$random, na.action = na.pass)

#### Bonus: WRDS Database info ####

res <- dbSendQuery(wrds, "select table_name,column_name
                   from information_schema.columns
                   where table_schema='information_schema'")
dfInfo <- dbFetch(res, n=-1) %>% as_tibble()
dbClearResult(res)

res <- dbSendQuery(wrds, "select distinct table_schema,table_type
                   from information_schema.tables
                   where table_type ='VIEW'
                   or table_type ='FOREIGN TABLE'")
dfDb <- dbFetch(res, n=-1)
dbClearResult(res)

res <- dbSendQuery(wrds, "select table_name,column_name,udt_name
                   from information_schema.columns
                   where table_schema='comp'")
dfComp <- dbFetch(res, n=-1) %>% as_tibble()
dbClearResult(res)

res <- dbSendQuery(wrds, "select table_name,column_name,udt_name
                   from information_schema.columns
                   where table_schema='crsp'")
dfCrsp <- dbFetch(res, n=-1) %>% as_tibble()
dbClearResult(res)


