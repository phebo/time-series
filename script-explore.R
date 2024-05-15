library(tidyverse) # For tibble tools, CSV reader, ggplot, etc.
library(RPostgres)

#### Basic commands in R ####

# Internal data sets in R
?data
data()
data("JohnsonJohnson") # Quarterly earnings (dollars) per Johnson & Johnson share 1960–80.
str(JohnsonJohnson)
JohnsonJohnson

# Some plots
plot(JohnsonJohnson)
plot(log(JohnsonJohnson))
diff(log(JohnsonJohnson))
plot(diff(log(JohnsonJohnson)))

# Decomposition
?decompose
str(decompose(JohnsonJohnson))
plot(decompose(JohnsonJohnson))
plot(decompose(JohnsonJohnson, type = "multiplicative"))
plot(decompose(log(JohnsonJohnson)))
plot(stl(log(JohnsonJohnson), "periodic"))
plot(stl(log(JohnsonJohnson), s.window = 7)) # Allows changing seasonality pattern over time
plot(stl(log(JohnsonJohnson), s.window = 13)) # Allows changing seasonality pattern over time
plot(stl(log(JohnsonJohnson), s.window = 21))

#### Download external data ####

writeData <- T
dataDir <- "data"

# Economic data from https://fred.stlouisfed.org/
dfRf <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=DGS10&scale=left&cosd=1962-01-02&coed=2021-01-07&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Daily&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2021-01-11&revision_date=2021-01-11&nd=1962-01-02",
                 col_types = list(DGS10 = col_double()))
dfCpi <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CPIAUCSL&scale=left&cosd=1947-01-01&coed=2021-02-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2021-03-26&revision_date=2021-03-26&nd=1947-01-01")
dfGdp <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=968&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=GDP,ND000334Q,NA000334Q,GDPC1&scale=left,left,left,left&cosd=1947-01-01,2002-01-01,1947-01-01,1947-01-01&coed=2022-01-01,2022-01-01,2022-01-01,2022-01-01&line_color=%234572a7,%23aa4643,%2389a54e,%2380699b&link_values=false,false,false,false&line_style=solid,solid,solid,solid&mark_type=none,none,none,none&mw=3,3,3,3&lw=2,2,2,2&ost=-99999,-99999,-99999,-99999&oet=99999,99999,99999,99999&mma=0,0,0,0&fml=a,a,a,a&fq=Quarterly,Quarterly,Quarterly,Quarterly&fam=avg,avg,avg,avg&fgst=lin,lin,lin,lin&fgsnd=2020-02-01,2020-02-01,2020-02-01,2020-02-01&line_index=1,2,3,4&transformation=lin,lin,lin,lin&vintage_date=2022-05-30,2022-05-30,2022-05-30,2022-05-30&revision_date=2022-05-30,2022-05-30,2022-05-30,2022-05-30&nd=1947-01-01,2002-01-01,1947-01-01,1947-01-01",
                  col_types = list(ND000334Q = col_double()))
# Seasonaly adjusted: GDP (nominal, $B), GDPC1 (real, 2012 $B)
# Non-adjusted: NA000334Q (nominal, $M), ND000334Q (real, 2012 $B)

# To set up querying WRDS from R, see:
# - https://wrds-www.wharton.upenn.edu/pages/support/programming-wrds/programming-r/r-from-your-computer/
# - https://wrds-www.wharton.upenn.edu/pages/support/programming-wrds/programming-r/advanced-topics-in-r/querying-wrds-data-r/
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
  dir.create(dataDir)
  write_csv(dfRf, file.path(dataDir, "rf.csv"))
  write_csv(dfCpi, file.path(dataDir, "cpi.csv"))
  write_csv(dfGdp, file.path(dataDir, "gdp.csv"))
  write_csv(dfCompu, file.path(dataDir, "compu.csv"))
  write_csv(dfIndex, file.path(dataDir, "index.csv"))
}

#### Analysis ####

View(dfGdp)
dfGdp2 <- dfGdp %>% 
  select(
    date = DATE,
    gdp = GDP,  # nominal gdp $B, seasonality adjusted
    gdp2 = NA000334Q) %>%  # nomional gdp $M, non-seasonality adjusted
  mutate(
    growth = gdp / lag(gdp) - 1,
    growth2 = gdp2 / lag(gdp2) - 1,
    index = gdp / gdp[1] * 100,
    index2 = gdp2 / gdp2[1] * 100)
ggplot(dfGdp2, aes(x = date, y = gdp)) + geom_line() +
  scale_y_log10()
ggplot(dfGdp2, aes(x = date, y = gdp2)) + geom_line() + scale_y_log10()
dfGdp2 %>% select(date, index, index2) %>% pivot_longer(c(index, index2)) %>%
  ggplot(aes(x = date, y = value, color = name)) + geom_line() + scale_y_log10()
dfGdp2 %>% select(date, growth, growth2) %>% pivot_longer(c(growth, growth2)) %>%
  ggplot(aes(x = date, y = value, color = name, group=name)) + geom_line()

tsGrowth <- ts(dfGdp2$growth[-1], frequency = 4, start = 1947.25)
tsGrowth
plot(tsGrowth)

tsGdp <- ts(dfGdp2$index, frequency = 4, start = 1947)
tsGdp2 <- ts(dfGdp2$index2, frequency = 4, start = 1947)
plot(tsGdp)
plot(tsGdp2)

# Decomposition
plot(log(tsGdp))
dGdp <- decompose(log(tsGdp2))
plot(dGdp)
dGdp2 <- stl(log(tsGdp2), s.window = 7)
plot(dGdp2)
dGdp3 <- stl(log(tsGdp2), s.window = 21)
plot(dGdp3)
dGdp4 <- stl(log(tsGdp), s.window = 21) 
plot(dGdp4) # Some seasonality left in the FED's "seasonality adjusted" data?

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
