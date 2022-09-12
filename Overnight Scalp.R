library(tidyverse)
library(lubridate)
library(NMOF)
library(tidyverse)
library(lubridate)
library(janitor)
library(ks)
library(kableExtra)
library(kdensity)
library(KernSmooth)
library(plyr)
library(readxl)
library(roll)


riskfreerate <- read_excel("riskfreerate.xlsx")
riskfreerate <- riskfreerate%>%mutate(year=year(date),month=month(date))
riskfreerate <- subset(riskfreerate,select=c(rate,year,month))


#Ticker
ticker <- "AMZN"
#Data Retrieval and adding date specifics
data <- jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v3/historical-price-full/%s?apikey=c34a828bdd72b3933d449ea419de9562", ticker))$historical
data <- clean_names(data)
data <- data%>%mutate(wday =wday(date,week_start = 1))
data <- data%>%mutate(yweek = isoweek(date))
data <- data%>%mutate(year = year(date))
data <- data%>%mutate(month=month(date))
#Rounding the close
data <- data%>%mutate(roundedclose=round_any(close,5))
#

#Adding Risk free interest rate
func <- function(x,y){filter(riskfreerate,year==x&month==y)$rate}
dates <- select(data,date,year,month)
rfrvec <- numeric(nrow(dates))
for (i in 1:length(rfrvec)) {
  year <- slice(select(dates,year),i)$year
  month <- slice(select(dates,month),i)$month
  rfrvec[i] <- func(year,month)
}
data <- mutate(data,rfr=rfrvec)

#IV
data <- arrange(data,date)%>%mutate(iv=(roll_var(change_over_time,5))^0.5*sqrt(252))


#Adding expiry times
data <- mutate(data,t_sell=ifelse({wday==1},{96},{ifelse({wday==2},{72},{ifelse({wday==3},{48},{ifelse({wday==4},{24},{120})})})}))
data <- mutate(data,t_buy=ifelse({wday==1},{102.5},{ifelse({wday==2},{78.5},{ifelse({wday==3},{56.5},{ifelse({wday==4},{30.5},{6.5})})})}))

data <- data[complete.cases(data),]

#Creating Strategy
#Call Open
c_open_vec <- numeric(nrow(data))
for (i in 1:nrow(data)) {
  S <- slice(select(data,close),i)$close
  X <- slice(select(data,roundedclose),i)$roundedclose
  tau <- slice(select(data,t_sell),i)$t_sell*0.000114
  r <- slice(select(data,rfr),i)$rfr
  q <- 0
  vol <- slice(select(data,iv),i)$iv
  type <- "call"
  c_open_vec[i] <- vanillaOptionAmerican(S,X,tau,r,q,vol^2,type=type,greeks = TRUE)$value
}
#Put Open
p_open_vec <- numeric(nrow(data))
for (i in 1:nrow(data)) {
  S <- slice(select(data,close),i)$close
  X <- slice(select(data,roundedclose),i)$roundedclose
  tau <- slice(select(data,t_sell),i)$t_sell*0.000114
  r <- slice(select(data,rfr),i)$rfr
  q <- 0
  vol <- slice(select(data,iv),i)$iv
  type <- "put"
  p_open_vec[i] <- vanillaOptionAmerican(S,X,tau,r,q,vol^2,type=type,greeks = TRUE)$value
}

#Call Close
c_close_vec <- numeric(nrow(data))
for (i in 1:nrow(data)) {
  S <- slice(select(data,open),i+1)$open
  X <- slice(select(data,roundedclose),i)$roundedclose
  tau <- slice(select(data,t_buy),i+1)$t_buy*0.000114
  r <- slice(select(data,rfr),i+1)$rfr
  q <- 0
  vol <- slice(select(data,iv),i+1)$iv
  type <- "call"
  c_close_vec[i] <- vanillaOptionAmerican(S,X,tau,r,q,vol^2,type=type,greeks = TRUE)$value
}


#Put Close
p_close_vec <- numeric(nrow(data))
for (i in 1:nrow(data)) {
  S <- slice(select(data,open),i+1)$open
  X <- slice(select(data,roundedclose),i)$roundedclose
  tau <- slice(select(data,t_buy),i+1)$t_buy*0.000114
  r <- slice(select(data,rfr),i+1)$rfr
  q <- 0
  vol <- slice(select(data,iv),i+1)$iv
  type <- "put"
  p_close_vec[i] <- vanillaOptionAmerican(S,X,tau,r,q,vol^2,type=type,greeks = TRUE)$value
}


results <- data.frame("c_open"=c_open_vec,"p_open"=p_open_vec,"c_close"=c_close_vec,"p_close"=p_close_vec)
results <- mutate(results,profit=100*(c_open-c_close+p_open-p_close),cum=cumsum(profit),trade=seq(1,nrow(results),1))


ggplot(results,mapping = aes(x=trade,y=cum))+
  geom_line()

wr <- sum(0<=results$profit)/nrow(results)
rr <- mean(0<=results$profit)/mean(results$profit<0)
bw <- max(results$profit)
ll <- min(results$profit)

returns <- as.numeric(c(round(diff(log(data$close)),3)))





