library(crypto2)
library(tidyverse)
library(stats)
library(MASS)
library(AER)
library(corrplot)

options(digits=10)

# coins <- crypto_list(only_active=TRUE)
# 
# coin_info <- crypto_info(coins,limit=1)

endDate <- "20220202"

# coin_hist_daily <- crypto_history(coins, limit=1, end_date=endDate)
# coin_hist_monthly <- crypto_history(coins, limit=1, end_date=endDate, interval = "monthly")
# 
# write.csv(coin_hist_daily,"data/bitcoin_history_daily.csv", row.names = FALSE)
# write.csv(coin_hist_monthly,"data/bitcoin_history_monthly.csv", row.names = FALSE)

coin_hist_daily <- read_csv("data/bitcoin_history_daily.csv")
coin_hist_monthly <- read_csv("data/bitcoin_history_monthly.csv")

summary(coin_hist_daily[c("open", "high", "low", "close", "volume", "market_cap")])

coin_hist_daily %>% 
  ggplot(aes(as.Date(timestamp), high)) +
  geom_line( color="#F2A900" ) +
  scale_x_date(date_labels = "%b-%d-%Y") +
  theme_minimal() +
  labs(title = "Bitcoin Highest Price (Daily)", x = "Date", y = "High (USD)")
  
coin_hist_monthly %>% 
  ggplot(aes(as.Date(timestamp), high)) +
  geom_line( color="#F2A900" ) +
  scale_x_date(date_labels = "%b-%d-%Y") +
  theme_minimal() +
  labs(title = "Bitcoin Highest Price (Monthly)", x = "Date", y = "High (USD)")


# plot(coin_hist_daily$close ~ tw_uncer$`TEU-ENG`)

# Daily data set
coin_hist_daily$date <- as.Date(coin_hist_daily$timestamp)
tw_uncer$date <- as.Date(tw_uncer$date)
datasetDaily <- left_join(coin_hist_daily, tw_uncer, by = c("date" = "date"))

# VIX
colnames(vix) <- paste0("VIX_", colnames(vix))
datasetDaily <- left_join(datasetDaily, vix, by = c("date" = "VIX_Date"))

# Gold Price
goldSelect <- gold[, c(1, 2)]
colnames(goldSelect) <- paste0("GOLD_", colnames(goldSelect))
datasetDaily <- left_join(datasetDaily, goldSelect, by = c("date" = "GOLD_Name"))

# Gold VIX (GVZCLS)
datasetDaily <- left_join(datasetDaily, gold_vix, by = c("date" = "DATE"))

# Return
datasetDaily <- datasetDaily %>%  mutate(return = log(close/lag(close)))
# mutate(return = log((close - lag(close))/lag(close)))
# log(prices[-1]/prices[-n]) | diff(log(prices), lag=1) | diff(as.matrix(log(prices[,-1]))) | ROC(datasetDaily[,"close"])
write.csv(datasetDaily,"data/datasetDaily.csv", row.names = FALSE)


plot(datasetDaily$return ~ datasetDaily$`TEU-ENG`)


# Analysis ----------------------------------------------------------------

datasetDailyAnalysis <- subset(datasetDaily, select = c("close", "market_cap", "TEU-ENG", "TMU-ENG", "GOLD_US dollar...2", "GVZCLS", "return", "volume"))

datasetDailyAnalysis <- na.omit(datasetDailyAnalysis)
corrResult <- cor(datasetDailyAnalysis)

cor(datasetDailyAnalysis$`GOLD_US dollar...2`, datasetDailyAnalysis$close)
corrplot(corrResult, method="number", type="lower")

model <- lm(return ~ log(GVZCLS) + log(`GOLD_US dollar...2`) + log(`TEU-ENG`) + volume, data = datasetDailyAnalysis)
summary(model)


# Analysis Monthly --------------------------------------------------------



coin_hist_monthly$date <- as.Date(coin_hist_monthly$timestamp)
epu_uncer$Date <- as.Date(epu_uncer$Date)
datasetMonthly <- left_join(coin_hist_monthly, epu_uncer[ , c("GEPU_current", "GEPU_ppp", "Date")], by = c("date" = "Date"))

gti$Month <- as.Date(gti$Month)
colnames(gti) <- paste0("GTI_", colnames(gti))
datasetMonthly <- left_join(datasetMonthly, gti, by = c("date" = "GTI_Month"))

gpr$month <- as.Date(gpr$month)
datasetMonthly <- left_join(datasetMonthly, gpr[ , c("GPR", "month")], by = c("date" = "month"))

colnames(gold_monthly) <- paste0("Gold_", colnames(gold_monthly))
datasetMonthly$Month_Yr <- format(as.Date(datasetMonthly$date), "%Y-%m")
gold_monthly$Month_Yr <- format(as.Date(gold_monthly$Gold_Date), "%Y-%m")

datasetMonthly <- left_join(datasetMonthly, gold_monthly[, c("Month_Yr", "Gold_United States(USD)")], by = c("Month_Yr" = "Month_Yr"))

datasetMonthly <- datasetMonthly %>%  mutate(return = log(close/lag(close)))
datasetMonthly <- datasetMonthly %>%  mutate(GEPU_log = log(GEPU_current/lag(GEPU_current)))
datasetMonthly <- datasetMonthly %>%  mutate(GPR_log = log(GPR/lag(GPR)))

datasetMonthly$volume[datasetMonthly$volume == 0] <- NA
# datasetMonthly <- na.omit(datasetMonthly)

model <- lm(return ~ GPR_log + log(volume), data = datasetMonthly)
summary(model)

model <- lm(return ~ GPR_log  + log(GTI_Bitcoin) + log(volume), data = datasetMonthly)
summary(model)

model1 <- lm(return ~ GEPU_log + log(volume) + log(GTI_Bitcoin), data = datasetMonthly)
summary(model1)

# daily to monthly
teu_monthly <- aggregate(tw_uncer$`TEU-ENG`, list(format(tw_uncer$date, "%Y-%m")), mean)
names(teu_monthly) <- c("date", "TEU")
teu_monthly$date <- as.Date(paste(teu_monthly$date,"-01",sep=""))

datasetMonthly <- left_join(datasetMonthly, teu_monthly, by = c("date" = "date"))

datasetMonthly <- datasetMonthly %>%  mutate(TEU_log = log(TEU/lag(TEU)))
model2 <- lm(return ~ TEU_log + log(volume) + log(GTI_Bitcoin) + log(`Gold_United States(USD)`), data = datasetMonthly)
summary(model2)

vix_monthly <- aggregate(vix$VIX_Close, list(format(vix$VIX_Date, "%Y-%m")), mean)
names(vix_monthly) <- c("date", "VIX")
vix_monthly$date <- as.Date(paste(vix_monthly$date,"-01",sep=""))

datasetMonthly <- left_join(datasetMonthly, vix_monthly, by = c("date" = "date"))

datasetMonthly <- datasetMonthly %>%  mutate(VIX_log = log(VIX/lag(VIX)))
model3 <- lm(return ~ VIX_log + log(volume), data = datasetMonthly)
summary(model3)
  
model4 <-lm(return ~ VIX_log + log(volume) + log(GTI_Bitcoin), data = datasetMonthly)
summary(model4)
  
psych::describe(datasetMonthly)
qt(0.1, 93, lower.tail=FALSE)
runif(1, min=1.290, max=1.660)

plot <- ggplot(datasetMonthly, aes(x = date, y = VIX)) +
  geom_line(color="#77a8a7", size = 1) + theme_classic() + scale_y_continuous(labels=comma) 
plot

testModel <- lm(return ~ GEPU_log + log(GTI_Bitcoin) + log(volume) + log(`Gold_United States(USD)`) + log(high), data = datasetMonthly)

durbinWatsonTest(model1)



