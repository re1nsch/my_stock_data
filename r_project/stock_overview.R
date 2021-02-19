library(quantmod)
library(tidyverse)
library(haven)
library(scales)
library(ggplot2)
library(dplyr)

stocklist_full <- read_csv2("./data/stock_symbols.csv")

stocklist_symbole <- stocklist_full$Symbole
stocklist_name <- stocklist_full$Name
stocklist_country <- stocklist_full$Country
stocklist_sector <- stocklist_full$Sector
stocklist_branch <- stocklist_full$Branch
stocklist_currency <- stocklist_full$Currency

df_stocklist <- data.frame(0,0,0,0,0,0,0,0,0,0,0,0,0,0)
names(df_stocklist) <- c("Symbol",
                         "Name",
                         "Country",
                         "Sector",
                         "Branch",
                         "Current",
                         "Maxtotal",
                         "Min2008",
                         "Max2020",
                         "Min2020",
                         "Daily",
                         "Weekly",
                         "MaxMin2020",
                         "MaxCur2020")
df_stocklist <- rbind(df_stocklist, NULL)

for (idx in seq(length(stocklist_symbole))) {
#for (idx in seq(1:1)) {
  stocksymbole_index <- stocklist_symbole[idx]
  stockname_index <- stocklist_name[idx]
  stockcountry_index <- stocklist_country[idx]
  stocksector_index <- stocklist_sector[idx]
  stockbranch_index <- stocklist_branch[idx]
  
  raw_stockdata <- NULL
  suppressWarnings(
    raw_stockdata <- getSymbols(
      stocksymbole_index,
      from = "2000-01-01",
      to = Sys.Date(),
      env=raw_stockdata))
  
  temp_df <- fortify(raw_stockdata) %>% 
  setNames(c("Date",
             "Open",
             "High",
             "Low",
             "Close",
             "Volume",
             "Adjusted"))
  
  print(stockname_index)
  
  plot1 <- temp_df %>% 
    ggplot(aes(x=Date, y=Close)) +
      geom_line(colour="red3") +
      theme_minimal() +
      theme(legend.position = "none")
  
  plot2 <- temp_df %>% 
    filter(Date >= Sys.Date()-365) %>% 
    ggplot(aes(x=Date, y=Close)) +
      geom_line(colour="green3") + 
      theme_minimal() +
      theme(legend.position = "none")
  
  print(plot1)
  print(plot2)
  
  min2008 <- temp_df %>%
    filter(Date<="2010-01-01" & Date>="2007-01-01") %>%
    summarize(tmp = min(Close, na.rm = TRUE)) %>%
    `[[`(1)
  
  min2020 <- temp_df %>%
    filter(Date<="2020-04-01" & Date>="2020-01-01") %>%
    summarize(tmp = min(Close, na.rm = TRUE)) %>%
    `[[`(1)

  max2020 <- temp_df %>%
    filter(Date<="2020-03-23" & Date>="2020-01-01") %>%
    summarize(tmp = max(Close, na.rm = TRUE)) %>%
    `[[`(1)
  
  maxtotal <- temp_df %>%
    filter(Date<=Sys.Date()) %>%
    summarize(tmp = max(Close, na.rm = TRUE)) %>%
    `[[`(1)
  
  current <- temp_df$Close[nrow(temp_df)]
  week <- temp_df$Close[nrow(temp_df)-7]
  day <- temp_df$Close[nrow(temp_df)-1]
  
  maxmin2020 <- 100/max2020*(min2020-max2020)
  maxcur2020 <- 100/max2020*(current-max2020)
  week_dev <- 100*(current-week)/week
  day_dev <- 100*(current-day)/day

  df_tmp <- data.frame(stocksymbole_index,
                       stockname_index,
                       stockcountry_index,
                       stocksector_index,
                       stockbranch_index,
                       round(current,1),
                       round(maxtotal,1),
                       round(min2008,1),
                       round(max2020,1),
                       round(min2020,1),
                       round(day_dev,1),
                       round(week_dev,1),
                       round(maxmin2020,1),
                       round(maxcur2020,1))
  names(df_tmp) <- c("Symbol",
                     "Name",
                     "Country",
                     "Sector",
                     "Branch",
                     "Current",
                     "Maxtotal",
                     "Min2008",
                     "Max2020",
                     "Min2020",
                     "Daily",
                     "Weekly",
                     "MaxMin2020",
                     "MaxCur2020")
  df_stocklist <- rbind(df_tmp, df_stocklist)
}

#remove(...)

df_stocklist <- df_stocklist[-c(nrow(df_stocklist)),]
df_stocklist$Name <- as.character(df_stocklist$Name)
df_stocklist$Symbol <- as.character(df_stocklist$Symbol)
df_stocklist$Sector <- as.character(df_stocklist$Sector)
df_stocklist$Branch <- as.character(df_stocklist$Branch)
df_stocklist$Country <- as.character(df_stocklist$Country)

df_stocklist <- df_stocklist[order(df_stocklist$Name),]

#percent(df_stocklist$DropCorona/100)
#raw_stockdata <- NULL
#raw_stockdata <- getSymbols("WDI.F", from = "2020-01-01", to = Sys.Date(), env=raw_stockdata)
#plot(raw_stockdata$WDI.F.Close)