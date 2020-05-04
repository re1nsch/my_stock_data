library(quantmod)
library(tidyverse)
library(haven)
library(scales)

stocklist_full <-read_csv2("./stock_symbols.csv")
stocklist_symbole <- stocklist_full$Symbole
stocklist_name <- stocklist_full$Name

df_stocklist <- data.frame(0,0,0,0,0,0,0,0,0)
names(df_stocklist) <- c("Symbol",
                         "Name",
                         "Current",
                         "Max_Total",
                         "Min2008",
                         "Max2020",
                         "Min2020",
                         "Max2020 vs Min2020",
                         "Max2020 vs Current")
df_stocklist <- rbind(df_stocklist, NULL)

for (idx in seq(length(stocklist_symbole))) {
  stocksymbole_index <- stocklist_symbole[idx]
  stockname_index <- stocklist_name[idx]
  
  raw_stockdata <- NULL
  raw_stockdata <- getSymbols(stocksymbole_index, from = "2000-01-01", to = Sys.Date(), env=raw_stockdata)
  temp_df <- fortify(raw_stockdata) %>% 
  setNames(c("Date",
             "Open",
             "High",
             "Low",
             "Close",
             "Volume",
             "Adjusted"))

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
  
  max_total <- temp_df %>%
    filter(Date<=Sys.Date()) %>%
    summarize(tmp = max(Close, na.rm = TRUE)) %>%
    `[[`(1)
  
  current <- temp_df$Close[nrow(temp_df)]
  
  max_vs_corona <- 100/max2020*(min2020-max2020)
  max_vs_current <- 100/max2020*(current-max2020)

  df_tmp <- data.frame(stocksymbole_index,
                       stockname_index,
                       round(current,1),
                       round(max_total,1),
                       round(min2008,1),
                       round(max2020,1),
                       round(min2020,1),
                       round(max_vs_corona,1),
                       round(max_vs_current,1))
  names(df_tmp) <- c("Symbol",
                     "Name",
                     "Current",
                     "Max_Total",
                     "Min2008",
                     "Max2020",
                     "Min2020",
                     "Max2020 vs Min2020",
                     "Max2020 vs Current")
  df_stocklist <- rbind(df_tmp, df_stocklist)
}

remove(min2008,
       min2020,
       max2020,
       max_total,
       current,
       max_vs_corona,
       max_vs_current,
       idx,
       stocksymbole_index,
       stockname_index,
       stocklist_name,
       stocklist_symbole,
       df_tmp,
       temp_df,
       stocklist_full,
       raw_stockdata)

df_stocklist <- df_stocklist[-c(nrow(df_stocklist)),]
df_stocklist <- df_stocklist %>%
  map_df(rev)

# percent(df_stocklist$DropCorona/100)

#raw_stockdata <- NULL
#raw_stockdata <- getSymbols("L0CK.F", from = "2000-01-01", to = Sys.Date(), env=raw_stockdata)