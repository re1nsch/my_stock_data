library(quantmod)
library(tidyverse)
library(haven)
library(scales)

stocklist_full <-read_csv2("./stock_symbols.csv")
stocklist_symbole <- stocklist_full$Symbole
stocklist_name <- stocklist_full$Name

df_stocklist <- data.frame(0,0,0,0,0,0,0,0,0)
names(df_stocklist)<-c("Symbol",
                       "Name",
                       "Current",
                       "Max_Total",
                       "Min2008",
                       "Max2020",
                       "Min2020",
                       "DropCorona",
                       "PlusCorona")
df_stocklist <- rbind(df_stocklist, NULL)

for (idx in seq(length(stocklist_symbole))){
  stocksymbole_index <- stocklist_symbole[idx]
  stockname_index <- stocklist_name[idx]
  
  raw <- NULL
  raw <- getSymbols(stocksymbole_index, from = "2000-01-01", to = Sys.Date(), env=raw)
  temp_df <- fortify(raw) %>% 
  setNames(c("Date",
             "Open",
             "High",
             "Low",
             "Close",
             "Volume",
             "Adjusted"))
  
  temp <- temp_df %>% 
    filter(Date<="2010-01-01" & Date>="2007-01-01")
  min2008 <- min(temp$Close, na.rm = TRUE)
  
  temp <- temp_df %>% 
    filter(Date<=Sys.Date() & Date>="2020-01-01")
  min2020 <- min(temp$Close, na.rm = TRUE)
  
  max_total <- max(temp_df$Close, na.rm = TRUE)
  
  temp <- temp_df %>% 
    filter(Date<=Sys.Date() & Date>="2020-01-01")
  max2020 <- max(temp$Close, na.rm = TRUE)
  
  current <- temp_df$Close[nrow(temp_df)]
  
  drop_corona <- 100/max2020*abs(min2020-max2020)
  rise_corona <- 100/max2020*abs(current-max2020)
  
  df<-data.frame(stocksymbole_index,
                 stockname_index,
                 round(current,1),
                 round(max_total,1),
                 round(min2008,1),
                 round(max2020,1),
                 round(min2020,1),
                 round(drop_corona,1),
                 round(rise_corona,1))

  names(df)<-c("Symbol",
               "Name",
               "Current",
               "Max_Total",
               "Min2008",
               "Max2020",
               "Min2020",
               "DropCorona",
               "PlusCorona")

  df_stocklist <- rbind(df, df_stocklist)
}
df_stocklist <- df_stocklist[-c(nrow(df_stocklist)),]
df_stocklist <- df_stocklist %>%
  map_df(rev)

percent(df_stocklist$DropCorona/100)