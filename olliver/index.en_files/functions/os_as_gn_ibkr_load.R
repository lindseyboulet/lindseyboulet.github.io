pkgs <- c("ggplot2", "plyr", "dplyr","plotly", "tidyquant",
"tidyverse", "lubridate", "forcats", "RColorBrewer", "blogdown")
for(i in 1:length(pkgs)){
is_installed <- function(mypkg){is.element(mypkg, installed.packages()[,1])}
  if(!is_installed(pkgs[i])){install.packages(pkgs[i], repos="http://lib.stat.cmu.edu/R/CRAN")}
  library(pkgs[i], character.only=TRUE, quietly=TRUE,verbose=FALSE)}

fn <- list.files('index.en_files/data', full.names = TRUE)

dList <- lapply(fn, read.table, header = FALSE, sep = ",",
                col.names = paste0("V",seq_len(16)), fill = TRUE)
print(getwd())
trade_extr <- function(df){
  df2 <- df %>%
    filter(V1 == "Trades")
  colnames(df2) <-  df2  %>% filter(V2 == "Header") %>% unlist()
  df2 %>%
    filter(DataDiscriminator == "Order")%>%
    mutate(`Date/Time` = ymd_hms(`Date/Time`),
           ind.basis = as.numeric(Basis)/as.numeric(Quantity)) %>%
    arrange(`Date/Time`)
  }
fund_extr <- function(df){
  df2 <- df %>%
    filter(V1 == "Deposits & Withdrawals") %>%
    select(V1:V6)
  colnames(df2) <-  df2  %>% filter(V2 == "Header") %>% unlist()
  df2 %>% 
    filter(Currency == "CAD") %>%
    mutate(`Settle Date` = ymd(`Settle Date`)) %>%
    arrange(`Settle Date`)
}

# Funds Analysis ----------------------------------------------------------

dfFunds <- ldply(dList, fund_extr)
# sum(as.numeric(dfFunds$Amount))
# dfFunds %>%
#   select(`Settle Date`, Amount, Description) %>%
#   mutate(Investor = "") %>%
#  write.csv('./output/settle_date.csv', row.names = FALSE)
dfInvID <- read.csv('index.en_files/output/settle_date_inv.csv') %>%
  mutate(Settle.Date = mdy(Settle.Date))

dfFunds <- 
  dfFunds %>%
  mutate(`Settle Date` = ymd(`Settle Date`),
         Amount = as.numeric(Amount)) %>%
  left_join(dfInvID, by = c(`Settle Date` = 'Settle.Date',
                            "Amount" = "Amount",
                            'Description' = 'Description')) %>%
  filter(!is.na(Investor))
dfFunds %>% 
  group_by(Investor) %>% 
  mutate(cum_sum = cumsum(Amount)) %>%
  ggplot(aes(`Settle Date`, y=cum_sum, color = Investor)) + 
  geom_line() + 
  geom_point()


# Trade Analysis ----------------------------------------------------------

dfTrade <- ldply(dList, trade_extr)
# dfTrade %>%
#   mutate(Quantity = as.numeric(Quantity),
#          Investor = NA) %>%
#   uncount(Quantity, .remove = FALSE) %>%
#   mutate(NewQ = 1) %>%
#  write.csv('./output/trades.csv', row.names = FALSE)
dfTrade <- read.csv('index.en_files/output/trades_inv.csv')

dfTrade %>% 
  group_by(Investor) %>% 
  mutate(cum_sum = cumsum(ind.basis),
         Date.Time = mdy_hm(Date.Time)) %>%
  ggplot(aes(x = Date.Time, y=cum_sum, color = Investor)) + 
  geom_line() + 
  geom_point()

# Summary -----------------------------------------------------------------
getSymbols(paste0(unique(dfTrade$Symbol),".TO"),warnings = FALSE,
           auto.assign = TRUE)
XEQT.TO <- data.frame(XEQT.TO %>% tail(1) %>% unlist())

VGRO.TO <- data.frame(VGRO.TO %>% tail(1) %>% unlist())

dfCurr <- bind_rows(data.frame('Symbol' = 'XEQT', 'current.price' = XEQT.TO[['XEQT.TO.Close']]), 
          data.frame('Symbol' = 'VGRO', 'current.price' = VGRO.TO[['VGRO.TO.Close']]))


dfSum <- 
  dfTrade%>%
  group_by(Symbol, Investor) %>%
  summarise(average = mean(ind.basis),
            shares = sum(NewQ)) %>%
  left_join(dfCurr) %>%
  mutate(cost = average * shares,
         value = current.price * shares,
         pl = value - cost)

dfout <- dfSum%>%
  group_by(Investor) %>%
  summarise(total_cost = sum(cost),
            total_value = sum(value),
            total_pl = sum(pl)
            )


