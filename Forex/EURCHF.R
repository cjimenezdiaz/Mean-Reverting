# Cleaning the Environment
rm(list=setdiff(ls(), ""))
dev.off()

# Libraries
if (!require("tidyquant")) install.packages("tidyquant"); library(tidyquant)
if (!require("dplyr")) install.packages("dplyr"); library(dplyr)
if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)
if (!require("TTR")) install.packages("TTR"); library(TTR)
if (!require("lubridate")) install.packages("lubridate"); library(lubridate)
if (!require("gridExtra")) install.packages("gridExtra"); library(gridExtra)

# Variables
Periodos  <- 5 # SMA period
k         <- 1 # sd factor

# Ddownloading the data
dataset.Precios <- tq_get("EURCHF=X",
                          get = "stock.prices", 
                          from = Sys.Date() - years(4)) %>%
  as.data.frame() %>%
  select(2:6) %>%
  na.omit()

# Strategy Builder
BBands_data <- BBands(as.matrix(dataset.Precios[,c(3:5)]), n = Periodos, sd = k) %>% as.data.frame()

dataset.Strategy <- dataset.Precios %>%
  mutate(Return = ROC(close, n = 1, type = "discrete"),
         SMA_Close = SMA(close, n = Periodos)) %>%
  mutate(Upper_Band = BBands_data$up,
         Lower_Band = BBands_data$dn) %>%
  na.omit() 

# Signal (Buy or Sell)
dataset.Strategy$Signal <- 0
for(i in 1:nrow(dataset.Strategy)){
  
  if(dataset.Strategy$close[i] > dataset.Strategy$Upper_Band[i] && i < nrow(dataset.Strategy)){
    dataset.Strategy$Signal[i] <- -1
    i = i + 1
    while(dataset.Strategy$close[i] > dataset.Strategy$SMA_Close[i] && i < nrow(dataset.Strategy)){
      dataset.Strategy$Signal[i] <- -1
      i = i + 1        
    }
  }
  
  if(dataset.Strategy$close[i] < dataset.Strategy$Lower_Band[i] && i < nrow(dataset.Strategy)){
    dataset.Strategy$Signal[i] <- 1
    i = i + 1
    while(dataset.Strategy$close[i] < dataset.Strategy$SMA_Close[i] && i < nrow(dataset.Strategy)){
      dataset.Strategy$Signal[i] <- 1
      i = i + 1        
    }
  }
}

# Acum Return and Drawdown calculation
dataset.Strategy <- dataset.Strategy %>%
  mutate(Strategy_Return = lead(Return, n = 1)*Signal) %>%
  na.omit() %>%
  mutate(Acum_Return = cumsum(Strategy_Return),
         Drawdown = (Acum_Return + 1) / cummax(Acum_Return + 1) - 1) %>%
  na.omit()

# Statistical Analysis
DDBB_Statistic <- dataset.Strategy %>%
  select(date, Strategy_Return) %>%
  remove_rownames() %>%
  column_to_rownames(var = "date") %>%
  as.xts() %>%
  table.AnnualizedReturns(scale = 252)

# Charts
p <- dataset.Strategy %>%
  ggplot(aes(x = as.Date(date), y = Acum_Return)) +
  geom_line(colour = "steelblue", size = 1, alpha = 0.7) + 
  theme_gray() +
  labs(title = "Strategy Performance",
       subtitle = "Daily trading on the EURCHF",
       caption = "",
       x = "",
       y = "Acum Return") +
  scale_y_continuous(labels = scales::percent)

d <- dataset.Strategy %>%
  ggplot(aes(x = as.Date(date), y = Drawdown)) +
  geom_line(colour = "steelblue", size = 1, alpha = 0.7) + 
  theme_gray() +
  labs(title = "",
       subtitle = "",
       caption = paste("CAGR: ", round(DDBB_Statistic[1,1]*100, digits = 2), "% - MaxDD: ", round(min(dataset.Strategy$Drawdown)*100, digits = 2), "% - Sharpe: ", round(DDBB_Statistic[3,1], digits = 3), "\nBy: Carlos Jimenez",sep = ""),
       x = "Dates",
       y = "Drawdown") +
  scale_y_continuous(labels = scales::percent)

grid.arrange(p, d, nrow = 2)
