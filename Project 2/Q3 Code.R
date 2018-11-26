library(readxl)
library(ggplot2)
library(zoo)

setwd("C:/Users/Laura/Desktop/MIT Fall 2018/Financial Data Science/Assignment/Assignment #5")

DF = read_excel("ProjectE Q3 Final Clean.xlsx")

DF_90 = subset(DF, DF$id_strike==90)
DF_95 = subset(DF, DF$id_strike==95)
DF_100 = subset(DF, DF$id_strike==100)

 

sf <- function (DF, action, Q)
# DF = data frame of 1 strike
# action = "Short" or "Long"
# of options bought or sold = ALWAYS POSITIVE  
{
  ## Variables Set Up
  m = as.data.frame(matrix(0,nrow = nrow(DF), ncol = 9))
  colnames(m) <- c("d", "Q", "q", "Cash","MV", "MV_temp", "PnL", "DIA_return", "delta")
  m$d = DF$d
  m$delta = DF$delta
  
  for (i in 1:nrow(DF))
  {
    if(i > 1)
    {
      m$DIA_return[i] = DF$spot[i]/DF$spot[i-1] - 1
    }
    
    # First Trading Day Set Up
    if (i == 1)
    {

      if(action == "Short")
      {
        m$Q[i] = -Q
        m$q[i] = -m$Q[i] * DF$delta[i]
        m$Cash[i] = -m$Q[i] * DF$ask[i] - m$q[i] * DF$spot[i]
        m$MV[i] = m$Q[i] * (DF$p[i] - DF$ask[i])

      }
      else
      {
        m$Q[i] = Q
        m$q[i] = -m$Q[i] * DF$delta[i]
        m$Cash[i] = -m$Q[i] * DF$bid[i] - m$q[i] * DF$spot[i]
        m$MV[i] = m$Q[i] * (DF$p[i] - DF$bid[i])
      }
    }
    
    # Other Trading Days
    if(i>1 & i<nrow(DF))
    {
      # PnL Calculation
      m$MV_temp[i] = m$Q[i-1] * DF$p[i] + m$q[i-1] * DF$spot[i] + ((1 + DF$rf[i]) * m$Cash[i-1] + DF$dividend[i] * DF$spot[i])
      m$PnL[i] = m$MV_temp[i] - m$MV_temp[i-1]
    
      # Rebalancing
      m$Q[i] = m$Q[i-1]
      m$q[i] = -m$Q[i] * DF$delta[i]
      m$Cash[i] = (1 + DF$rf[i]) * m$Cash[i-1] - (m$q[i] - m$q[i-1]) * DF$spot[i]
      m$MV[i] = m$Q[i] *DF$p[i] + m$q[i] *DF$spot[i] + m$Cash[i]
      
      
    }
    
    # Last Trading Day
    if(i==nrow(DF))
    {
      if(action == "Short")
      {
        m$Q[i] = 0
        m$q[i] = 0
        m$Cash[i] = (1 + DF$rf[i]) * m$Cash[i-1] + m$q[i-1] * DF$spot[i] + m$Q[i-1] * DF$bid[i] ### Is this right??? 
        m$MV[i] = m$Cash[i]
      }
      else
      {
        m$Q[i] = 0
        m$q[i] = 0
        m$Cash[i] = (1 + DF$rf[i]) * m$Cash[i-1] + m$q[i-1] * DF$spot[i] + m$Q[i-1] * DF$ask[i] ### Is this right??? 
        m$MV[i] = m$Cash[i]
      }
    }
    
  }
  return(m)  
}


#########################  Q3  #########################
call_90 = sf(DF_90, "Short", Q = 35)
call_95 = sf(DF_95, "Short", Q = 35)
call_100 = sf(DF_100, "Short", Q = 35)

### a

plot_MV <- ggplot(call_90, aes(x = as.Date(call_90$d))) + geom_line(aes(y = call_90$MV, color = "90 Strike - MV")) + 
  geom_line(aes(y = call_95$MV, color = "95 Strike - MV")) +
  geom_line(aes(y = call_100$MV, color = "100 Strike - MV")) + 
  scale_colour_manual("Legend",values = c("90 Strike - MV"="blue","95 Strike - MV"="red", "100 Strike - MV"="green")) +
  labs(title = "Short Call Market Value", x = "Date", y = "Market Value")
print(plot_MV)

plot_PnL <- ggplot(call_90, aes(x = as.Date(call_90$d))) + geom_line(aes(y = call_90$PnL, color = "90 Strike - PnL")) + 
  geom_line(aes(y = call_95$PnL, color = "95 Strike - PnL")) +
  geom_line(aes(y = call_100$PnL, color = "100 Strike - PnL")) + 
  scale_colour_manual("Legend",values = c("90 Strike - PnL"="blue","95 Strike - PnL"="red", "100 Strike - PnL"="green")) +
  labs(title = "Short Call PnL", x = "Date", y = "PnL")
print(plot_PnL)

### d
final_realized_PnL_90 <- sum(call_90$PnL)
final_realized_PnL_95 <- sum(call_95$PnL)
final_realized_PnL_100 <- sum(call_100$PnL)

final_realized_PnL_90
final_realized_PnL_95
final_realized_PnL_100

### e
PnL_sd_90 <- sd(call_90$PnL) * sqrt(261)
PnL_sd_95 <- sd(call_95$PnL) * sqrt(261)
PnL_sd_100 <- sd(call_100$PnL) * sqrt(261)

PnL_sd_90
PnL_sd_95
PnL_sd_100

### g
plot_delta <- ggplot(DF_90, aes(x = as.Date(DF_90$d))) + geom_line(aes(y = DF_90$delta, color = "90 Strike - Delta")) + 
  geom_line(aes(y = DF_95$delta, color = "95 Strike - Delta")) +
  geom_line(aes(y = DF_100$delta, color = "100 Strike - Delta")) + 
  scale_colour_manual("Legend",values = c("90 Strike - Delta"="blue","95 Strike - Delta"="red", "100 Strike - Delta"="green")) +
  labs(title = "Short Call Delta Absolute Value", x = "Date", y = "Delta")
print(plot_delta)

### h
rolling_vol_1mo <- rollapply(DF_90$spot, width = 21, FUN = sd)
date_vector <- as.Date(DF_90$d[21:261])
rolling_vol_1mo_char <- cbind.data.frame(date_vector, rolling_vol_1mo, DF_90$iv[21:261] )
colnames(rolling_vol_1mo_char) <- c("d", "vol", "ivol")

plot_vol <- ggplot(rolling_vol_1mo_char, aes(x = as.Date(rolling_vol_1mo_char$d))) + geom_line(aes(y = rolling_vol_1mo_char$vol, color = "1 Month Rolling Vol")) + 
  geom_line(aes(y = rolling_vol_1mo_char$ivol, color = "Implied Volatility")) +
  scale_colour_manual("Legend",values = c("1 Month Rolling Vol"="blue","Implied Volatility"="red")) +
  labs(title = "Volatilities", x = "Date", y = "Vol")
print(plot_vol)

### ???? maybe ask

### i
plot(call_90$DIA_return, call_90$PnL, xlab = "DIA Return", ylab = "P&L", main = "Daily P&L vs DIA Return")
head(call_90)

#########################  Q4  #########################
butterfly_90 = sf(DF_90, "Short", Q = 35)
butterfly_95 = sf(DF_95, "Long", Q = 70)
butterfly_100 = sf(DF_100, "Short", Q = 35)

butterfly_total <- as.data.frame(matrix(0,nrow = nrow(DF), ncol = 9))
colnames(butterfly_total) <- c("d", "Q", "q", "Cash","MV", "MV_temp", "PnL", "DIA_return", "delta")

butterfly_total$d = butterfly_90$d
butterfly_total$q = butterfly_90$q + butterfly_95$q + butterfly_100$q
butterfly_total$Cash = butterfly_90$Cash + butterfly_95$Cash + butterfly_100$Cash
butterfly_total$MV = butterfly_90$MV + butterfly_95$MV + butterfly_100$MV
butterfly_total$MV_temp = butterfly_90$MV_temp + butterfly_95$MV_temp + butterfly_100$MV_temp
butterfly_total$PnL = butterfly_90$PnL + butterfly_95$PnL + butterfly_100$PnL
butterfly_total$DIA_return = butterfly_90$DIA_return
butterfly_total$delta = 35 * butterfly_90$delta - 70 * butterfly_95$delta + 35 * butterfly_100$delta

### a
plot_MV_butterfly <- ggplot(butterfly_total, aes(x = as.Date(butterfly_total$d))) + geom_line(aes(y = butterfly_total$MV, color = "Butterfly - MV")) + 
  labs(title = "Butterfly Market Value", x = "Date", y = "Market Value")
print(plot_MV_butterfly)

plot_PnL_butterfly <- ggplot(butterfly_total, aes(x = as.Date(butterfly_total$d))) + geom_line(aes(y = butterfly_total$PnL, color = "Butterfly - PnL")) + 
  labs(title = "Butterfly Market Value", x = "Date", y = "PnL")
print(plot_PnL_butterfly)

### d
final_realized_PnL_butterfly <- sum(butterfly_total$PnL)
final_realized_PnL_butterfly

### e
PnL_sd_butterfly <- sd(butterfly_total$PnL) * sqrt(261)
PnL_sd_butterfly

### g
plot_delta_butterfly <- ggplot(butterfly_total, aes(x = butterfly_total$d)) + geom_line(aes(y = butterfly_total$delta, color = "Butterfly - Delta")) + 
  labs(title = "Butterfly Delta", x = "Date", y = "Delta")
print(plot_delta_butterfly)

### i
plot(butterfly_total$DIA_return, butterfly_total$PnL, xlab = "DIA Return", ylab = "P&L Butterfly", main = "Daily Buttefly P&L vs DIA Return")


