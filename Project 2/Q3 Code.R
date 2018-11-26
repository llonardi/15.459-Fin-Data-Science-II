library(readxl)
library(ggplot2)

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
  m = as.data.frame(matrix(0,nrow = nrow(DF), ncol = 7))
  colnames(m) <- c("d", "Q", "q", "Cash","MV", "MV_temp", "PnL")
  m$d = DF$d

  for (i in 1:nrow(DF))
  {
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

call_90 = sf(DF_90, "Short", Q = 35)
call_95 = sf(DF_95, "Short", Q = 35)
call_100 = sf(DF_100, "Short", Q = 35)

write.csv(DF_90, "test.csv")
write.csv(call_90, "test2.csv")

### a

plot_MV <- ggplot(call_90, aes(x = as.Date(call_90$d))) + geom_line(aes(y = call_90$MV, color = "90 Strike - MV")) + 
  geom_line(aes(y = call_95$MV, color = "95 Strike - MV")) +
  geom_line(aes(y = call_100$MV, color = "100 Strike - MV")) + 
  scale_colour_manual("Legend",values = c("90 Strike - MV"="blue","95 Strike - MV"="red", "100 Strike - MV"="green")) +
  labs(title = "Short Call Market Value", x = "Date", y = "Market Value")
print(plot_MV)

 ### g
plot_delta <- ggplot(DF_90, aes(x = as.Date(DF_90$d))) + geom_line(aes(y = DF_90$delta, color = "90 Strike - Delta")) + 
  geom_line(aes(y = DF_95$delta, color = "95 Strike - Delta")) +
  geom_line(aes(y = DF_100$delta, color = "100 Strike - Delta")) + 
  scale_colour_manual("Legend",values = c("90 Strike - Delta"="blue","95 Strike - Delta"="red", "100 Strike - Delta"="green")) +
  labs(title = "Short Call Delta", x = "Date", y = "Delta")
print(plot_delta)











