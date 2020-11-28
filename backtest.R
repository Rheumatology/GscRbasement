library(quantmod)
library(TTR)
library(PerformanceAnalytics)

# Getting stock prices of SPY with RSI
# getSymbols("SPY",src = 'yahoo',from = '2019-01-01'); chartSeries(SPY, TA="addRSI();addLines(h=c(30,70), on=2)")
# getSymbols("SPY",src = 'yahoo',from = '2019-01-01'); chartSeries(SPY, TA="addMFI();addLines(h=c(20,80), on=2)")

# Getting stock prices of SPY
getSymbols('SPY', src = 'yahoo', from = '2018-01-01')

# Basic plot of the three stocks
barChart(SPY, theme = chartTheme('black'))

##################################################################################
##########  Creating Leading and Lagging Technical Indicators ###################
##################################################################################

# 1. Simple Moving Average (SMA)
sma20_SPY <- SMA(SPY$SPY.Close, n = 20)
sma50_SPY <- SMA(SPY$SPY.Close, n = 50)
lineChart(SPY, theme = chartTheme('black'))
addSMA(n = 20, col = 'blue')
addSMA(n = 50, col = 'orange')
legend('left', col = c('green','blue','orange'),
       legend = c('SPY','SMA20','SMA50'), lty = 1, bty = 'n',
       text.col = 'white', cex = 0.8)

# 2.  Parabolic Stop And Reverse (SAR)
sar_SPY <- SAR(cbind(Hi(SPY),Lo(SPY)), accel = c(0.02, 0.2))
barChart(SPY, theme = 'black')
addSAR(accel = c(0.02, 0.2), col ='lightblue')

# 3. Commodity Channel Index (CCI)
cci_SPY <- CCI(HLC(SPY), n = 20, c = 0.015)
head(cci_SPY)
barChart(SPY, theme = 'black')
addCCI(n = 20, c = 0.015)

# 4. Rate of Change (ROC)
roc_SPY <- ROC(SPY$SPY.Close, n = 25)
head(roc_SPY)
barChart(SPY, theme = 'black')
addROC(n = 25)
legend('left', col = 'red', legend = 'ROC(25)', lty = 1, bty = 'n',text.col = 'white', cex = 0.8)

# 5. Stochastic Momentum Index (SMI)
smi_SPY <- SMI(HLC(SPY),n = 13, nFast = 2, nSlow = 25, nSig = 9)
barChart(SPY, theme = 'black')
addSMI(n = 13, fast = 2, slow = 2, signal = 9)

# 6. Williams %R
wpr_SPY <- WPR(HLC(SPY), n = 14)
colnames(wpr_SPY) <- 'wpr'
barChart(SPY, theme = 'black')
addWPR(n = 14)

# 7. RSI
rsi_SPY <- RSI(Cl(SPY), n = 12)
colnames(rsi_SPY) <- 'rsi'
barChart(SPY, theme = 'black')
addRSI(n=12)

# 8. MFI
mfi_SPY <- MFI(HLC(SPY), Vo(SPY))
colnames(mfi_SPY) <- 'mfi'
barChart(SPY, theme = 'black')
addMFI(n=12)
addBBands(n=20)
addZigZag(change=12)

##################################################################################
############# Creating Trading signal with Indicators ########################
##################################################################################

# SMA 20 Crossover Signal 
sma20_SPY_ts <- Lag(
  ifelse(Lag(Cl(SPY)) < Lag(sma20_SPY) & Cl(SPY) > sma20_SPY,1,
         ifelse(Lag(Cl(SPY)) > Lag(sma20_SPY) & Cl(SPY) < sma20_SPY,-1,0)))
sma20_SPY_ts[is.na(sma20_SPY_ts)] <- 0

# SMA 50 Crossover Signal
sma50_SPY_ts <- Lag(
  ifelse(Lag(Cl(SPY)) < Lag(sma50_SPY) & Cl(SPY) > sma50_SPY,1,
         ifelse(Lag(Cl(SPY)) > Lag(sma50_SPY) & Cl(SPY) < sma50_SPY,-1,0)))
sma50_SPY_ts[is.na(sma50_SPY_ts)] <- 0

# SMA 20 and SMA 50 Crossover Signal
sma_SPY_ts <- Lag(
  ifelse(Lag(sma20_SPY) < Lag(sma50_SPY) & sma20_SPY > sma50_SPY,1,
         ifelse(Lag(sma20_SPY) > Lag(sma50_SPY) & sma20_SPY < sma50_SPY,-1,0)))
sma_SPY_ts[is.na(sma_SPY_ts)] <- 0


# 2. Parabolic Stop And Reverse (SAR) 
sar_SPY_ts <- Lag(
  ifelse(Lag(Cl(SPY)) < Lag(sar_SPY) & Cl(SPY) > sar_SPY,1,
         ifelse(Lag(Cl(SPY)) > Lag(sar_SPY) & Cl(SPY) < sar_SPY,-1,0)))
sar_SPY_ts[is.na(sar_SPY_ts)] <- 0

# 3. Commodity Channel Index  (CCI)
cci_SPY_ts <- Lag(
  ifelse(Lag(cci_SPY) < (-100) & cci_SPY > (-100),1,
         ifelse(Lag(cci_SPY) < (100) & cci_SPY > (100),-1,0)))
cci_SPY_ts[is.na(cci_SPY_ts)] <- 0

# 4. Rate of Change (ROC)
roc_SPY_ts <- Lag(
  ifelse(Lag(roc_SPY) < (-0.05) & roc_SPY > (-0.05),1,
         ifelse(Lag(roc_SPY) < (0.05) & roc_SPY > (0.05),-1,0)))
roc_SPY_ts[is.na(roc_SPY_ts)] <- 0


# 5. Stochastic Momentum Index (SMI)
smi_SPY_ts <- Lag(
  ifelse(Lag(smi_SPY[,1]) < Lag(smi_SPY[,2]) & smi_SPY[,1] > smi_SPY[,2],1, 
         ifelse(Lag(smi_SPY[,1]) > Lag(smi_SPY[,2]) & smi_SPY[,1] < smi_SPY[,2],-1,0)))
smi_SPY_ts[is.na(smi_SPY_ts)] <- 0

# 6. williams %R
wpr_SPY_ts <- Lag(
  ifelse(Lag(wpr_SPY) > 0.8 & wpr_SPY < 0.8,1,
         ifelse(Lag(wpr_SPY) > 0.2 & wpr_SPY < 0.2,-1,0)))
wpr_SPY_ts[is.na(wpr_SPY_ts)] <- 0


# 7. Relative Strength Index(RSI)
rsi_SPY_ts <- Lag(
  ifelse(Lag(rsi_SPY) < 30 & rsi_SPY > 30,1, 
         ifelse(Lag(rsi_SPY) > 72 & rsi_SPY < 72,-1,0)))
rsi_SPY_ts[is.na(rsi_SPY_ts)] <- 0

# 8. Money Flow Index(MFI)
mfi_SPY_ts <- Lag(
  ifelse(Lag(mfi_SPY) < 28 & mfi_SPY > 28,1, 
         ifelse(Lag(mfi_SPY) > 80 & mfi_SPY < 80,-1,0)))
mfi_SPY_ts[is.na(mfi_SPY_ts)] <- 0

##################################################################################
############# Creating Trading Strategies using Signals ########################
##################################################################################

# 1. SMA 20 and SMA 50 Crossover Strategy
sma_SPY_strat <- ifelse(sma_SPY_ts > 1,0,1)
for (i in 1 : length(Cl(SPY))) {
  sma_SPY_strat[i] <- ifelse(sma_SPY_ts[i] == 1,1,ifelse(sma_SPY_ts[i] == -1,0,sma_SPY_strat[i-1]))
}
sma_SPY_strat[is.na(sma_SPY_strat)] <- 1
sma_SPY_stratcomp <- cbind(sma20_SPY, sma50_SPY, sma_SPY_ts, sma_SPY_strat)
colnames(sma_SPY_stratcomp) <- c('SMA(20)','SMA(50)','SMA SIGNAL','SMA POSITION')
head(sma_SPY_stratcomp)

# 2.Parabolic SAR Strategy 
sar_SPY_strat <- ifelse(sar_SPY_ts > 1,0,1)
for (i in 1 : length(Cl(SPY))) {
  sar_SPY_strat[i] <- ifelse(sar_SPY_ts[i] == 1,1,ifelse(sar_SPY_ts[i] == -1,0,sar_SPY_strat[i-1]))
}
sar_SPY_strat[is.na(sar_SPY_strat)] <- 1
sar_SPY_stratcomp <- cbind(Cl(SPY), sar_SPY, sar_SPY_ts, sar_SPY_strat)
colnames(sar_SPY_stratcomp) <- c('Close','SAR','SAR SIGNAL','SAR POSITION')

# 3.CCI
cci_SPY_strat <- ifelse(cci_SPY_ts > 1,0,1)
for (i in 1 : length(Cl(SPY))) {
  cci_SPY_strat[i] <- ifelse(cci_SPY_ts[i] == 1,1,ifelse(cci_SPY_ts[i] == -1,0,cci_SPY_strat[i-1]))
}
cci_SPY_strat[is.na(cci_SPY_strat)] <- 1
cci_SPY_stratcomp <- cbind(cci_SPY, cci_SPY_ts, cci_SPY_strat)
colnames(cci_SPY_stratcomp) <- c('CCI','CCI SIGNAL','CCI POSITION')

# 4.ROC
roc_SPY_strat <- ifelse(roc_SPY_ts > 1,0,1)
for (i in 1 : length(Cl(SPY))) {
  roc_SPY_strat[i] <- ifelse(roc_SPY_ts[i] == 1,1,ifelse(roc_SPY_ts[i] == -1,0,roc_SPY_strat[i-1]))
}
roc_SPY_strat[is.na(roc_SPY_strat)] <- 1
roc_SPY_stratcomp <- cbind(roc_SPY, roc_SPY_ts, roc_SPY_strat)
colnames(roc_SPY_stratcomp) <- c('ROC(25)','ROC SIGNAL','ROC POSITION')

# 5.SMI
smi_SPY_strat <- ifelse(smi_SPY_ts > 1,0,1)
for (i in 1 : length(Cl(SPY))) {
  smi_SPY_strat[i] <- ifelse(smi_SPY_ts[i] == 1,1,ifelse(smi_SPY_ts[i] == -1,0,smi_SPY_strat[i-1]))
}
smi_SPY_strat[is.na(smi_SPY_strat)] <- 1
smi_SPY_stratcomp <- cbind(smi_SPY[,1],smi_SPY[,2],smi_SPY_ts,smi_SPY_strat)
colnames(smi_SPY_stratcomp) <- c('SMI','SMI(S)','SMI SIGNAL','SMI POSITION')

# 6.WPR
wpr_SPY_strat <- ifelse(wpr_SPY_ts > 1,0,1)
for (i in 1 : length(Cl(SPY))) {
  wpr_SPY_strat[i] <- ifelse(wpr_SPY_ts[i] == 1,1,ifelse(wpr_SPY_ts[i] == -1,0,wpr_SPY_strat[i-1]))
}
wpr_SPY_strat[is.na(wpr_SPY_strat)] <- 1
wpr_SPY_stratcomp <- cbind(wpr_SPY, wpr_SPY_ts, wpr_SPY_strat)
colnames(wpr_SPY_stratcomp) <- c('WPR(14)','WPR SIGNAL','WPR POSITION')

# 7.RSI
rsi_SPY_strat <- ifelse(rsi_SPY_ts > 1,0,1)
for (i in 1 : length(Cl(SPY))) {
  rsi_SPY_strat[i] <- ifelse(rsi_SPY_ts[i] == 1,1,ifelse(rsi_SPY_ts[i] == -1,0,rsi_SPY_strat[i-1]))
}
rsi_SPY_strat[is.na(rsi_SPY_strat)] <- 1
rsi_SPY_stratcomp <- cbind(rsi_SPY,rsi_SPY_ts, rsi_SPY_strat)
colnames(rsi_SPY_stratcomp) <- c('RSI','RSI SIGNAL','RSI POSITION')

# 8.MFI
mfi_SPY_strat <- ifelse(mfi_SPY_ts > 1,0,1)
for (i in 1 : length(Cl(SPY))) {
  mfi_SPY_strat[i] <- ifelse(mfi_SPY_ts[i] == 1,1,ifelse(mfi_SPY_ts[i] == -1,0,mfi_SPY_strat[i-1]))
}
mfi_SPY_strat[is.na(mfi_SPY_strat)] <- 1
mfi_SPY_stratcomp <- cbind(mfi_SPY,mfi_SPY_ts, mfi_SPY_strat)
colnames(mfi_SPY_stratcomp) <- c('MFI','MFI SIGNAL','MFI POSITION')

##################################################################################
################ Trading Strategy Performance  ################################
##################################################################################

# Calculating Returns & setting Benchmark for comapnies
ret_SPY <- diff(log(Cl(SPY)))
benchmark_SPY <- ret_SPY

# SMA 
sma_SPY_ret <- ret_SPY*sma_SPY_strat
sma_SPY_ret_commission_adj <- ifelse((sma_SPY_ts == 1|sma_SPY_ts == -1) & sma_SPY_strat != Lag(sma_SPY_ts), (ret_SPY-0.05)*sma_SPY_strat, ret_SPY*sma_SPY_strat)
sma_SPY_comp <- cbind(sma_SPY_ret, sma_SPY_ret_commission_adj, benchmark_SPY)
colnames(sma_SPY_comp) <- c('SMA','SMA Commission Adj','Benchmark')
charts.PerformanceSummary(sma_SPY_comp, main = 'SMA Performance')
sma_SPY_comp_table <- table.AnnualizedReturns(sma_SPY_comp)

# Parabolic SAR 
sar_SPY_ret <- ret_SPY*sar_SPY_strat
sar_SPY_ret_commission_adj <- ifelse((sar_SPY_ts == 1|sar_SPY_ts == -1) & sar_SPY_strat != Lag(sar_SPY_ts), (ret_SPY-0.05)*sar_SPY_strat, ret_SPY*sar_SPY_strat)
sar_SPY_comp <- cbind(sar_SPY_ret, sar_SPY_ret_commission_adj, benchmark_SPY)
colnames(sar_SPY_comp) <- c('SAR','SAR Commission Adj','Benchmark')
charts.PerformanceSummary(sar_SPY_comp, main = 'Parabolic SAR Performance')
sar_SPY_comp_table <- table.AnnualizedReturns(sar_SPY_comp)

# CCI  
cci_SPY_ret <- ret_SPY*cci_SPY_strat
cci_SPY_ret_commission_adj <- ifelse((cci_SPY_ts == 1|cci_SPY_ts == -1) & cci_SPY_strat != Lag(cci_SPY_ts), (ret_SPY-0.05)*cci_SPY_strat, ret_SPY*cci_SPY_strat)
cci_SPY_comp <- cbind(cci_SPY_ret, cci_SPY_ret_commission_adj, benchmark_SPY)
colnames(cci_SPY_comp) <- c('CCI','CCI Commission Adj','Benchmark')
charts.PerformanceSummary(cci_SPY_comp, main = 'CCI Performance')
cci_SPY_comp_table <- table.AnnualizedReturns(cci_SPY_comp)

# ROC  
roc_SPY_ret <- ret_SPY*roc_SPY_strat
roc_SPY_ret_commission_adj <- ifelse((roc_SPY_ts == 1|roc_SPY_ts == -1) & roc_SPY_strat != Lag(roc_SPY_ts), (ret_SPY-0.05)*roc_SPY_strat, ret_SPY*roc_SPY_strat)
roc_SPY_comp <- cbind(roc_SPY_ret, roc_SPY_ret_commission_adj, benchmark_SPY)
colnames(roc_SPY_comp) <- c('ROC','ROC Commission Adj','Benchmark')
charts.PerformanceSummary(roc_SPY_comp, main = 'ROC Performance')
roc_SPY_comp_table <- table.AnnualizedReturns(roc_SPY_comp)

# SMI  
smi_SPY_ret <- ret_SPY*smi_SPY_strat
smi_SPY_ret_commission_adj <- ifelse((smi_SPY_ts == 1|smi_SPY_ts == -1) & smi_SPY_strat != Lag(smi_SPY_ts), (ret_SPY-0.05)*smi_SPY_strat, ret_SPY*smi_SPY_strat)
smi_SPY_comp <- cbind(smi_SPY_ret, smi_SPY_ret_commission_adj, benchmark_SPY)
colnames(smi_SPY_comp) <- c('SMI','SMI Commission Adj','Benchmark')
charts.PerformanceSummary(smi_SPY_comp, main = 'SMI Performance')
smi_SPY_comp_table <- table.AnnualizedReturns(smi_SPY_comp)

# WPR  
wpr_SPY_ret <- ret_SPY*wpr_SPY_strat
wpr_SPY_ret_commission_adj <- ifelse((wpr_SPY_ts == 1|wpr_SPY_ts == -1) & wpr_SPY_strat != Lag(wpr_SPY_ts), (ret_SPY-0.05)*wpr_SPY_strat, ret_SPY*wpr_SPY_strat)
wpr_SPY_comp <- cbind(wpr_SPY_ret, wpr_SPY_ret_commission_adj, benchmark_SPY)
colnames(wpr_SPY_comp) <- c('WPR','WPR Commission Adj','Benchmark')
charts.PerformanceSummary(wpr_SPY_comp, main = 'WPR Performance')
wpr_SPY_comp_table <- table.AnnualizedReturns(wpr_SPY_comp)

#RSI
rsi_SPY_ret <- ret_SPY*rsi_SPY_strat
rsi_SPY_ret_commission_adj <- ifelse((rsi_SPY_ts == 1|rsi_SPY_ts == -1) & rsi_SPY_strat != Lag(rsi_SPY_ts), (ret_SPY-0.05)*rsi_SPY_strat, ret_SPY*rsi_SPY_strat)
rsi_SPY_comp <- cbind(rsi_SPY_ret, rsi_SPY_ret_commission_adj, benchmark_SPY)
colnames(rsi_SPY_comp) <- c('RSI','RSI Commission Adj','Benchmark')
charts.PerformanceSummary(rsi_SPY_comp, main = 'RSI Performance')
rsi_SPY_comp_table <- table.AnnualizedReturns(rsi_SPY_comp)

#MFI
mfi_SPY_ret <- ret_SPY*mfi_SPY_strat
mfi_SPY_ret_commission_adj <- ifelse((mfi_SPY_ts == 1|mfi_SPY_ts == -1) & mfi_SPY_strat != Lag(mfi_SPY_ts), (ret_SPY-0.05)*mfi_SPY_strat, ret_SPY*mfi_SPY_strat)
mfi_SPY_comp <- cbind(mfi_SPY_ret, mfi_SPY_ret_commission_adj, benchmark_SPY)
colnames(mfi_SPY_comp) <- c('MFI','MFI Commission Adj','Benchmark')
charts.PerformanceSummary(mfi_SPY_comp, main = 'MFI Performance')
mfi_SPY_comp_table <- table.AnnualizedReturns(mfi_SPY_comp,geometric = FALSE)
mfi_SPY_comp_table

? table.AnnualizedReturns
##################################################################################
################ Compare Trading Strategy Performance  ###########################
##################################################################################

rename<-function(x){
  colnames(x)<-c("Strategy","Adj","Benchmark")
  return(x)
}

sma<-rename(sma_SPY_comp_table)
sar<-rename(sar_SPY_comp_table)
cci<-rename(cci_SPY_comp_table)
roc<-rename(roc_SPY_comp_table)
smi<-rename(smi_SPY_comp_table)
wpr<-rename(wpr_SPY_comp_table)
rsi<-rename(rsi_SPY_comp_table)
mfi<-rename(mfi_SPY_comp_table)

rlt<-rbind(sma,sar,cci,roc,smi,wpr,rsi,mfi)
rlt<-rlt[seq(1,nrow(rlt),by=3),]
rownames(rlt)<-c("SMA","SAR","CCI","ROC","SMI","WPR","RSI","MFI")
rlt        

