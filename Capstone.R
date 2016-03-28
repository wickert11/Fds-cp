### Getting the Data
library(quantmod)
#Selecting Ticker Symbols
Symbols<-c("VOO","VEA","VWO","VGLT","GLD")
#Loading sybols using quantmod for date range (farthest back possible for selected securities)
loadSymbols(Symbols,from = '2010-09-09', to = '2016-02-23')
#Isolation of adjusted close prices in new data frame
PRICES <- data.frame(GLD$GLD.Adjusted,VEA$VEA.Adjusted,VGLT$VGLT.Adjusted,VOO$VOO.Adjusted,VWO$VWO.Adjusted)
#Creating monthly price data frame from daily price dataframe
PRICES.MONTH <-PRICES[endpoints(PRICES,on="months", k=1),]

#Calculating number of rows in dataframe
n <- nrow(PRICES.MONTH)

###Calculation of Returns for selected Securities
GLD.Return <-   ((PRICES.MONTH[2:n, 1] - PRICES.MONTH[1:(n-1), 1])/PRICES.MONTH[1:(n-1), 1])
VEA.Return <-   ((PRICES.MONTH[2:n, 2] - PRICES.MONTH[1:(n-1), 2])/PRICES.MONTH[1:(n-1), 2])
VGLT.Return <-  ((PRICES.MONTH[2:n, 3] - PRICES.MONTH[1:(n-1), 3])/PRICES.MONTH[1:(n-1), 3])
VOO.Return  <-  ((PRICES.MONTH[2:n, 4] - PRICES.MONTH[1:(n-1), 4])/PRICES.MONTH[1:(n-1), 4])
VWO.Return  <-  ((PRICES.MONTH[2:n, 5] - PRICES.MONTH[1:(n-1), 5])/PRICES.MONTH[1:(n-1), 5])

#Combine Returns into data frame
RETURN.monthly <- data.frame(GLD.Return,VEA.Return,VGLT.Return,VOO.Return,VWO.Return)

###Evaluating decision to Buy Security (monthly performance is > than cash (VGLT) buy and hold for next month)
#Security Buys are their monthly performance minus cash
GLD.Buys <- RETURN.monthly[2:n, 1]-RETURN.monthly[2:n, 3]
VEA.Buys <- RETURN.monthly[2:n, 2]-RETURN.monthly[2:n, 3]
VOO.Buys <- RETURN.monthly[2:n, 4]-RETURN.monthly[2:n, 3]
VWO.Buys <- RETURN.monthly[2:n, 5]-RETURN.monthly[2:n, 3]
#Apply decison to complete Buy Rule
#Replace all positive values with buy and negative or 0 with cash
GLD.Buys [GLD.Buys>0]<- "Buy"
GLD.Buys [GLD.Buys<=0]<- "Cash"
VEA.Buys [VEA.Buys>0]<- "Buy"
VEA.Buys [VEA.Buys<=0]<- "Cash"
VOO.Buys [VOO.Buys>0]<- "Buy"
VOO.Buys [VOO.Buys<=0]<- "Cash"
VWO.Buys [VWO.Buys>0]<- "Buy"
VWO.Buys [VWO.Buys<=0]<- "Cash"
#Combine all buy and sell decisions into new data frame
Buys.monthly <- data.frame(GLD.Buys,VEA.Buys,VOO.Buys,VWO.Buys)

###Getting ready to trade
# removing first months return because first month is not traded although it was used to determine Buys in second month
return.monthly2 <- RETURN.monthly[-1,]
# removing last month of buy decisions because there are no future prices 
Buys.monthly2<- na.omit(Buys.monthly)


library("dplyr")

###Trading
#Create new data frame that combines the adjusted trading decisions and the corresponding return
temp<- data.frame (Buys.monthly2,return.monthly2)
#convert the buy and sell decisions to numeric values (Checking that buy=1)
temp$GLD.Buys <- as.numeric(temp$GLD.Buys)
temp$VEA.Buys <- as.numeric(temp$VEA.Buys)
temp$VOO.Buys <- as.numeric(temp$VOO.Buys)
temp$VWO.Buys <- as.numeric(temp$VWO.Buys)
temp<- na.omit(temp)
#Replacing decisions to buy with the corresponding return for each asset
for(i in 1:64){
  if(temp$GLD.Buys[i]==1){
    temp$GLD.Buys[i] <- (temp$GLD.Return[i+1])
    }
}

for(i in 1:64){
  if(temp$VEA.Buys[i]==1){
    temp$VEA.Buys[i] <- (temp$VEA.Return[i+1])
  }
}

for(i in 1:64){
  if(temp$VOO.Buys[i]==1){
    temp$VOO.Buys[i] <- (temp$VOO.Return[i+1])
  }
}

for(i in 1:64){
  if(temp$VWO.Buys[i]==1){
    temp$VWO.Buys[i] <- (temp$VWO.Return[i+1])
  }
}

#Replacing the decision to hold the cash asset with the cash corresponding return
for(i in 1:64){
  if(temp$GLD.Buys[i]==2){
    temp$GLD.Buys[i] <- (temp$VGLT.Return[i+1])
  }
}

for(i in 1:64){
  if(temp$VEA.Buys[i]==2){
    temp$VEA.Buys[i] <- (temp$VGLT.Return[i+1])
  }
}

for(i in 1:64){
  if(temp$VOO.Buys[i]==2){
    temp$VOO.Buys[i] <- (temp$VGLT.Return[i+1])
  }
}

for(i in 1:64){
  if(temp$VWO.Buys[i]==2){
    temp$VWO.Buys[i] <- (temp$VGLT.Return[i+1])
  }
}

###Calculating Strategy performance
strategy.returns <- data.frame(temp$GLD.Buys,temp$VEA.Buys,temp$VOO.Buys,temp$VWO.Buys)
strategy.returns$Monthly <- rowMeans(strategy.returns[1:4])
strategy.returns$Value [1]<-10000

for(i in 2:63){
  strategy.returns$Value[i]=strategy.returns$Value[i-1]*strategy.returns$Monthly[i]+strategy.returns$Value[i-1]
}

Strategy.CAGR<- (strategy.returns$Value [63]/strategy.returns$Value[1])^(1/(63/12))-1


### Calculating Benchmark performanc

strategy.returns$Benchmark.Monthly <- rowMeans(return.monthly2[1:5])

strategy.returns$benchmark.Value [1]<-10000

for(i in 2:63){
  strategy.returns$benchmark.Value[i]=strategy.returns$benchmark.Value[i-1]*strategy.returns$Benchmark.Monthly[i]+strategy.returns$benchmark.Value[i-1]
}  

Benchmark.CAGR<- (strategy.returns$benchmark.Value [63]/strategy.returns$benchmark.Value [1])^(1/(63/12))-1


PRICES.MONTH2 <- PRICES.MONTH[-c(1,66),]

RESULTS<- data.frame(PRICES.MONTH2,strategy.returns$Value,strategy.returns$benchmark.Value)


Strategy.SD <- sd(na.omit(strategy.returns$Monthly)) 
Benchmark.SD <- sd(strategy.returns$Benchmark.Monthly)

Strategy.RV <-(mean(na.omit(strategy.returns$Monthly))/Strategy.SD)
Benchmark.RV <-(mean(strategy.returns$Benchmark.Monthly)/Benchmark.SD)

library("ggplot2")

RESULTS <- add_rownames(RESULTS,"Date")
RESULTS$Date <- as.Date.character (RESULTS$Date)

ggplot(RESULTS[1:63,], aes(Date))+
  geom_line(aes(y = strategy.returns.Value, colour= "Strategy" ),size = 1)+
  geom_line(aes(y = strategy.returns.benchmark.Value, colour= "Benchmark"),size = 1)+
  scale_colour_manual("",breaks= c("Strategy","Benchmark"),values = c("Strategy"= "green","Benchmark"="blue"))+
  labs(y="Value")+
  ggtitle("Strategy vs. Benchmark")  


ggplot(RESULTS, aes(Date))+
  geom_line(aes(y = GLD.Adjusted , colour= "GLD" ),size = 1)+
  geom_line(aes(y = VEA.Adjusted , colour= "VEA" ),size = 1)+
  geom_line(aes(y = VOO.Adjusted , colour= "VOO" ),size = 1)+
  geom_line(aes(y = VWO.Adjusted , colour= "VWO" ),size = 1)+
  geom_line(aes(y = VGLT.Adjusted , colour= "VGLT" ),size = 1)+
  scale_colour_manual("",breaks= c("VOO","GLD","VGLT","VEA","VWO","Benchmark"),
      values = c("GLD"= "yellow","VEA"= "red","VOO"= "blue","VWO"= "purple","VGLT"= "green","Benchmark"="black"))+
  labs(y="Value")+
  ggtitle("Asset Prices")  

RESULTS<- data.frame(RESULTS,RETURN.monthly[-1,],strategy.returns$Monthly,strategy.returns$Benchmark.Monthly)
RESULTS<-RESULTS[-64,]

ggplot(RESULTS, aes(Date))+
  geom_line(aes(y = GLD.Return , colour= "GLD" ),size = 1)+
  geom_line(aes(y = VEA.Return , colour= "VEA" ),size = 1)+
  geom_line(aes(y = VOO.Return , colour= "VOO" ),size = 1)+
  geom_line(aes(y = VWO.Return , colour= "VWO" ),size = 1)+
  geom_line(aes(y = VGLT.Return , colour= "VGLT" ),size = 1)+
  scale_colour_manual("",breaks= c("VOO","GLD","VGLT","VEA","VWO","Benchmark"),
      values = c("GLD"= "yellow","VEA"= "red","VOO"= "blue","VWO"= "purple","VGLT"= "green","Benchmark"="black"))+
  labs(y="Value")+
  ggtitle("Asset Percent Change")  


StandardizedReturn<- data.frame(RESULTS$Date,RESULTS$strategy.returns.Value,RESULTS$strategy.returns.benchmark.Value)
StandardizedReturn$GLD[1]<-10000
for(i in 2:63){
  StandardizedReturn$GLD[i]=StandardizedReturn$GLD[i-1]*RESULTS$GLD.Return[i]+StandardizedReturn$GLD[i-1]
}
StandardizedReturn$VEA[1]<-10000
for(i in 2:63){
  StandardizedReturn$VEA[i]=StandardizedReturn$VEA[i-1]*RESULTS$VEA.Return[i]+StandardizedReturn$VEA[i-1]
}

StandardizedReturn$VOO[1]<-10000
for(i in 2:63){
  StandardizedReturn$VOO[i]=StandardizedReturn$VOO[i-1]*RESULTS$VOO.Return[i]+StandardizedReturn$VOO[i-1]
}
StandardizedReturn$VGLT[1]<-10000
for(i in 2:63){
  StandardizedReturn$VGLT[i]=StandardizedReturn$VGLT[i-1]*RESULTS$VGLT.Return[i]+StandardizedReturn$VGLT[i-1]
}
StandardizedReturn$VWO[1]<-10000
for(i in 2:63){
  StandardizedReturn$VWO[i]=StandardizedReturn$VWO[i-1]*RESULTS$VWO.Return[i]+StandardizedReturn$VWO[i-1]
}

ggplot(StandardizedReturn, aes(RESULTS.Date))+
  geom_line(aes(y = GLD, colour= "GLD" ),size = 1)+
  geom_line(aes(y = VEA, colour= "VEA" ),size = 1)+
  geom_line(aes(y = VOO, colour= "VOO" ),size = 1)+
  geom_line(aes(y = VWO, colour= "VWO" ),size = 1)+
  geom_line(aes(y = VGLT, colour= "VGLT" ),size = 1)+
  geom_line(aes(y = RESULTS.strategy.returns.Value, colour= "Strategy" ),size = 1)+
  scale_colour_manual("",breaks= c("VOO","GLD","VGLT","VEA","VWO","Strategy"),
    values = c("GLD"= "yellow","VEA"= "red","VOO"= "blue","VWO"= "purple","VGLT"= "green","Strategy"="black"))+
  labs(y="Value")+
  labs(x="Date")+
  ggtitle("Strategy vs Assets")  


ggplot(StandardizedReturn, aes(RESULTS.Date))+
  geom_line(aes(y = GLD, colour= "GLD" ),size = 1)+
  geom_line(aes(y = VEA, colour= "VEA" ),size = 1)+
  geom_line(aes(y = VOO, colour= "VOO" ),size = 1)+
  geom_line(aes(y = VWO, colour= "VWO" ),size = 1)+
  geom_line(aes(y = VGLT, colour= "VGLT" ),size = 1)+
  geom_line(aes(y = RESULTS.strategy.returns.benchmark.Value, colour= "Benchmark" ),size = 1)+
  scale_colour_manual("",breaks= c("VOO","GLD","VGLT","VEA","VWO","Benchmark"),
                      values = c("GLD"= "yellow","VEA"= "red","VOO"= "blue","VWO"= "purple","VGLT"= "green","Benchmark"="black"))+
  labs(y="Value")+
  labs(x="Date")+
  ggtitle("Benchmark vs Assets")  




