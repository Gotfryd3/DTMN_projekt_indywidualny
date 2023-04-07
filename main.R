#####
##  Zadanie 1
#
library(MASS)
library(moments)
library(dplyr)
library(plotrix)
library(car)
library(ggpubr)
library(cowplot)

rawData <- data.frame(Cars93)

USmileToKm <- 1.6
USgalonToL <- 3.8
funtToKg <- 0.4536
USdolarToPLN <- 3.35

rawData["Fuel_consumption_city"] <- 100 / (rawData["MPG.city"] * USmileToKm) * USgalonToL
rawData["Fuel_consumption_highway"] <- 100 / (rawData["MPG.highway"] * USmileToKm) * USgalonToL
rawData["Weight_Kg"] <- rawData["Weight"] * funtToKg
rawData["Min_Price_PLN"] <- rawData["Min.Price"] * USdolarToPLN

basicStatistics <- list()
basicStatistics[1] <- range(rawData$Min_Price_PLN)
basicStatistics[2] <- mean(rawData$Min_Price_PLN)
basicStatistics[3] <- median(rawData$Min_Price_PLN)
basicStatistics[4] <- min(rawData$Min_Price_PLN)
basicStatistics[5] <- max(rawData$Min_Price_PLN)
basicStatistics[6] <- sd(rawData$Min_Price_PLN)
basicStatistics[7] <- kurtosis(rawData$Min_Price_PLN)
basicStatistics[8] <- skewness(rawData$Min_Price_PLN)
basicStatistics[9] <- quantile(rawData$Min_Price_PLN, 0.25)
basicStatistics[10] <- quantile(rawData$Min_Price_PLN, 0.75)
basicStatistics[11] <- quantile(rawData$Min_Price_PLN, 0.95)
basicStatistics <- data.frame(basicStatistics)
colnames(basicStatistics) <- c("range", "mean", "median", "min", "max", "sd", "kurtosis", "skewness", "Q_1st", "Q_3rd", "Q_0.95")

veryHighPricedCars <- filter(rawData, rawData$Min_Price_PLN > basicStatistics$Q_0.95)
veryHighPricedCars <- veryHighPricedCars[c("Model", "Min_Price_PLN")]

countedByType <- rawData %>% 
                group_by( Type ) %>% 
                summarise( Count_by_type = n(), Percentage_of_type = round( n() / nrow(.) * 100, 1) ) %>% 
                arrange( desc( Count_by_type ) )
lbls <- paste(countedByType$Type, countedByType$Count_by_type)
barplot(table(rawData$Type), xlab="Type of car", ylab="Counted by type")
pie3D(table(rawData$Type), labels = lbls, explode = 0.1, main="Cars counted by type")

USCars <- filter(rawData, rawData$Origin == "USA")
nonUSCars <- filter(rawData, rawData$Origin != "USA")
boxplot(USCars$Fuel_consumption_city, nonUSCars$Fuel_consumption_city, xlab="Origin", ylab="Fuel consumption in city", names = c("US Cars", "non US Cars"))

correlationPrice_FCC <- cor(rawData[c("Min_Price_PLN", "Fuel_consumption_city")])
correlationFCH_FCC <- cor(rawData[c("Fuel_consumption_highway", "Fuel_consumption_city")])
sp1 <- ggscatter(rawData, x="Min_Price_PLN", y="Fuel_consumption_city", add = "reg.line", conf.int=TRUE) + annotate("text", x=100, y=7, label=paste("Correlation = ", as.character(correlationPrice_FCC[2])))
sp2 <- ggscatter(rawData, x="Fuel_consumption_highway", y="Fuel_consumption_city", add = "reg.line", conf.int=TRUE) + annotate("text", x=10, y=7, label=paste("Correlation = ", as.character(correlationFCH_FCC[2])))
plot_grid(sp1, sp2, labels = c("Min_Price_PLN ~ Fuel_consumption_city", "Fuel_consumption_highway ~ Fuel_consumption_city"), ncol = 1, nrow = 2)

hist(rawData$Weight_Kg,  main="Histogram of car weights", xlab="Weight [Kg]", ylab="Count")

#####
##  Zadanie 2
#
rawData2 <- read.table("airpollution.txt", sep='\t', header=TRUE)

summary(rawData2)
correlationMatrix <- cor(rawData2)
linearModel1 <- lm(Mortality ~ NOx, data = rawData2)
summary(linearModel1)

rawData2["log(NOx)"] <- log(rawData2$NOx)
linearModel2 <- lm(Mortality ~ log(NOx), data = rawData2)
summary(linearModel2)

Q <- quantile(linearModel2$residuals, probs=c(0.25, 0.75), na.rm = FALSE)
iqr <- IQR(linearModel2$residuals)
cleanedData <- subset(rawData2, linearModel2$residuals>(Q[1] - 1.5*iqr) & linearModel2$residuals<(Q[2] + 1.5*iqr))
linearModel3 <- lm(Mortality ~ log(NOx), data = cleanedData)
summary(linearModel3)

#####
##  Zadanie 3
#
rawData3 <- read.table("savings.txt", sep=';', header=TRUE)
summary(rawData3)
corelationMatrix2 <- cor(rawData3[-1])
linearModel4 <- lm(Savings ~ dpi + ddpi + Pop15 + Pop75, data=rawData3)

maxValResidual <- max(linearModel4$residuals)
minValResidual <- min(linearModel4$residuals)
countryMaxValResidual <- rawData3$Country[which(linearModel4$residuals == maxValResidual)[1]]
countryMinValResidual <- rawData3$Country[which(linearModel4$residuals == minValResidual)[1]]
plot(linearModel4$residuals)

leverageDataFrame <- as.data.frame(hatvalues(linearModel4))
plot(leverageDataFrame)

studentizedResiduals <- studres(linearModel4)
plot(rawData3$Savings, studentizedResiduals)

dffitsLinearModel4 <- dffits(linearModel4)
dfbetasLinearModel4 <- dfbetas(linearModel4)
cookeDistanceLinearModel4 <- cooks.distance(linearModel4)
influenceLinearModel4 <- influence.measures(linearModel4)
summary(influenceLinearModel4)

maxValCookDistance <- max(cookeDistanceLinearModel4)
clearedRawData3 <- subset(rawData3, cookeDistanceLinearModel4 < maxValCookDistance)
modifiedLinearModel4 <- lm(Savings ~ dpi + ddpi + Pop15 + Pop75, data=clearedRawData3)
summary(modifiedLinearModel4)

differenceInCoeficients <- (linearModel4$coefficients - modifiedLinearModel4$coefficients) / linearModel4$coefficients
barplot(differenceInCoeficients, main="Difference in model coeficients", xlab="Coeficient name", ylab="Relative difference [%]", horiz=FALSE, names.arg=c("Beta0", "Pop15", "Pop75", "dpi", "ddpi"))

#####
##  Zadanie 4
#
rawData4 <- read.table("realest.txt", sep=';', header=TRUE)

linearModel5 <- lm(Price ~ Bedroom + Space + Room + Lot + Tax + Bathroom + Garage + Condition, rawData4)
modifiedData4 <- rawData4
modifiedData4["Bedroom"] <- modifiedData4["Bedroom"] + 1
influenceOnPrice <- (fitted(linearModel5) - predict(linearModel5, modifiedData4[-1])) / fitted(linearModel5)
simplifiedLinearModel5 <- lm(Price ~ Bedroom, rawData4)
influenceOnPrice <- (fitted(simplifiedLinearModel5) - predict(simplifiedLinearModel5, modifiedData4[-1])) / fitted(simplifiedLinearModel5)

price <- predict(linearModel5, data.frame(Bedroom=3, Space=1500, Room=8, Lot=40, Bathroom=5, Garage=1, Tax=1000, Condition=0), interval="confidence", level=0.95)
print(price)

#####
##  Zadanie 5
#
rawData5 <- read.table("gala_data.txt", header=TRUE)




