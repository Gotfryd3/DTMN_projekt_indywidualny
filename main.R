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
library(caTools)
library(tree)

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
correlationMatrix2 <- cor(rawData2)
linearModel2A <- lm(Mortality ~ NOx, data = rawData2)
summary(linearModel2A)

rawData2["log(NOx)"] <- log(rawData2$NOx)
linearModel2B <- lm(Mortality ~ log(NOx), data = rawData2)
summary(linearModel2B)

Q <- quantile(linearModel2B$residuals, probs=c(0.25, 0.75), na.rm = FALSE)
iqr <- IQR(linearModel2B$residuals)
cleanedData <- subset(rawData2, linearModel2B$residuals>(Q[1] - 1.5*iqr) & linearModel2B$residuals<(Q[2] + 1.5*iqr))
linearModel2C <- lm(Mortality ~ log(NOx), data = cleanedData)
summary(linearModel2C)

#####
##  Zadanie 3
#
rawData3 <- read.table("savings.txt", sep=';', header=TRUE)
summary(rawData3)
corelationMatrix3 <- cor(rawData3[-1])
linearModel3 <- lm(Savings ~ dpi + ddpi + Pop15 + Pop75, data=rawData3)

maxValResidual <- max(linearModel3$residuals)
minValResidual <- min(linearModel3$residuals)
countryMaxValResidual <- rawData3$Country[which(linearModel3$residuals == maxValResidual)[1]]
countryMinValResidual <- rawData3$Country[which(linearModel3$residuals == minValResidual)[1]]
plot(linearModel3$residuals)

leverageDataFrame <- as.data.frame(hatvalues(linearModel3))
plot(leverageDataFrame)

studentizedResiduals <- studres(linearModel3)
plot(rawData3$Savings, studentizedResiduals)

dffitsLinearModel3 <- dffits(linearModel3)
dfbetasLinearModel3 <- dfbetas(linearModel3)
cookeDistanceLinearModel3 <- cooks.distance(linearModel3)
influenceLinearModel4 <- influence.measures(linearModel3)
summary(influenceLinearModel4)

maxValCookDistance <- max(cookeDistanceLinearModel3)
clearedRawData3 <- subset(rawData3, cookeDistanceLinearModel4 < maxValCookDistance)
modifiedLinearModel3 <- lm(Savings ~ dpi + ddpi + Pop15 + Pop75, data=clearedRawData3)
summary(modifiedLinearModel3)

differenceInCoeficients <- (linearModel3$coefficients - modifiedLinearModel3$coefficients) / linearModel3$coefficients
barplot(differenceInCoeficients, main="Difference in model coeficients", xlab="Coeficient name", ylab="Relative difference [%]", horiz=FALSE, names.arg=c("Beta0", "Pop15", "Pop75", "dpi", "ddpi"))

#####
##  Zadanie 4
#
rawData4 <- read.table("realest.txt", sep=';', header=TRUE)

linearModel4 <- lm(Price ~ Bedroom + Space + Room + Lot + Tax + Bathroom + Garage + Condition, rawData4)
modifiedData4 <- rawData4
modifiedData4["Bedroom"] <- modifiedData4["Bedroom"] + 1
influenceOnPrice <- (fitted(linearModel4) - predict(linearModel4, modifiedData4[-1])) / fitted(linearModel4)
simplifiedLinearModel4 <- lm(Price ~ Bedroom, rawData4)
influenceOnPrice <- (fitted(simplifiedLinearModel4) - predict(simplifiedLinearModel4, modifiedData4[-1])) / fitted(simplifiedLinearModel4)

price <- predict(linearModel4, data.frame(Bedroom=3, Space=1500, Room=8, Lot=40, Bathroom=5, Garage=1, Tax=1000, Condition=0), interval="confidence", level=0.95)
print(price)

#####
##  Zadanie 5
#
rawData5 <- read.table("gala_data.txt", header=TRUE)

linearModel5A <- lm(Species ~ Endemics + Area + Elevation + Nearest + Scruz + Adjacent, rawData5)
plot(linearModel5A)
qqnorm(linearModel5A$res)
qqline(linearModel5A$res)
cutoff <- 4/((nrow(linearModel5A)-length(linearModel5A$coefficients)-2))
plot(linearModel5A, which=4, cook.levels=cutoff)
influencePlot(linearModel5A, main="Influence Plot", sub="Circle size is proportial to Cook's Distance")

modifiedData5A <- rawData5
modifiedData5A["sSpecies"] <- sqrt(rawData5$Species)
modifiedData5A <- modifiedData5A[-2]
linearModel5B <- lm(sSpecies ~ Endemics + Area + Elevation + Nearest + Scruz + Adjacent, modifiedData5A)
pVal <- summary(linearModel5B)$coefficients[,4]
modifiedData5B <- modifiedData5A[-which(max(pVal) == pVal)]
linearModel5C <- lm(sSpecies ~ Endemics + Area + Nearest + Scruz + Adjacent, modifiedData5B)
summary(linearModel5A)$r.squared
summary(linearModel5C)$r.squared
summary(linearModel5A)$adj.r.squared
summary(linearModel5C)$adj.r.squared

#####
##  Zadanie 6
#
rawData6 <- read.table("iris.txt", sep=",", header=TRUE)
rawData6$class <- as.factor(rawData6$class)

class1Data <- data.frame(filter(rawData6, class == "Iris-setosa"))
class2Data <- data.frame(filter(rawData6, class == "Iris-versicolor"))
class3Data <- data.frame(filter(rawData6, class == "Iris-virginica"))

split1 = sample.split(class1Data, SplitRatio = 0.7)
trainData1 = subset(class1Data, split1 == TRUE)
testData1  = subset(class1Data, split1 == FALSE)
split2 = sample.split(class2Data, SplitRatio = 0.7)
trainData2 = subset(class2Data, split2 == TRUE)
testData2  = subset(class2Data, split2 == FALSE)
split3 = sample.split(class3Data, SplitRatio = 0.7)
trainData3 = subset(class3Data, split3 == TRUE)
testData3  = subset(class3Data, split3 == FALSE)

trainData <- rbind(trainData1, trainData2, trainData3)
testData <- rbind(testData1, testData2, testData3)

model <- tree(class ~ sepal.length + sepal.width + petal.length + petal.width, trainData, na.action=na.pass, control=tree.control(90), split=c("gini"))
tablica <- table(predict(model, testData, type="class"), testData$class)
badClassification <- sum(tablica)-sum(diag(tablica))
print(paste(badClassification, "/", count(testData), "have been classified badly!"))
# X11()
plot(model)
text(model,use.n=TRUE,all=FALSE,cex=0.7)
print(tablica)

#####
##  Zadanie 7
#
rawData7 <- read.table("iris.txt", sep=",", header=TRUE)
rawData7$class <- as.factor(rawData7$class)
