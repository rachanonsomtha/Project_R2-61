forest_fire <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/f
orest-fires/forestfires.csv")

#Prepearing data

n_row = nrow(forest_fire)

forest_fire

forest_area <- forest_fire$area
#Forest freq with area
hist(forest_area ,xlab = "Forest area", main ="Forest frequency with area", col="gray")
#Forest freq with Temperature
hist(forest_fire$temp ,xlab = "Forest Temperature", main ="Forest frequency with Temperature", col="gray")


#observation of a forest fire.
#True =>  observed&&forest fires are triggered
#False  =>  observed&&forest fires aren't triggered
round(table(forest_area==0)/(n_row),3)

summary(forest_fire)
summary(forest_fire$month)

par(mfrow=c(3,3))

forest_fire <- forest_fire[forest_fire$area>0,]
forest_fire


plot(log(area)~as.factor(X), data = forest_fire, xlab = "X", ylab = "fire area",
        main = "forest fire area for different X's")

plot(log(area)~as.factor(Y), data= forest_fire, xlab = "Y", ylab = "fire area",
        main = "forest fire area for different Y's")

plot(log(area)~forest_fire$month, data = forest_fire, xlab = "month's", ylab = "fire area", 
        main = "forest fire area for different month's")

plot(log(area)~forest_fire$day, data = forest_fire, xlab = "day's", ylab = "fire area", 
        main = "forest fire area for different day's")

plot(log(area)~forest_fire$FFMC, data = forest_fire, xlab = "FFMC's", ylab = "fire area", 
        main = "forest fire area for different FFMC's")

plot(log(area)~forest_fire$DMC, data = forest_fire, xlab = "DMC's", ylab = "fire area", 
        main = "forest fire area for different DMC's")

plot(log(area)~forest_fire$DC, data = forest_fire, xlab = "DC's", ylab = "fire area", 
        main = "forest fire area for different DC's")

plot(log(area)~forest_fire$ISI, data = forest_fire, xlab = "ISI's", ylab = "fire area", 
        main = "forest fire area for different ISI's")

plot(log(area)~forest_fire$temp, data = forest_fire, xlab = "temp's", ylab = "fire area", 
        main = "forest fire area for different temp's")

plot(log(area)~forest_fire$RH, data = forest_fire, xlab = "RH's", ylab = "fire area", 
        main = "forest fire area for different RH's")

plot(log(area)~forest_fire$wind, data = forest_fire, xlab = "Wind's", ylab = "fire area", 
        main = "forest fire area for different Wind's")

plot(log(area)~forest_fire$rain, data = forest_fire, xlab = "Rain's", ylab = "fire area", 
        main = "forest fire area for different Rain's")

plot(log(area)~forest_fire$rain, data = forest_fire, xlab = "Rain's", ylab = "fire area", 
        main = "forest fire area for different Rain's")

n_row = nrow(forest_fire)
summary(forest_fire$area)

#Seasons
forest_fire$season <- rep("spring", n_row)
for (i in 1:n_row){
  if (forest_fire$month[i] %in% c("feb","jan","dec")) forest_fire$season[i] <- "winter"
  if (forest_fire$month[i] %in% c("oct","nov","sep")) forest_fire$season[i] <- "autumn"
  if (forest_fire$month[i] %in% c("aug","jul","jun")) forest_fire$season[i] <- "summer"
}
forest_fire$season <- as.factor(forest_fire$season)
boxplot(log(area)~season, data = forest_fire, xlab = "season", ylab = "fire area",
        main = "forest fire area for different seasons")

boxplot(log(area)~month, data = forest_fire, xlab = "month", ylab = "fire area", 
        main = "forest fire area for different months")

par(mfrow=c(3,3))
plot(log(area) ~ FFMC + DMC + DC + ISI + temp + RH + wind + rain +season,
     data = forest_fire)
par(mfrow=c(1,1))

#season
reg_season = lm(log(forest_fire$area)~forest_fire$season, data = forest_fire)
summary(reg_season)

#day
reg_day = lm(log(forest_fire$area)~day, data = forest_fire)
summary(reg_day)

model_all <- lm(area~season+X+Y+month+day+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data=forest_fire)

summary(model_all)

set.seed(69)
row.number<- sample(1:nrow(forest), size= 0.2* nrow(forest))
forest
row.number
forest_test<-  forest[row.number,]
dim(forest_test) ## Size of the testing set
forest_train<- forest[-row.number,]
dim(forest_train)  ## Size of training set
summary(forest_train)

#check the correlation matrix
M <- cor(forest_train[,-c(3,4)])
M

#correlation plot correlation between variables 
par(mfrow=c(1,1))
corrplot(M,method='number',bg = "white")

# And density curve for other variables also
plot(density(forest_train$FFMC))
plot(density(forest_train$DMC))
plot(density(forest_train$DC))
plot(density(forest_train$ISI))
plot(density(forest_train$temp))   
plot(density(forest_train$RH))
plot(density(forest_train$wind))
plot(density(forest_train$rain))
plot(density(forest_train$area))

plot(density(log(forest_train$rain))) # log
plot(density(log(forest_train$area))) # log









