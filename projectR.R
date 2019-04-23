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

par(mfrow=c(1,1))

forest_fire <- forest_fire[forest_fire$area>0,]
forest_fire

# Let's explore the relationships between the response and the predictors
boxplot(log(area)~as.factor(X), data = forest_fire, xlab = "X", ylab = "fire area",
        main = "forest fire area for different X's")

n_row = nrow(forest_fire)

#Seasons
forest_fire$season <- rep("spring", n_row)
forest_fire$season
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

boxplot(log(area)~forest_fire$Y, data = forest_fire, xlab = "Y's", ylab = "fire area", 
        main = "forest fire area for different Y's")

boxplot(log(area)~forest_fire$X, data = forest_fire, xlab = "X's", ylab = "fire area", 
        main = "forest fire area for different X's")

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

# Now we check the correlation matrix
M <- cor(forest_train[,-c(3,4)])
M

#correlation plot correlation between variables 
corrplot(M,method='number',bg = "white")






