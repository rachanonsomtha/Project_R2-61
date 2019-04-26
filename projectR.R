forest_fire <-
  read.csv(
    "https://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv"
  )

#Prepearing data

n_row = nrow(forest_fire)
head(forest_fire)

forest_area <- forest_fire$area
#Forest freq with area
hist(forest_area ,
     xlab = "Forest area",
     main = "Forest frequency with area",
     col = "gray")
hist(log(forest_area) ,
     xlab = "Log(Forest area)",
     main = "Forest frequency with area",
     col = "gray")

#Forest freq with Temperature
hist(forest_fire$temp ,
     xlab = "Forest Temperature",
     main = "Forest frequency with Temperature",
     col = "gray")


#observation of a forest fire.
#True =>  observed&&forest fires are triggered
#False  =>  observed&&forest fires aren't triggered
round(table(forest_area == 0) / (n_row), 3)
#check if NA avaliable?
summary(forest_fire)
summary(forest_fire$month)

forest_fire <- forest_fire[forest_fire$area > 0 ,]
dim(forest_fire)

plot(
  log(area) ~ as.factor(X),
  data = forest_fire,
  xlab = "X",
  ylab = "fire area",
  main = "forest fire area for different X's"
)

plot(
  log(area) ~ as.factor(Y),
  data = forest_fire,
  xlab = "Y",
  ylab = "fire area",
  main = "forest fire area for different Y's"
)

plot(
  log(area) ~ forest_fire$month,
  data = forest_fire,
  xlab = "month's",
  ylab = "fire area",
  main = "forest fire area for different month's"
)

plot(
  log(area) ~ forest_fire$day,
  data = forest_fire,
  xlab = "day's",
  ylab = "fire area",
  main = "forest fire area for different day's"
)

plot(
  log(area) ~ forest_fire$FFMC,
  data = forest_fire,
  xlab = "FFMC's",
  ylab = "fire area",
  main = "forest fire area for different FFMC's"
)

plot(
  log(area) ~ forest_fire$DMC,
  data = forest_fire,
  xlab = "DMC's",
  ylab = "fire area",
  main = "forest fire area for different DMC's"
)

plot(
  log(area) ~ forest_fire$DC,
  data = forest_fire,
  xlab = "DC's",
  ylab = "fire area",
  main = "forest fire area for different DC's"
)

plot(
  log(area) ~ forest_fire$ISI,
  data = forest_fire,
  xlab = "ISI's",
  ylab = "fire area",
  main = "forest fire area for different ISI's"
)

plot(
  log(area) ~ forest_fire$temp,
  data = forest_fire,
  xlab = "temp's",
  ylab = "fire area",
  main = "forest fire area for different temp's"
)

par(mfrow = c(2, 2))

plot(
  log(area) ~ forest_fire$RH,
  data = forest_fire,
  xlab = "RH's",
  ylab = "fire area",
  main = "forest fire area for different RH's"
)

plot(
  log(area) ~ forest_fire$wind,
  data = forest_fire,
  xlab = "Wind's",
  ylab = "fire area",
  main = "forest fire area for different Wind's"
)

plot(
  log(area) ~ forest_fire$rain,
  data = forest_fire,
  xlab = "Rain's",
  ylab = "fire area",
  main = "forest fire area for different Rain's"
)

plot(
  log(area) ~ forest_fire$rain,
  data = forest_fire,
  xlab = "Rain's",
  ylab = "fire area",
  main = "forest fire area for different Rain's"
)

# visualize how data represent in line graph
library(ggplot2)

ggplot(data = forest_fire) +
  geom_smooth(mapping = aes(x = X, y = area))

ggplot(data = forest_fire) +
  geom_smooth(mapping = aes(x = Y, y = area))

ggplot(data = forest_fire) +
  geom_smooth(mapping = aes(x = month, y = area))

ggplot(data = forest_fire) +
  geom_smooth(mapping = aes(x = day, y = area))

ggplot(data = forest_fire) +
  geom_smooth(mapping = aes(x = FFMC, y = area))

ggplot(data = forest_fire) +
  geom_smooth(mapping = aes(x = DMC, y = area))

ggplot(data = forest_fire) +
  geom_smooth(mapping = aes(x = DC, y = area))

ggplot(data = forest_fire) +
  geom_smooth(mapping = aes(x = ISI, y = area))

ggplot(data = forest_fire) +
  geom_smooth(mapping = aes(x = temp, y = area))

ggplot(data = forest_fire) +
  geom_smooth(mapping = aes(x = RH, y = area))

ggplot(data = forest_fire) +
  geom_smooth(mapping = aes(x = wind, y = area))

ggplot(data = forest_fire) +
  geom_smooth(mapping = aes(x = rain, y = area))

#################################

n_row = nrow(forest_fire)
summary(forest_fire$area)

#Seasons
forest_fire$season <- rep("spring", n_row)
for (i in 1:n_row) {
  if (forest_fire$month[i] %in% c("feb", "jan", "dec"))
    forest_fire$season[i] <- "winter"
  if (forest_fire$month[i] %in% c("oct", "nov", "sep"))
    forest_fire$season[i] <- "autumn"
  if (forest_fire$month[i] %in% c("aug", "jul", "jun"))
    forest_fire$season[i] <- "summer"
}
forest_fire$season <- as.factor(forest_fire$season)
par(mfrow = c(1, 1))

boxplot(
  log(area) ~ season,
  data = forest_fire,
  xlab = "season",
  ylab = "fire area",
  main = "forest fire area for different seasons"
)
#Summer have less forest fire area

#season
reg_season = lm(log(forest_fire$area) ~ forest_fire$season, data = forest_fire)
summary(reg_season)

par(mfrow = c(2, 2))
plot(log(area) ~ FFMC + DMC + DC + ISI + temp + RH + wind + rain + season,
     data = forest_fire)
par(mfrow = c(1, 1))

#day
reg_day = lm(log(forest_fire$area) ~ day, data = forest_fire)
summary(reg_day)

model_all <-
  lm(area ~ season + X + Y + month + day + FFMC + DMC + DC + ISI + temp +
       RH + wind + rain,
     data = forest_fire)
summary(model_all)

set.seed(69)
forest <-
  read.csv(
    "https://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv"
  )

row.number <- sample(1:nrow(forest), size = 0.2 * nrow(forest))
forest
row.number
forest_test <-  forest[row.number,]
dim(forest_test) ## Size of the testing set
forest_train <- forest[-row.number,]
dim(forest_train)  ## Size of training set
summary(forest_train)

#check the correlation matrix
M <- cor(forest_train[,-c(3, 4)])
M

#correlation plot correlation between variables
par(mfrow = c(1, 1))

library(corrplot)

corrplot(M,
         order = "hclust",
         tl.col = "black",
         tl.srt = 45)

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
par(mfrow = c(1, 2))

plot(density(log(forest_train$rain))) # log
plot(density(log(forest_train$area))) # log
par(mfrow = c(1, 1))


#################################


##############Prediction SVM
mydata <-
  read.csv(
    "https://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv"
  )

#mydata <- mydata[mydata$area>0,]

#nornalize data
normalise <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

dim(mydata)

mydata$temp <- normalise(mydata$temp)
mydata$rain <- normalise(mydata$rain)
mydata$RH <- normalise(mydata$RH)
mydata$wind <- normalise(mydata$wind)
sum(mydata$area < 5)
sum(mydata$area >= 5)

mydata$size <- NULL
mydata$size <- factor(ifelse(mydata$area < 5, 1, 0),
                      labels = c("small", "large"))
train <-
  sample(x = nrow(mydata),
         size = 0.2 * nrow(mydata),
         replace = FALSE)

library(kernlab)

m.poly <- ksvm(
  size ~ temp + RH + wind + rain,
  data = mydata[train, ],
  kernel = "polydot",
  C = 1
)
m.poly

m.rad <- ksvm(
  size ~ temp + RH + wind + rain,
  data = mydata[train, ],
  kernel = "rbfdot",
  C = 1
)
m.rad

m.tan <- ksvm(
  size ~ temp + RH + wind + rain,
  data = mydata[train, ],
  kernel = "tanhdot",
  C = 1
)
m.tan
#use  m.rad training error 18%
pred <-
  predict(m.rad, newdata = mydata[-train, ], type = "response")

library(e1071)
library(caret) # include library to use confusionMatrix
table(pred, mydata[-train, "size"])
dim(mydata[-train,])
data2 <-
  table(pred, mydata[-train, "size"])  #  [[]] gives the contents of a list
confusionMatrix(data2, positive = "small")


################

# Feature selection
set.seed(131)

forest_data <-
  read.csv(
    "https://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv"
  )
forest_data$temp <- normalise(forest_data$temp)
forest_data$rain <- normalise(forest_data$rain)
forest_data$RH <- normalise(forest_data$RH)
forest_data$wind <- normalise(forest_data$wind)

names(forest_data)

# drop column day
forest_data <- subset(forest_data, select = -c(day, month))


# 80% train 20 % test
space <- sample(
  x = nrow(forest_data),
  size = 0.2 * nrow(forest_data),
  replace = FALSE
)

forest_train <- forest_data[space, ]
forest_test <- forest_data[-space, ]

# Using Random Forest to select feature importance

# Random forest model

library(randomForest)

rf <-
  randomForest(area ~ .,
               data = forest_train,
               mtry = 3,
               importance = TRUE)
## Show "importance" of variables: higher value mean more important:
imp = round(importance(rf), 2)
imp_value = imp[,"IncNodePurity"]
df = data.frame(class=names(imp_value), importance=imp_value)
plot(df, aes(x=class))

# first 5 imporntance attributes: temp, DMC, DC, FFMC, RH

# Random forest model with feature importance
rf.importance <-
  randomForest(
    area ~ temp + DMC + DC + FFMC + RH,
    data = forest_train,
    mtry = 3
  )

summary(rf)
summary(rf.importance)

# Logistic Regression
lr = glm(area ~ ., data = forest_train)
summary(lr)

# Logistic Regression with feature importance
lr.importance =  glm(area ~ temp + DMC + DC + FFMC + RH, data = forest_train)
summary(lr.importance)


# Linear Regression
lmr <- lm(area ~ ., data = forest_train)
summary(lmr)

# Linear Regression with feature importance
lmr.importance <-
  lm(area ~ temp + DMC + DC + FFMC + RH, data = forest_train)
summary(lmr.importance)

library(rpart)
library(rpart.plot)

# Decision tree
dt <-
  rpart(area ~ ., data = forest_train, control = rpart.control(minsplit =
                                                                 5))
summary(dt)

# Decision Tree with feature importance
dt.importance <-
  rpart(area ~ temp + DMC + DC + FFMC + RH,
        data = forest_train,
        control = rpart.control(minsplit = 5))
summary(dt.importance)

MSE <- function(model, test, Y) {
  predictions <- predict(model, test)
  mse <- mean((Y - predictions) ^ 2)
  return(mse)
}

test = forest_test
Y = forest_test$area

showModelResult <- function() {
  cat('Random Forest:', MSE(rf, test, Y), '\n')
  cat('Random Forest2:', MSE(rf.importance, test, Y), '\n')
  cat('Logistic Regression:', MSE(lr, test, Y), '\n')
  cat('Logistic Regression2:', MSE(lr.importance, test, Y), '\n')
  cat('Linear Regression:', MSE(lmr, test, Y), '\n')
  cat('Linear Regression2:', MSE(lmr.importance, test, Y), '\n')
  cat('Decition Tree:', MSE(dt, test, Y), '\n')
  cat('Decition Tree2:', MSE(dt.importance, test, Y), '\n')
}

showModelResult()
