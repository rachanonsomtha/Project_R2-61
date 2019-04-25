forest_fire <- read.csv("forestfires.csv")

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

par(mfrow = c(3, 3))

forest_fire <- forest_fire[forest_fire$area > 0 , ]
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
row.number <- sample(1:nrow(forest), size = 0.2 * nrow(forest))
forest
row.number
forest_test <-  forest[row.number, ]
dim(forest_test) ## Size of the testing set
forest_train <- forest[-row.number, ]
dim(forest_train)  ## Size of training set
summary(forest_train)

#check the correlation matrix
M <- cor(forest_train[, -c(3, 4)])
M

#correlation plot correlation between variables
par(mfrow = c(1, 1))
corrplot(M, method = 'number', bg = "white")

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


#################################
forest_fire <-read.csv("forestfires.csv")

summary(forest_fire)
forest_fire <- forest_fire[forest_fire$area > 0,]
summary(forest_fire)
prin_data <- forest_fire[c(1:2, 5:12)]
head(prin_data)

sum(is.na(prin_data))


pc <- princomp(prin_data)
plot(pc)
summary(pc)
prin_data <- forest_fire[c(1:2)]
head(prin_data)

pc
plot(prin_data,
     cex = 0.9,
     col = "blue",
     main = "Plot")

standardize <- function(x) {
  (x - mean(x))
}
my.scaled.classes = apply(prin_data, 2, function(x)
  (x - mean(x)))
plot(
  my.scaled.classes,
  cex = 0.9,
  col = "blue",
  main = "Plot",
  sub = "Mean Scaled",
  xlim = c(-30, 30)
)

##############Prediction SVM
mydata <-read.csv("forestfires.csv")

#nornalize data
normalise <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

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
  data = mydata[train,],
  kernel = "polydot",
  C = 1
)
m.poly

m.rad <- ksvm(
  size ~ temp + RH + wind + rain,
  data = mydata[train,],
  kernel = "rbfdot",
  C = 1
)
m.rad

m.tan <- ksvm(
  size ~ temp + RH + wind + rain,
  data = mydata[train,],
  kernel = "tanhdot",
  C = 1
)
m.tan
#use  m.rad training error 24%
pred <-
  predict(m.rad, newdata = mydata[-train,], type = "response")

library(e1071)
library(caret) # include library to use confusionMatrix
table(pred, mydata[-train, "size"])
dim(mydata[-train, ])
data2 <-
  table(pred, mydata[-train, "size"])  #  [[]] gives the contents of a list
confusionMatrix(data2, positive = "small")


# Feature selection
set.seed(131)

forest_data <- read.csv("forestfires.csv")

forest_data$temp <- normalise(forest_data$temp)
forest_data$rain <- normalise(forest_data$rain)
forest_data$RH <- normalise(forest_data$RH)
forest_data$wind <- normalise(forest_data$wind)

names(forest_data)

# drop column day
forest_data <- subset(forest_data, select = -c(day))

# Using Random Forest to select feature importance
area.rf <-
  randomForest(
    area ~ .,
    data = forest_data,
    mtry = 3,
    importance = TRUE,
    na.action = na.omit
  )

print(area.rf)
## Show "importance" of variables: higher value mean more important:
round(importance(area.rf), 2)
# first 5 imporntance attributes: temp, DMC, DC, FFMC, RH


# Logistic Regression
lr = glm(area ~ ., data = forest_data)
summary(lr)

lr.importance =  glm(area ~ temp + DMC + DC + FFMC + RH, data = forest_data)
summary(lr.importance)