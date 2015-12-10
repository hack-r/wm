barplot(table(train$Weekday))
barplot(table(train$TripType))
barplot(table(train$DepartmentDescription))

train$flag_40    <- 0
train$flag_39    <- 0
train$flag_39_40 <- 0

train$flag_40[train$TripType == "40"]                             <- 1
train$flag_40[train$TripType == "39"]                             <- 1
train$flag_39_40[train$TripType == "39" | train$TripType == "40"] <- 1

prop.table(table(train$flag_39_40))
prop.table(table(train$Weekday ))

tmp <- na.omit(train)
tmp <- sample_n(tmp, 500)

prop.table(table(tmp$flag_39_40))

glmulti.lm.out <-
  glmulti("flag_39_40 ~ Weekday + DepartmentDescription + ScanCount + records + flag_weekend",
          family = "binomial", data = tmp,
          level = 2,               # No interaction considered
          method = "h",            # Exhaustive approach
          crit = "bic",            # AIC as criteria
          confsetsize = 2,         # Keep 5 best models
          plotty = F, report = F,  # No plot or interim reports
          fitfunction = "lm")      # lm function

## Show best models (Use @ instead of $ for an S4 object)
glmulti.lm.out@formulas


# Predict flag_39_40 ------------------------------------------------------
train$sunday_flag    <- NULL
train$monday_flag    <- NULL
train$tuesday_flag   <- NULL
train$wednesday_flag <- NULL
train$thursday_flag  <- NULL
train$friday_flag    <- NULL
train$saturday_flag  <- NULL
train$flag_39        <- NULL
train$flag_40        <- NULL

test$sunday_flag    <- NULL
test$monday_flag    <- NULL
test$tuesday_flag   <- NULL
test$wednesday_flag <- NULL
test$thursday_flag  <- NULL
test$friday_flag    <- NULL
test$saturday_flag  <- NULL
test$flag_39        <- NULL
test$flag_40        <- NULL

set.seed(3456)
train.samp <- sample_n(train,10000)
trainIndex <- createDataPartition(train.samp$flag_39_40, p = .7,
                                  list = FALSE,
                                  times = 1)
head(trainIndex,100)
flagTrain <- train[ trainIndex,]
flagTest  <- train[-trainIndex,]

summary(glm("flag_39_40 ~ .", data = flagTrain), family = "binomial", link = "logit")
