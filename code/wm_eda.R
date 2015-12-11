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

set.seed(3456)
train.samp <- sample_n(train, 20000)
trainIndex <- createDataPartition(train.samp$flag_39_40, p = .7,
                                  list = FALSE,
                                  times = 1)
head(trainIndex,100)
flagTrain <- train.samp[ trainIndex,]
flagTest  <- train.samp[-trainIndex,]

flagTrain$sunday_flag    <- NULL
flagTrain$monday_flag    <- NULL
flagTrain$tuesday_flag   <- NULL
flagTrain$wednesday_flag <- NULL
flagTrain$thursday_flag  <- NULL
flagTrain$friday_flag    <- NULL
flagTrain$saturday_flag  <- NULL
flagTrain$flag_39        <- NULL
flagTrain$flag_40        <- NULL

flagTest$sunday_flag    <- NULL
flagTest$monday_flag    <- NULL
flagTest$tuesday_flag   <- NULL
flagTest$wednesday_flag <- NULL
flagTest$thursday_flag  <- NULL
flagTest$friday_flag    <- NULL
flagTest$saturday_flag  <- NULL
flagTest$flag_39        <- NULL
flagTest$flag_40        <- NULL

summary(flag_model <- glm("flag_40 ~ FinelineNumber + Weekday + records + interaction_weekend_records", data = flagTrain), family = "binomial", link = "logit")

flag_pred <- predict(flag_model, newdata = flagTest, type = "response")

confusion.matrix(flag_pred, flagTest$flag_40)