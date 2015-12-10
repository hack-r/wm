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

pacman::p_load(glmulti)

tmp <- na.omit(train)
require(dplyr)
tmp <- sample_n(tmp, 5000)

glmulti.lm.out <-
  glmulti("flag_39_40 ~ Weekday + DepartmentDescription + ScanCount + records + flag_weekend", data = tmp,
          level = 2,               # No interaction considered
          method = "h",            # Exhaustive approach
          crit = "aic",            # AIC as criteria
          confsetsize = 2,         # Keep 5 best models
          plotty = F, report = F,  # No plot or interim reports
          fitfunction = "lm")      # lm function

## Show 5 best models (Use @ instead of $ for an S4 object)
glmulti.lm.out@formulas
