## File: wm.R ####
## Author: Jason D. Miller
## Description: Monolithic master script
## (c) 2015


# Info --------------------------------------------------------------------
# Data fields
#
# TripType - a categorical id representing the type of shopping trip the customer made. This is the ground truth that you are predicting. TripType_999 is an "other" category.
# VisitNumber - an id corresponding to a single trip by a single customer
# Weekday - the weekday of the trip
# Upc - the UPC number of the product purchased
# ScanCount - the number of the given item that was purchased. A negative value indicates a product return.
# DepartmentDescription - a high-level description of the item's department
#     FinelineNumber - a more refined category for each of the products, created by Walmart


# Options, libraries, init h2o --------------------------------------------
#setwd("C:/Users/jmiller/Desktop")
#setwd("C:/Users/Jason/Desktop")
setwd("T://RNA//Baltimore//Jason//ad_hoc//wm//data")

#install.packages("pacman")
require(pacman)
pacman::p_load(bit64, data.table, dplyr, Metrics, h2o)
train <- fread("train.csv")
test  <- fread("test.csv")
samp  <- fread("sample_submission.csv")
h2o.init(nthreads=-1)

##############
# Read in data from H2O web browser flow
test.pred <- fread("C:\\Users\\jmiller\\Downloads\\drf_100_25percent.csv")
test.pred <- as.data.frame(test.pred)

summary(test.pred)

test.pred$predict <- NULL
test.pred$Weekday <- NULL
test.pred$Upc     <- NULL
test.pred$DepartmentDescription <- NULL
test.pred$FinelineNumber        <- NULL
test.pred$ScanCount             <- NULL

pred <- as.data.table(test.pred)

sub <-
pred[,.(
  TripType_3  = mean(p3,na.rm=T),
  TripType_4  = mean(p4,na.rm=T),
  TripType_5  = mean(p5,na.rm=T),
  TripType_6  = mean(p6,na.rm=T),
  TripType_7  = mean(p7,na.rm=T),
  TripType_8  = mean(p8, na.rm = T),
  TripType_9  = mean(p9, na.rm = T),
#  TripType_10 = mean(p10, na.rm = T),
  TripType_12 = mean(p12, na.rm = T),
  #TripType_13 = mean(p13, na.rm = T),
  TripType_14 = mean(p14, na.rm = T),
  TripType_15 = mean(p15, na.rm = T),
  #TripType_16 = mean(p16, na.rm = T),
#  TripType_17 = mean(p17, na.rm = T),
  TripType_18 = mean(p18, na.rm = T),
  TripType_19 = mean(p19, na.rm = T),
  TripType_20 = mean(p20, na.rm = T),
  TripType_21 = mean(p21, na.rm = T),
  TripType_22 = mean(p22, na.rm = T),
  TripType_23 = mean(p23, na.rm = T),
  TripType_24 = mean(p24, na.rm = T),
  TripType_25 = mean(p25, na.rm = T),
  TripType_26 = mean(p26, na.rm = T),
  TripType_27 = mean(p27, na.rm = T),
  TripType_28 = mean(p28, na.rm = T),
  TripType_29 = mean(p29, na.rm = T),

  TripType_30 = mean(p30, na.rm = T),
  TripType_31 = mean(p31, na.rm = T),
  TripType_32 = mean(p32, na.rm = T),
  TripType_33 = mean(p33, na.rm = T),
  TripType_34 = mean(p34, na.rm = T),
  TripType_35 = mean(p35, na.rm = T),
  TripType_36 = mean(p36, na.rm = T),
  TripType_37 = mean(p37, na.rm = T),
  TripType_38 = mean(p38, na.rm = T),
  TripType_39 = mean(p39, na.rm = T),

  TripType_40 = mean(p40, na.rm = T),
  TripType_41 = mean(p41, na.rm = T),
  TripType_42 = mean(p42, na.rm = T),
  TripType_43 = mean(p43, na.rm = T),
  TripType_44 = mean(p44, na.rm = T),
  TripType_999 = mean(p999, na.rm = T)
),VisitNumber]

summary(pred)

saveRDS(sub, "init_submit.RDS")
write.csv(sub, "sub_glm.csv", row.names = F)
##############


train$TripType_3   <- 0
train$TripType_4   <- 0
train$TripType_5   <- 0
train$TripType_6   <- 0
train$TripType_7   <- 0
train$TripType_8   <- 0
train$TripType_9   <- 0
train$TripType_10  <- 0
train$TripType_11  <- 0
train$TripType_12  <- 0
train$TripType_13   <- 0
train$TripType_14   <- 0
train$TripType_15   <- 0
train$TripType_16   <- 0
train$TripType_17   <- 0
train$TripType_18   <- 0
train$TripType_19   <- 0
train$TripType_20  <- 0
train$TripType_21  <- 0
train$TripType_22  <- 0
train$TripType_23   <- 0
train$TripType_24   <- 0
train$TripType_25   <- 0
train$TripType_26   <- 0
train$TripType_27   <- 0
train$TripType_28   <- 0
train$TripType_29   <- 0
train$TripType_30  <- 0
train$TripType_31  <- 0
train$TripType_32  <- 0
train$TripType_33   <- 0
train$TripType_34   <- 0
train$TripType_35   <- 0
train$TripType_36   <- 0
train$TripType_37   <- 0
train$TripType_38   <- 0
train$TripType_39   <- 0
train$TripType_40  <- 0
train$TripType_41  <- 0
train$TripType_42  <- 0
train$TripType_43  <- 0
train$TripType_44  <- 0
train$TripType_999 <- 0

train$TripType_3[train$TripType == "3"]   <- 1
train$TripType_4[train$TripType == "4"]   <- 1
train$TripType_5[train$TripType == "5"]   <- 1
train$TripType_6[train$TripType == "6"]   <- 1
train$TripType_7[train$TripType == "7"]   <- 1
train$TripType_8[train$TripType == "8"]   <- 1
train$TripType_9[train$TripType == "9"]   <- 1
train$TripType_10[train$TripType == "10"]  <- 1
train$TripType_11[train$TripType == "11"]  <- 1
train$TripType_12[train$TripType == "12"]  <- 1
train$TripType_13[train$TripType == "13"]   <- 1
train$TripType_14[train$TripType == "14"]   <- 1
train$TripType_15[train$TripType == "15"]   <- 1
train$TripType_16[train$TripType == "16"]   <- 1
train$TripType_17[train$TripType == "17"]   <- 1
train$TripType_18[train$TripType == "18"]   <- 1
train$TripType_19[train$TripType == "19"]   <- 1
train$TripType_20[train$TripType == "20"]  <- 1
train$TripType_21[train$TripType == "21"]  <- 1
train$TripType_22[train$TripType == "22"]  <- 1
train$TripType_23[train$TripType == "23"]   <- 1
train$TripType_24[train$TripType == "24"]   <- 1
train$TripType_25[train$TripType == "25"]   <- 1
train$TripType_26[train$TripType == "26"]   <- 1
train$TripType_27[train$TripType == "27"]   <- 1
train$TripType_28[train$TripType == "28"]   <- 1
train$TripType_29[train$TripType == "29"]   <- 1
train$TripType_30[train$TripType == "30"]  <- 1
train$TripType_31[train$TripType == "31"]  <- 1
train$TripType_32[train$TripType == "32"]  <- 1
train$TripType_33[train$TripType == "33"]   <- 1
train$TripType_34[train$TripType == "34"]   <- 1
train$TripType_35[train$TripType == "35"]   <- 1
train$TripType_36[train$TripType == "36"]   <- 1
train$TripType_37[train$TripType == "37"]   <- 1
train$TripType_38[train$TripType == "38"]   <- 1
train$TripType_39[train$TripType == "39"]   <- 1
train$TripType_40[train$TripType == "40"]  <- 1
train$TripType_41[train$TripType == "41"]  <- 1
train$TripType_42[train$TripType == "42"]  <- 1
train$TripType_43[train$TripType == "43"]  <- 1
train$TripType_44[train$TripType == "44"]  <- 1
train$TripType_999[train$TripType == "999"] <- 1

train$sunday_flag    <- 0
train$monday_flag    <- 0
train$tuesday_flag   <- 0
train$wednesday_flag <- 0
train$thursday_flag  <- 0
train$friday_flag    <- 0
train$saturday_flag  <- 0

train$sunday_flag[train$Weekday == "Sunday"]       <- 1
train$monday_flag[train$Weekday == "Monday"]       <- 1
train$tuesday_flag[train$Weekday == "Tuesday"]     <- 1
train$wednesday_flag[train$Weekday == "Wednesday"] <- 1
train$thursday_flag[train$Weekday == "Thursday"]   <- 1
train$friday_flag[train$Weekday == "Friday"]       <- 1
train$saturday_flag[train$Weekday == "Saturday"]   <- 1

test$sunday_flag    <- 0
test$monday_flag    <- 0
test$tuesday_flag   <- 0
test$wednesday_flag <- 0
test$thursday_flag  <- 0
test$friday_flag    <- 0
test$saturday_flag  <- 0

test$sunday_flag[test$Weekday == "Sunday"]       <- 1
test$monday_flag[test$Weekday == "Monday"]       <- 1
test$tuesday_flag[test$Weekday == "Tuesday"]     <- 1
test$wednesday_flag[test$Weekday == "Wednesday"] <- 1
test$thursday_flag[test$Weekday == "Thursday"]   <- 1
test$friday_flag[test$Weekday == "Friday"]       <- 1
test$saturday_flag[test$Weekday == "Saturday"]   <- 1

train$Weekday <- NULL
test$Weekday  <- NULL

train$Upc <- as.factor(train$Upc)
test$Upc  <- as.factor(test$Upc)

train$FinelineNumber <- as.factor(train$FinelineNumber)
test$FinelineNumber  <- as.factor(test$FinelineNumber)

train$DepartmentDescription <- as.factor(train$DepartmentDescription)
test$DepartmentDescription  <- as.factor(test$DepartmentDescription)

#train <- na.omit(train);gc()
#train2 <- melt(train, id.vars=c("VisitNumber"))
data_wide <- dcast(train, "VisitNumber  ~ ", value.var="target")

trainHex<-as.h2o(train[,.(
  target       = mean(TripType_3, na.rm=T),
  meanSunday   = mean(sunday_flag,na.rm=T),
  meanMonday   = mean(monday_flag,na.rm=T),
  meanTuesday  = mean(tuesday_flag,na.rm=T),
  meanThursday = mean(thursday_flag,na.rm=T),
  meanFriday   = mean(friday_flag,na.rm=T),
  meanSat      = mean(saturday_flag, na.rm = T),
  meanDept      = mean(DepartmentDescription, na.rm = T),
  records      = .N
),VisitNumber],destination_frame="train.hex")

t <- as.h2o(aus)
h2o.gbm(y = "target", x = "meanSunday", data = australia.hex, n.trees
        = 15, interaction.depth = 5,
        n.minobsinnode = 2, shrinkage = 0.01, distribution= "multinomial")

rfHex<-h2o.randomForest(x=c("meanSunday", "meanMonday","meanTuesday", "meanThursday", "meanFriday", "meanDept", "records"), ntrees = 300,
                        y="target",training_frame=trainHex,model_id="drf_flags.hex")
rfHex<-h2o.gbm(x=c("meanSunday", "meanMonday","meanTuesday", "meanThursday", "meanFriday", "meanDept", "records"), ntrees = 300,
               y="target",training_frame=trainHex,model_id="drf_flags.hex")

testHex<-as.h2o(test[,.(
  meanSunday   = mean(sunday_flag,na.rm=T),
  meanMonday   = mean(monday_flag,na.rm=T),
  meanTuesday  = mean(tuesday_flag,na.rm=T),
  meanThursday = mean(thursday_flag,na.rm=T),
  meanFriday   = mean(friday_flag,na.rm=T),
  meanSat      = mean(saturday_flag, na.rm = T),
  meanDept      = mean(DepartmentDescription, na.rm = T),
  records      = .N
),VisitNumber],destination_frame="test.hex")

submission  <- samp
predictions <- as.data.frame(h2o.predict(rfHex,testHex))

summary(predictions$predict)
summary(submission$Expected)
cat("The H2O Starter prediction looked like sh*t!")
plot(density(predictions$predict))
plot(density(submission$Expected))
#submission$Expected<-expm1(predictions$predict)*0.5+submission$Expected*0.5
write.csv(submission,"randomForest_H2O_fork.csv",row.names=F)


####################################################################################
## Appendix: h2o.randomForest API
####################################################################################
##h2o.randomForest(x, y, training_frame, model_id, validation_frame, checkpoint,
##  mtries = -1, sample_rate = 0.632, build_tree_one_node = FALSE,
##  ntrees = 50, max_depth = 20, min_rows = 1, nbins = 20,
##  nbins_cats = 1024, binomial_double_trees = FALSE,
##  balance_classes = FALSE, max_after_balance_size = 5, seed,
##  offset_column = NULL, weights_column = NULL, nfolds = 0,
##  fold_column = NULL, fold_assignment = c("AUTO", "Random", "Modulo"),
##  keep_cross_validation_predictions = FALSE, ...)
## Arguments

## x
## A vector containing the names or indices of the predictor variables to use in building the GBM model.

## y
## The name or index of the response variable. If the data does not contain a header, this is the column index number starting at 1, and increasing from left to right. (The response must be either an integer or a categorical variable).

## training_frame
## An H2OFrame object containing the variables in the model.

## model_id
## (Optional) The unique id assigned to the resulting model. If none is given, an id will automatically be generated.

## validation_frame
## An H2OFrame object containing the variables in the model.

## checkpoint
## "Model checkpoint (either key or H2ODeepLearningModel) to resume training with."

## mtries
## Number of variables randomly sampled as candidates at each split. If set to -1, defaults to sqrtp for classification, and p/3 for regression, where p is the number of predictors.

## sample_rate
## Sample rate, from 0 to 1.0.   (edit: row sampling, per tree)

## build_tree_one_node
## Run on one node only; no network overhead but fewer cpus used. Suitable for small datasets.

## ntrees
## A nonnegative integer that determines the number of trees to grow.

## max_depth
## Maximum depth to grow the tree.

## min_rows
## Minimum number of rows to assign to teminal nodes.

## nbins
## For numerical columns (real/int), build a histogram of this many bins, then split at the best point.

## nbins_cats
## For categorical columns (enum), build a histogram of this many bins, then split at the best point. Higher values can lead to more overfitting.

## binomial_double_trees
## For binary classification: Build 2x as many trees (one per class) - can lead to higher accuracy.

## balance_classes
## logical, indicates whether or not to balance training data class counts via over/under-sampling (for imbalanced data)

## max_after_balance_size
## Maximum relative size of the training data after balancing class counts (can be less than 1.0)

## seed
## Seed for random numbers (affects sampling) - Note: only reproducible when running single threaded

## offset_column
## Specify the offset column.

## weights_column
## Specify the weights column.

## nfolds
## (Optional) Number of folds for cross-validation. If nfolds >= 2, then validation must remain empty.

## fold_column
## (Optional) Column with cross-validation fold index assignment per observation

## fold_assignment
## Cross-validation fold assignment scheme, if fold_column is not specified Must be "AUTO", "Random" or "Modulo"

## keep_cross_validation_predictions
## Whether to keep the predictions of the cross-validation models