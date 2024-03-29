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
pacman::p_load(bit64, caret, data.table, dplyr, FactoMineR, glmulti, h2o, Matrix,
               Metrics, mlbench, nnet, pls, rattle, sqldf, varSelRF)
#h2o.init(nthreads=-1)


# Raw data import ---------------------------------------------------------
train <- fread("train.csv")
test  <- fread("test.csv")
samp  <- fread("sample_submission.csv")

# Set initial data types --------------------------------------------------
train$Upc <- as.numeric(train$Upc)
test$Upc  <- as.numeric(test$Upc)

train$Weekday <- as.factor(train$Weekday)
test$Weekday  <- as.factor(test$Weekday)

train$DepartmentDescription <- as.factor(train$DepartmentDescription)
test$DepartmentDescription  <- as.factor(test$DepartmentDescription)

train$FinelineNumber <- as.factor(train$FinelineNumber)
test$FinelineNumber  <- as.factor(test$FinelineNumber)

train$VisitNumber <- as.factor(train$VisitNumber)
test$VisitNumber  <- as.factor(test$VisitNumber)

train$TripType <- as.factor(train$TripType)

# Add record count --------------------------------------------------------
train2 <- sqldf("select VisitNumber, count(VisitNumber) as records from train group by VisitNumber")
train3 <- sqldf("select a.*, b.records from train a left join train2 b on a.VisitNumber = b.VisitNumber")

test2 <- sqldf("select VisitNumber, count(VisitNumber) as records from test group by VisitNumber")
test3 <- sqldf("select a.*, b.records from test a left join test2 b on a.VisitNumber = b.VisitNumber")

rm(train, train2)
train <- train3
rm(train3)

test <- test3
rm(test2, test3)

# Add flags/interactiosn/features to Train --------------------------------
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

train$scan_over_3_flag <- 0
train$scan_over_3_flag[train$ScanCount > 3] <- 1

train$scan_1_flag <- 0
train$scan_1_flag[train$ScanCount == 1] <- 1

train$return_flag <- 0
train$return_flag[train$ScanCount < 0] <- 1

train$scan_over_3_sunday_interaction <- train$scan_over_3_flag * train$sunday_flag

train$saturday_return <- train$saturday_flag * train$return_flag

train$sunday_return <- train$sunday_flag * train$return_flag

train$flag_weekend <- 0
train$flag_weekend[train$sunday_flag ==1 | train$saturday_flag ==1] <- 1

train$upc_flag <- 0
train$upc_flag[train$Upc < 10000] <- 1
#train$Upc <- NULL

train$top_upc_flag <- 0
train$top_upc_flag[train$Upc %in% c(9218868437227407360, 60538862097, 7874235186, 7874235187, 68113107862, 60538871457)] <- 1

train$interaction_weekend_records <- 0
train$interaction_weekend_records <- train$flag_weekend * train$records

train$flag_fineline <- 0
train$flag_fineline[train$FinelineNumber %in% c(4138, 4306, 4451, 4628, 6404, 635, 6302, 6111, 6101)] <- 1

## Batch factor to indicators
train <- with(train, data.frame(class.ind(DepartmentDescription), train[,]))
train <- with(train, data.frame(class.ind(FinelineNumber), train[,]))

## Counts of top departments
tmp <- aggregate(train$"PERSONAL.CARE", by = list(train$VisitNumber), FUN = sum)
colnames(tmp) <- c("VisitNumber", "sum_Personal_Care")

train <- sqldf("SELECT a.*, b.sum_Personal_Care
               FROM train a
               LEFT JOIN tmp b USING(VisitNumber)")

tmp <- aggregate(train$"GROCERY.DRY.GOODS", by = list(train$VisitNumber), FUN = sum)
colnames(tmp) <- c("VisitNumber", "sum_grocery")

train <- sqldf("SELECT a.*, b.sum_grocery
               FROM train a
               LEFT JOIN tmp b USING(VisitNumber)")

## Nonlinear transformations
train$log_records <- log(train$records)
train$sq_scans    <- train$ScanCount * train$ScanCount

## Model-based
flag_model         <- readRDS("flag40_model.RDS")
train$flag40_model <- predict(flag_model, newdata=train, type = "response")

flag_model         <- readRDS("flag39_model.RDS")
train$flag39_model <- predict(flag_model, newdata=train, type = "response")


# Same flags for Test -----------------------------------------------------
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

test$scan_over_3_flag <- 0
test$scan_over_3_flag[test$ScanCount > 3] <- 1

test$scan_1_flag <- 0
test$scan_1_flag[test$ScanCount == 1] <- 1

test$return_flag <- 0
test$return_flag[test$ScanCount < 0] <- 1

test$scan_over_3_sunday_interaction <- test$scan_over_3_flag * test$sunday_flag

test$saturday_return <- test$saturday_flag * test$return_flag

test$sunday_return <- test$sunday_flag * test$return_flag

test$flag_weekend <- 0
test$flag_weekend[test$sunday_flag == 1| test$saturday_flag ==1] <- 1

test$upc_flag <- 0
test$upc_flag[test$Upc < 10000] <- 1
#test$Upc <- NULL

test$interaction_weekend_records <- 0
test$interaction_weekend_records <- test$flag_weekend * test$records

test$flag_fineline <- 0
test$flag_fineline[test$FinelineNumber %in% c(4138, 4306, 4451, 4628, 6404, 635, 6302, 6111, 6101)] <- 1

test$top_upc_flag <- 0
test$top_upc_flag[test$Upc %in% c(9218868437227407360, 60538862097, 7874235186, 7874235187, 68113107862, 60538871457)] <- 1

## Batch indicators
test  <- with(test, data.frame(class.ind(DepartmentDescription), test[,]))
test <- with(test, data.frame(class.ind(FinelineNumber), test[,]))

## Counts of top departments by ID
tmp <- aggregate(test$"PERSONAL.CARE", by = list(test$VisitNumber), FUN = sum)
colnames(tmp) <- c("VisitNumber", "sum_Personal_Care")

test <- sqldf("SELECT a.*, b.sum_Personal_Care
              FROM test a
              LEFT JOIN tmp b USING(VisitNumber)")

tmp <- aggregate(test$"GROCERY.DRY.GOODS", by = list(test$VisitNumber), FUN = sum)
colnames(tmp) <- c("VisitNumber", "sum_grocery")

test <- sqldf("SELECT a.*, b.sum_grocery
              FROM test a
              LEFT JOIN tmp b USING(VisitNumber)")

## Nonlinear transformations
test$log_records <- log(test$records)
test$sq_scans    <- test$ScanCount * test$ScanCount

## model based
flag_model         <- readRDS("flag40_model.RDS")
test$flag40_model <- predict(flag_model, newdata=test, type = "response")

flag_model         <- readRDS("flag39_model.RDS")
test$flag39_model <- predict(flag_model, newdata=test, type = "response")

# Last second additions
train$flag_ScanCount_neg <- 0
train$flag_ScanCount_neg[train$ScanCount < 0] <- 1

train$flag_ScanCount_multiple <- 0
train$flag_ScanCount_multiple[train$ScanCount > 1] <- 1

train$flag_ScanCount_over_10 <- 0
train$flag_ScanCount_over_10[train$ScanCount > 10] <- 1


test$flag_ScanCount_neg <- 0
test$flag_ScanCount_neg[test$ScanCount < 0]                   <- 1

test$flag_ScanCount_multiple <- 0
test$flag_ScanCount_multiple[test$ScanCount > 1]              <- 1

test$flag_ScanCount_over_10 <- 0
test$flag_ScanCount_over_10[test$ScanCount > 10] <- 1


# Remove Redundant Features -----------------------------------------------
train$flag_40 <- NULL
train$flag_39 <- NULL
train$flag_39_40 <- NULL

# CN <- colnames(train)
# for( i in CN){
#   if (class(train[,i]) == "integer"){
#     train[,i] <- as.numeric(train[,i])
#   }
# }
#
# train$flag40_model <- na.roughfix(train$flag40_model)
#
# nums <- sapply(train, is.numeric)
# x    <- train[,nums]
# x    <- na.omit(x)
#
# set.seed(7)
# library(mlbench)
# library(caret)
# # calculate correlation matrix
# correlationMatrix <- cor(x[,!(colnames(x) %in% c("VisitNumber", "TripType"))])
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
# highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75, names = T)
# # print indexes of highly correlated attributes
# print(highlyCorrelated)
#
# Save pre-H2O data -------------------------------------------------------
sd <- setdiff(colnames(train), colnames(test))
sd <- sd[!(sd == "TripType")]
train <- train[,!(colnames(train) %in% sd)]

sd <- setdiff(colnames(test), colnames(train))
test <- test[,!(colnames(test) %in% sd)]

# saveRDS(train, "train_103vars.RDS")
# saveRDS(test, "test_102vars.RDS")
# write.csv(train, "train_103vars.csv",row.names = F)
# write.csv(test, "test_102vars.csv", row.names =F)

saveRDS(train, "train_5147vars.RDS")
saveRDS(test, "test_5147vars.RDS")
write.csv(train, "train_5147vars.csv",row.names = F)
write.csv(test, "test_5147vars.csv", row.names =F)


##############
# Read in data from H2O web browser flow
test.pred <- fread("T:\\RNA\\Baltimore\\Jason\\tmp\\2step_dec27.csv")
test.pred <- as.data.frame(test.pred)

test.pred$VisitNumber <- test$VisitNumber
head(test.pred$VisitNumber)

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
    TripType_12 = mean(p12, na.rm = T),
    TripType_14 = mean(p14, na.rm = T),
    TripType_15 = mean(p15, na.rm = T),
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

summary(sub)

saveRDS(sub, "2step.RDS")
write.csv(sub, "2step.csv", row.names = F)
zip("drf_600_102.zip", "drf_600_102.csv")
rm(sub)


# Ensemble Weighting ------------------------------------------------------
pcadrf   <- sub
drf600 <- readRDS("drf600.RDS")
drfglm <- readRDS("drf_glm_new_vars_dedup_500tree_100depth.RDS")

drfglm <- as.data.frame(drfglm)
pcadrf <- as.data.frame(pcadrf)
drf600 <- as.data.frame(drf600)

drfglm$VisitNumber <- NULL
pcadrf$VisitNumber   <- NULL
drf600$VisitNumber <- NULL

VisitNumber <- sub$VisitNumber

sub2 <- (drf600*.75) + (pcadrf *.25)
sub2 <- cbind(VisitNumber, sub2)

saveRDS(sub2, "post_model_blend_drf600_glmdrf_75_25.RDS")
write.csv(sub2, "post_model_blend_drf600_97_pca_01_xmas01_xgb01.csv", row.names = F)


sub2 <- (drf600*.99) + (pcadrf *.01)
sub2 <- cbind(VisitNumber, sub2)

saveRDS(sub2, "post_model_blend_drf600_pcadrf250_99_1.RDS")
write.csv(sub2, "post_model_blend_drf600_pcadrf250_99_1.csv", row.names = F)




sub2 <- (drf600*.90) + (as.data.table(drfglm) *.1)
sub2 <- cbind(VisitNumber, sub2)

saveRDS(sub2, "post_model_blend_drf_drfglm.RDS")
write.csv(sub2, "post_model_blend_drf_drfglm.csv", row.names = F)


# Deterministic Post-prediction Changes (exprimental) ---------------------
drf600 <- readRDS("drf600.RDS")
require(data.table)
test <- as.data.table(test)
sub2 <-
  test[,.(
    flag40     = mean(flag40_model, na.rm =T),
    flag39     = mean(flag39_model, na.rm = T)
  ),by=VisitNumber]

# sub3 <-
#   test[,(
#     flag40     = mean(flag40_model, na.rm =T),
#     flag39     = mean(flag39_model, na.rm = T)
#   ),VisitNumber]
#
# identical(sub2,sub3)

sub3 <- cbind(drf600, sub2)

sum(sub3$TripType_39) #10205.51 (10.6% of total)
sum(sub3$TripType_40) #6461.834 (6.7% of total)
(sum(sub3$TripType_39) + sum(sub3$TripType_40))/nrow(sub3) #0.1742097


prop.table(table(train$flag_39_40)) #0.4167627
prop.table(table(train$flag_39)) #0.1475982
prop.table(table(train$flag_40)) #0.2691646

table(sub3$flag40 > .5) #0.05910697
table(sub3$flag40 > .3) #0.2680457

table(sub3$flag39 > .1945) #about 14.5%

sub3$TripType_40[sub3$flag40 > .3] <- 1
sum(sub3$TripType_40)
sub4 <- sub3
sub4$flag39 <- NULL
sub4$flag40 <- NULL
sub4[,40] <- NULL
write.csv(sub4, "manual_adj_experiment1.csv")
