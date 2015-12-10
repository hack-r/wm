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
pacman::p_load(bit64, data.table, dplyr, Metrics, h2o, sqldf, glmulti, nnet)
train <- fread("train.csv")
test  <- fread("test.csv")
samp  <- fread("sample_submission.csv")
h2o.init(nthreads=-1)

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

# Add other flags ---------------------------------------------------------
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
train$Upc <- NULL

train$interaction_weekend_records <- 0
train$interaction_weekend_records <- train$flag_weekend * train$records

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
test$Upc <- NULL

test$interaction_weekend_records <- 0
test$interaction_weekend_records <- test$flag_weekend * test$records


# Remove unneeded columns -------------------------------------------------
train$sunday_flag    <- NULL
train$monday_flag    <- NULL
train$tuesday_flag   <- NULL
train$wednesday_flag <- NULL
train$thursday_flag  <- NULL
train$friday_flag    <- NULL
train$saturday_flag  <- NULL

test$sunday_flag    <- NULL
test$monday_flag    <- NULL
test$tuesday_flag   <- NULL
test$wednesday_flag <- NULL
test$thursday_flag  <- NULL
test$friday_flag    <- NULL
test$saturday_flag  <- NULL

# Save pre-H2O data -------------------------------------------------------
saveRDS(train, "train_enhanced.RDS")
saveRDS(test, "test_enhanced.RDS")
write.csv(train, "train_enhanced.csv",row.names = F)
write.csv(test, "test_enhanced.csv", row.names =F)


##############
# Read in data from H2O web browser flow
test.pred <- fread("C:\\Users\\jmiller\\Downloads\\drf_glm_prebal.csv")
test.pred <- as.data.frame(test.pred)

test.pred$VisitNumber <- test$VisitNumber

# test.pred$predict <- NULL
# test.pred$Weekday <- NULL
# test.pred$Upc     <- NULL
# test.pred$DepartmentDescription <- NULL
# test.pred$FinelineNumber        <- NULL
# test.pred$ScanCount             <- NULL

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

summary(pred)

saveRDS(sub, "drf_glm_prebal.RDS")
write.csv(sub, "drf_glm_prebal.csv", row.names = F)
##############


# Experimental - pre-balancing and reducing the data ----------------------
a <- train[NULL,]

for(i in levels(train$TripType)){
  a <- rbind(a, sample_n(train[train$TripType == i,],35))
}

train.tmp <- setDT(a)[ ,lapply(.SD, function(x) if(is.numeric(x)) mean(x, na.rm=TRUE) else
                names(which.max(table(x)))) , by=VisitNumber]

test.tmp <- setDT(test)[ ,lapply(.SD, function(x) if(is.numeric(x)) mean(x, na.rm=TRUE) else
                names(which.max(table(x)))) , by=VisitNumber]
write.csv(train.tmp, "train_special.csv", row.names = F)
write.csv(test.tmp,  "test_special.csv", row.names = F)

saveRDS(train.tmp, "train_tmp.RDS")
saveRDS(test.tmp, "test_tmp.RDS")
