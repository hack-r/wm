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
pacman::p_load(bit64, data.table, dplyr, Metrics, h2o, sqldf)
train <- fread("train.csv")
test  <- fread("test.csv")
samp  <- fread("sample_submission.csv")
h2o.init(nthreads=-1)


train2 <- sqldf("select VisitNumber, count(*) as records from train group by VisitNumber")
train3 <- sqldf("select a.*, b.records from train a left join train2 b on a.VisitNumber = b.VisitNumber")

test2 <- sqldf("select VisitNumber, count(*) as records from test group by VisitNumber")
test3 <- sqldf("select a.*, b.records from test a left join test2 b on a.VisitNumber = b.VisitNumber")

write.csv(train3, "train_with_record_counts.csv", row.names = F)
write.csv(test3, "test_with_record_counts.csv", row.names = F)

##############
# Read in data from H2O web browser flow
test.pred <- fread("C:\\Users\\jmiller\\Downloads\\drf750_multinomial_100lambda.csv")
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

saveRDS(sub, "drf750_multinomial_100lambda.RDS")
write.csv(sub, "drf750_multinomial_100lambda.csv", row.names = F)
##############

rm(train, train2)
train <- train3
rm(train3)
test <- test3
rm(test2, test3)