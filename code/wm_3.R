
# Options/Libraries -------------------------------------------------------
setwd("T://RNA//Baltimore//Jason//ad_hoc//wm//data")
pacman::p_load(autoencoder, bit64, caret, data.table, dplyr, FactoMineR, glmulti, h2o, Matrix,
               Metrics, mlbench, nnet, pls, SAENET, sqldf, varSelRF, vegan, xgboost)

test  <- fread("test.csv")
samp  <- fread("sample_submission.csv")
train <- fread("train.csv")

train.tr <- sample_n(train, 50000)
train.cv <- sample_n(train, 10000)



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

# Preprocess Training Data ------------------------------------------------
train$FinelineNumber        <- as.factor(train$FinelineNumber)
train$TripType              <- as.factor(train$TripType)
train$Weekday               <- as.factor(train$Weekday)
train$DepartmentDescription <- as.factor(train$DepartmentDescription)

# Rm ID
id                <- train$VisitNumber
#train$VisitNumber <- NULL

# Create flags to replace un-scalable columns, then drop original cols
train$flag_upc <- 0
train$flag_upc[train$Upc < 10000] <- 1

train$Upc <- NULL

train$flag_fineline <- 0
train$flag_fineline[train$FinelineNumber %in% c(4138, 4306, 4451, 4628, 6404, 635, 6302, 6111, 6101)] <- 1

#train$FinelineNumber <- NULL

train$flag_ScanCount_neg <- 0
train$flag_ScanCount_neg[train$ScanCount < 0] <- 1

train$flag_ScanCount_multiple <- 0
train$flag_ScanCount_multiple[train$ScanCount > 1] <- 1

train$flag_ScanCount_over_10 <- 0
train$flag_ScanCount_over_10[train$ScanCount > 10] <- 1

#train$ScanCount <- NULL

saveRDS(train, "train_xmas.RDS")
write.csv(train, "train_xmas.csv", row.names = F)
# Create Sparse Matrix
sparse_matrix     <- sparse.model.matrix(TripType~.-1, data = train)

# Create outcome variables
Y <- train$TripType
Y2 <- as.numeric(Y)
Y3 <- Y2 - 1 # Has to be zero-indexed

# Preprocess Test Data ----------------------------------------------------
test$FinelineNumber        <- as.factor(test$FinelineNumber)
test$Weekday               <- as.factor(test$Weekday)
test$DepartmentDescription <- as.factor(test$DepartmentDescription)

# Rm ID
test.id          <- test$VisitNumber
#test$VisitNumber <- NULL

# Create flags to replace un-scalable columns, then drop original cols
test$flag_upc <- 0
test$flag_upc[test$Upc < 10000] <- 1

test$Upc <- NULL

test$flag_fineline <- 0
test$flag_fineline[test$FinelineNumber %in% c(4138, 4306, 4451, 4628, 6404, 635, 6302, 6111, 6101)] <- 1

#test$FinelineNumber <- NULL

test$flag_ScanCount_neg <- 0
test$flag_ScanCount_neg[test$ScanCount < 0]                   <- 1

test$flag_ScanCount_multiple <- 0
test$flag_ScanCount_multiple[test$ScanCount > 1]              <- 1

test$flag_ScanCount_over_10 <- 0
test$flag_ScanCount_over_10[test$ScanCount > 10] <- 1

#test$ScanCount <- NULL

saveRDS(test, "test_dec26.RDS")
write.csv(test, "test_dec26.csv", row.names = F)
