
# Options/Libraries -------------------------------------------------------
setwd("T://RNA//Baltimore//Jason//ad_hoc//wm//data")
pacman::p_load(bit64, caret, data.table, dplyr, FactoMineR, glmulti, h2o, Matrix,
               Metrics, mlbench, nnet, pls, rattle, sqldf, varSelRF, xgboost)

test  <- fread("test.csv")
samp  <- fread("sample_submission.csv")
train <- fread("train.csv")

# Preprocess Training Data ------------------------------------------------
train$FinelineNumber        <- as.factor(train$FinelineNumber)
train$TripType              <- as.factor(train$TripType)
train$Weekday               <- as.factor(train$Weekday)
train$DepartmentDescription <- as.factor(train$DepartmentDescription)

# Rm ID
id                <- train$VisitNumber
train$VisitNumber <- NULL

# Create flags to replace un-scalable columns, then drop original cols
train$flag_upc <- 0
train$flag_upc[train$Upc < 10000] <- 1

train$Upc <- NULL

train$flag_fineline <- 0
train$flag_fineline[train$FinelineNumber %in% c(4138, 4306, 4451, 4628, 6404, 635, 6302, 6111, 6101)] <- 1

train$FinelineNumber <- NULL

train$flag_ScanCount_neg <- 0
train$flag_ScanCount_neg[train$ScanCount < 0] <- 1

train$flag_ScanCount_multiple <- 0
train$flag_ScanCount_multiple[train$ScanCount > 1] <- 1

train$flag_ScanCount_over_10 <- 0
train$flag_ScanCount_over_10[train$flag_ScanCount_over_10 > 10] <- 1

train$ScanCount <- NULL

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
test$VisitNumber <- NULL

# Create flags to replace un-scalable columns, then drop original cols
test$flag_upc <- 0
test$flag_upc[test$Upc < 10000] <- 1

test$Upc <- NULL

test$flag_fineline <- 0
test$flag_fineline[test$FinelineNumber %in% c(4138, 4306, 4451, 4628, 6404, 635, 6302, 6111, 6101)] <- 1

test$FinelineNumber <- NULL

test$flag_ScanCount_neg <- 0
test$flag_ScanCount_neg[test$ScanCount < 0]                   <- 1

test$flag_ScanCount_multiple <- 0
test$flag_ScanCount_multiple[test$ScanCount > 1]              <- 1

test$flag_ScanCount_over_10 <- 0
test$flag_ScanCount_over_10[test$flag_ScanCount_over_10 > 10] <- 1

test$ScanCount <- NULL

# Create Sparse Matrix
test_sparse_matrix     <- sparse.model.matrix(~.-1, data = test)

# Model -------------------------------------------------------------------
bst          <- xgboost(data = sparse_matrix,label = Y3, max.depth = 6, eta = .3, nround = 100,n_parallel_trees = 1000, #ntree = 100, #,
                        nthread = 50, objective = "multi:softprob", num_class=38, eval_metric = "mlogloss",
                        subsample = .6, colsample_bytree = .6, nfolds = 3, gamma = .9, max_delta_step = 10)

# bst.discreet <- xgboost(data = sparse_matrix, label = Y3, max.depth = 6, eta = .3, nround =10,
#                   nthread = 2, objective = "multi:softmax", num_class=38)

# Predict -----------------------------------------------------------------
pred       <- predict(bst, test_sparse_matrix)
saveRDS(pred, "pred.RDS")

predMatrix <- data.frame(matrix(pred, ncol=38, byrow=TRUE))

res <- data.frame(test.id, predMatrix)
colnames(res) <- colnames(samp)

pred <- as.data.table(res)

sub <-
  pred[,.(
    TripType_3  = mean(TripType_3,na.rm=T),
    TripType_4  = mean(TripType_4,na.rm=T),
    TripType_5  = mean(TripType_5,na.rm=T),
    TripType_6  = mean(TripType_6,na.rm=T),
    TripType_7  = mean(TripType_7,na.rm=T),
    TripType_8  = mean(TripType_8, na.rm = T),
    TripType_9  = mean(TripType_9, na.rm = T),
    TripType_12 = mean(TripType_12, na.rm = T),
    TripType_14 = mean(TripType_14, na.rm = T),
    TripType_15 = mean(TripType_15, na.rm = T),
    TripType_18 = mean(TripType_18, na.rm = T),
    TripType_19 = mean(TripType_19, na.rm = T),
    TripType_20 = mean(TripType_20, na.rm = T),
    TripType_21 = mean(TripType_21, na.rm = T),
    TripType_22 = mean(TripType_22, na.rm = T),
    TripType_23 = mean(TripType_23, na.rm = T),
    TripType_24 = mean(TripType_24, na.rm = T),
    TripType_25 = mean(TripType_25, na.rm = T),
    TripType_26 = mean(TripType_26, na.rm = T),
    TripType_27 = mean(TripType_27, na.rm = T),
    TripType_28 = mean(TripType_28, na.rm = T),
    TripType_29 = mean(TripType_29, na.rm = T),
    TripType_30 = mean(TripType_30, na.rm = T),
    TripType_31 = mean(TripType_31, na.rm = T),
    TripType_32 = mean(TripType_32, na.rm = T),
    TripType_33 = mean(TripType_33, na.rm = T),
    TripType_34 = mean(TripType_34, na.rm = T),
    TripType_35 = mean(TripType_35, na.rm = T),
    TripType_36 = mean(TripType_36, na.rm = T),
    TripType_37 = mean(TripType_37, na.rm = T),
    TripType_38 = mean(TripType_38, na.rm = T),
    TripType_39 = mean(TripType_39, na.rm = T),
    TripType_40 = mean(TripType_40, na.rm = T),
    TripType_41 = mean(TripType_41, na.rm = T),
    TripType_42 = mean(TripType_42, na.rm = T),
    TripType_43 = mean(TripType_43, na.rm = T),
    TripType_44 = mean(TripType_44, na.rm = T),
    TripType_999 = mean(TripType_999, na.rm = T)
  ),VisitNumber]

summary(sub)

# 3.3 score from eta = 1000, depth = 10, by row, num_parallel_trees = 100

write.csv(sub, 'xgboost_pred.csv', quote = F, row.names = F)
zip("xgboost_pred.zip", "xgboost_pred.csv")

# Alternate Prediction for 2-step Modeling --------------------------------
step1.train <- predict(bst, sparse_matrix)
step1.test  <- predict(bst, test_sparse_matrix)