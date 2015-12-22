
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
bst          <- xgboost(data = sparse_matrix, label = Y3, max.depth = 6, eta = .3, nround =10,
                        nthread = 20, objective = "multi:softprob", num_class=38, eval_metric = "mlogloss")

bst.discreet <- xgboost(data = sparse_matrix, label = Y3, max.depth = 6, eta = .3, nround =10,
                  nthread = 2, objective = "multi:softmax", num_class=38)

# Predict -----------------------------------------------------------------
pred       <- predict(bst, test_sparse_matrix) #, type = "response", outputmargin=TRUE

predMatrix <- data.frame(matrix(pred, ncol=38, byrow=TRUE))
colnames(predMatrix) = unique(train$TripType)

res <- data.frame(id, predMatrix)
write.csv(res, 'submission.csv', quote = F, row.names = F)

