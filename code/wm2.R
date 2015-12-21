test  <- fread("test.csv")
samp  <- fread("sample_submission.csv")
train <- fread("train.csv")

require(xgboost)
require(Matrix)
train$FinelineNumber        <- as.factor(train$FinelineNumber)
train$TripType              <- as.factor(train$TripType)
train$Upc                   <- as.factor(train$Upc)
train$Weekday               <- as.factor(train$Weekday)
train$DepartmentDescription <- as.factor(train$DepartmentDescription)

# one-hot encoding
id                <- train$VisitNumber
train$VisitNumber <- NULL

sparse_matrix     <- sparse.model.matrix(TripType~.-1, data = train)

output_vector      = train[,TripType] == "3"
output_vector      = train[,TripType] == "4"
output_vector      = train[,TripType] == "5"
output_vector      = train[,TripType] == "6"
output_vector      = train[,TripType] == "7"
output_vector      = train[,TripType] == "8"
output_vector      = train[,TripType] == "9"
output_vector      = train[,TripType] == "10"
output_vector      = train[,TripType] == "11"
output_vector      = train[,TripType] == "12"
output_vector      = train[,TripType] == "13"
output_vector      = train[,TripType] == "14"
output_vector      = train[,TripType] == "15"
output_vector      = train[,TripType] == "18"
output_vector      = train[,TripType] == "19"
output_vector      = train[,TripType] == "20"
output_vector      = train[,TripType] == "21"
output_vector      = train[,TripType] == "22"
output_vector      = train[,TripType] == "23"
output_vector      = train[,TripType] == "24"
output_vector      = train[,TripType] == "25"
output_vector      = train[,TripType] == "26"
output_vector      = train[,TripType] == "27"
output_vector      = train[,TripType] == "28"
output_vector      = train[,TripType] == "29"
output_vector      = train[,TripType] == "30"
output_vector      = train[,TripType] == "31"
output_vector      = train[,TripType] == "32"
output_vector      = train[,TripType] == "33"
output_vector      = train[,TripType] == "34"
output_vector      = train[,TripType] == "35"
output_vector      = train[,TripType] == "36"
output_vector      = train[,TripType] == "37"
output_vector      = train[,TripType] == "38"
output_vector      = train[,TripType] == "39"
output_vector      = train[,TripType] == "40"
output_vector      = train[,TripType] == "41"
output_vector      = train[,TripType] == "42"
output_vector      = train[,TripType] == "43"
output_vector      = train[,TripType] == "44"
output_vector      = train[,TripType] == "999"

datals <- as.list(sparse_matrix, output_vector)

bst      <- xgboost(data = sparse_matrix, label = output_vector, max.depth = 4, num_parallel_tree = 100, subsample = 0.5, colsample_bytree =0.5, nround = 1, objective = "binary:logistic")
