require(xgboost)
require(methods)


x = rbind(train[,-ncol(train)],test)
x = as.matrix(x)
x = matrix(as.numeric(x),nrow(x),ncol(x))
trind = 1:length(y)
teind = (nrow(train)+1):nrow(x)

# Set necessary parameter
param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = 38,
              "nthread" = 80)

# Run Cross Valication
cv.nround = 50
bst.cv = xgb.cv(param=param, data = sparse_matrix, label = Y3,
                nfold = 3, nrounds=cv.nround)

# Train the model
nround = 50
bst = xgboost(param=param, data = sparse_matrix, label = Y3, nrounds=nround)

# Make prediction
pred = predict(bst,x[teind,])
pred = matrix(pred,9,length(pred)/9)
pred = t(pred)

# Output submission
pred = format(pred, digits=2,scientific=F) # shrink the size of submission
pred = data.frame(1:nrow(pred),pred)
names(pred) = c('id', paste0('Class_',1:9))
write.csv(pred,file='submission.csv', quote=FALSE,row.names=FALSE)
