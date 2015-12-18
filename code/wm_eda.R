barplot(table(train$Weekday))
barplot(table(train$TripType))
barplot(table(train$DepartmentDescription))

train$flag_40    <- 0
train$flag_39    <- 0
train$flag_39_40 <- 0

train$flag_40[train$TripType == "40"]                             <- 1
train$flag_39[train$TripType == "39"]                             <- 1
train$flag_39_40[train$TripType == "39" | train$TripType == "40"] <- 1

prop.table(table(train$flag_39_40))
prop.table(table(train$flag_39))
prop.table(table(train$flag_40))
prop.table(table(train$Weekday ))

tmp <- na.omit(train)
tmp <- sample_n(tmp, 1000)

prop.table(table(tmp$flag_39_40))

glmulti.lm.out <-
  glmulti("flag_39 ~ ScanCount + records + flag_weekend + sum_grocery + upc_flag + GROCERY.DRY.GOODS + log_records + sq_scans + sum_Personal_Care + sunday_return + return_flag + top_upc_flag + flag_fineline",
          family = "binomial", data = tmp,
          level = 2,               # No interaction considered
          method = "g",            #genetic # Exhaustive approach
          crit = "bic",            # AIC as criteria
          confsetsize = 2,         # Keep 5 best models
          plotty = F, report = F,  # No plot or interim reports
          fitfunction = "glm")      # lm function

## Show best models (Use @ instead of $ for an S4 object)
glmulti.lm.out@formulas


# Predict flag_39_40 ------------------------------------------------------

set.seed(3456)
train.samp <- sample_n(train, 10000)
trainIndex <- createDataPartition(train.samp$flag_39, p = .7,
                                  list = FALSE,
                                  times = 1)
flagTrain <- train.samp[ trainIndex,]
flagTest  <- train.samp[-trainIndex,]

flagTrain$sunday_flag    <- NULL
flagTrain$monday_flag    <- NULL
flagTrain$tuesday_flag   <- NULL
flagTrain$wednesday_flag <- NULL
flagTrain$thursday_flag  <- NULL
flagTrain$friday_flag    <- NULL
flagTrain$saturday_flag  <- NULL

flagTest$sunday_flag    <- NULL
flagTest$monday_flag    <- NULL
flagTest$tuesday_flag   <- NULL
flagTest$wednesday_flag <- NULL
flagTest$thursday_flag  <- NULL
flagTest$friday_flag    <- NULL
flagTest$saturday_flag  <- NULL

flagTrain$FinelineNumber <- as.factor(flagTrain$FinelineNumber)
flagTest$FinelineNumber  <- as.factor(flagTest$FinelineNumber)

summary(flag_model <- glm("flag_40 ~ I(FinelineNumber =='4138') +
                          I(DepartmentDescription == 'GROCERY DRY GOODS') +
                          saturday_flag + sunday_flag +
                          records + interaction_weekend_records",
                          data = flagTrain), family = "binomial", link = "logit")

flagTest  <- na.omit(flagTest)
flag_pred <- predict(flag_model, newdata = flagTest, type = "response")

flag_pred[flag_pred >=.5 ] <- 1
flag_pred[flag_pred < .5 ] <- 0

confusionMatrix(flag_pred, flagTest$flag_40)
cm <- confusionMatrix(flag_pred, flagTest$flag_40)
mean(cm$byClass)

saveRDS(flag_model, "flag40_model.RDS")

train$flag40_model <- predict(flag_model, newdata=train, type = "response")
test$flag40_model  <- predict(flag_model, newdata=test, type = "response")


# Flag 39 -----------------------------------------------------------------
set.seed(3456)
train.samp <- sample_n(train, 10000)
trainIndex <- createDataPartition(train.samp$flag_39, p = .7,
                                  list = FALSE,
                                  times = 1)
flagTrain <- train.samp[ trainIndex,]
flagTest  <- train.samp[-trainIndex,]

flagTrain$sunday_flag    <- NULL
flagTrain$monday_flag    <- NULL
flagTrain$tuesday_flag   <- NULL
flagTrain$wednesday_flag <- NULL
flagTrain$thursday_flag  <- NULL
flagTrain$friday_flag    <- NULL
flagTrain$saturday_flag  <- NULL

flagTest$sunday_flag    <- NULL
flagTest$monday_flag    <- NULL
flagTest$tuesday_flag   <- NULL
flagTest$wednesday_flag <- NULL
flagTest$thursday_flag  <- NULL
flagTest$friday_flag    <- NULL
flagTest$saturday_flag  <- NULL

flagTrain$FinelineNumber <- as.factor(flagTrain$FinelineNumber)
flagTest$FinelineNumber  <- as.factor(flagTest$FinelineNumber)

flag39_model <-
  glm("flag_39 ~ 1 + log_records + sunday_return + records:ScanCount +
      top_upc_flag +
      sq_scans", family = "binomial",
      data = flagTrain)

summary(flag39_model)

saveRDS(flag39_model, "flag39_model.RDS")

flag39_pred <- predict(flag39_model, newdata = train, type = "response")


# Latent ------------------------------------------------------------------
#fit <- princomp(flagTrain[,colnames(flagTrain) %in% c("upc_flag", "return_flag", "flag_weekend", "records", "flag_fineline", "ScanCount")], cor=TRUE)
fit <- princomp(flagTrain[,colnames(flagTrain) %in% c("upc_flag", "return_flag", "flag_weekend", "records", "flag_fineline", "ScanCount")], cor=TRUE)
summary(fit) # print variance accounted for
loadings(fit) # pc loadings
plot(fit,type="lines") # scree plot
fit$scores # the principal components
biplot(fit)

pacman::p_load(psych)
CN <- colnames(train)
for( i in CN){
  if (class(train[,i]) == "integer"){
    train[,i] <- as.numeric(train[,i])
  }
}

train$flag40_model <- na.roughfix(train$flag40_model)

nums <- sapply(train, is.numeric)
x    <- train[,nums]
x    <- na.omit(x)
fit <- principal(x[,!(colnames(x) %in% c("VisitNumber", "TripType"))], nfactors = 4, rotate = "varimax")
fit

test.pca <- predict(fit, newdata = test)

library(FactoMineR)
result <- PCA(train[,10000:15000]) # graphs generated automatically

# Misc --------------------------------------------------------------------
summary(train)
