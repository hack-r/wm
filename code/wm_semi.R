
# Packages ----------------------------------------------------------------
pacman::p_load(spa,upclass,EMCluster)

pacman::p_load(bit64, caret, data.table, dplyr, FactoMineR, glmulti, h2o,
               Metrics, mlbench, nnet, pls, rattle, sqldf, varSelRF)

# Options -----------------------------------------------------------------
setwd("T://RNA//Baltimore//Jason//ad_hoc//wm//data")

# Raw data import ---------------------------------------------------------
train <- fread("train.csv")
test  <- fread("test.csv")
samp  <- fread("sample_submission.csv")


# Reshape -----------------------------------------------------------------


# Spa ---------------------------------------------------------------------
y <- train$TripType
x <- train$DepartmentDescription
g <- train$

data("coraAI")
y <- coraAI$class
x <- coraAI$journals
g <- coraAI$cite

keep <- which(as.vector(apply(g, 1, sum) > 1))
setdiff(1:length(y), keep)

y <- y[keep]
x <- x[keep,]
g <- g[keep, keep]
set.seed(100)
n <- dim(x)[1]
Ns <- as.vector(apply(x, 2, sum))
Ls <- sapply(1:length(Ns), function(i) sample(which(x[,i] == 1),
                                               ceiling(0.035 * Ns[i])))
L <- NULL
for(i in 1:length(Ns)) L <- c(L, Ls[[i]])
U <- setdiff(1:n, L)
ord <- c(L, U)
m <- length(L)
y1 <- y
y1[U] <- NA

library(igraph)
train <- as.data.frame(train)
train$VisitNumber <- as.numeric(train$VisitNumber)
dat <- train[,colnames(train) %in% c("VisitNumber", "FinelineNumber")]
g <- get.adjacency(graph.edgelist(as.matrix(dat), directed=FALSE))
