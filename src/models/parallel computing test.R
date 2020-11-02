# Trying to run random forest faster
library(caret)
library(randomForest)
library(doParallel)

# Without parallel computing
start.time <- proc.time()
set.seed(123)
model_1<- randomForest(price ~ ., 
                          train,
                          na.action = na.omit, # remove NAs
                          ntree=300)           # only create 300 trees / iterations
stop.time <- proc.time()
run.time1 <- stop.time - start.time
print(run.time)


# With parallel computing
cores <- detectCores()
cl<-makePSOCKcluster(cores)
registerDoParallel(cl)

start.time <- proc.time()
set.seed(123)
model_2 <- randomForest(price ~ ., 
                          train,
                          na.action = na.omit, # remove NAs
                          ntree=300)           # only create 300 trees / iterations
stop.time <- proc.time()
run.time2 <- stop.time - start.time
print(run.time2)

stopCluster(cl)

# With caret
start.time <- proc.time()
set.seed(123)
model_3 <- train(price ~ ., 
                 method = "rf",
                 data = train,
                 na.action = na.omit) # remove NAs
stop.time <- proc.time()
run.time3 <- stop.time - start.time
print(run.time3)


# With parallel computing and caret
cores <- detectCores()
cl<-makePSOCKcluster(cores)
registerDoParallel(cl)

start.time <- proc.time()
set.seed(123)
model_4 <- train(price ~ ., 
                 method = "rf",
                 data = train,
                 na.action = na.omit) # remove NAs

stop.time <- proc.time()
run.time4 <- stop.time - start.time
print(run.time4)

stopCluster(cl)

