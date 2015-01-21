library(parallel)
numCores <- detectCores()
numCores

testFunction = function(i) {
  summary(rnorm(1000000))
}

inputs = 1:20

system.time({
  results = mclapply(inputs, testFunction, mc.cores = 8)
})

system.time({
  results = lapply(inputs, testFunction)
})


# NOTE FOR WINDOWS USERS
# cl <- makeCluster(numCores)  
# results = parLapply(cl, inputs, testFunction)  
# stopCluster(cl)  


# overhead

system.time({
  results = mclapply(1:10000, sqrt, mc.cores = 8)
})

system.time({
  results = lapply(1:10000, sqrt)
})


#############
## FOREACH ##
#############

library(foreach)

# on windows
# library(doParallel)
# registerDoParallel(numCores)

# or for multiple machines
# library(snow)
# registerDoSNOW()

library(doMC)
registerDoMC(numCores)


x <- iris[which(iris[,5] != "setosa"), c(1,5)]
trials <- 10000
system.time({
    r <- foreach(icount(trials), .combine=cbind) %dopar% {
        ind <- sample(100, 100, replace=TRUE)
        result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
        coefficients(result1)
    }
})

system.time({
  r <- foreach(icount(trials), .combine=cbind) %do% {
    ind <- sample(100, 100, replace=TRUE)
    result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
    coefficients(result1)
  }
})

# gotcha: data copying

x = rep(0, times = 5)
for(i in 1:5) {
  x[i] = i * 2
}
x

x = rep(0, times = 5)
foreach(i = 1:5) %dopar% {
  x[i] = i * 2
}
x


# combine

mclapply(1:100, sqrt, mc.cores = 8)

foreach(x = 1:100) %dopar% {
  sqrt(x)
}

foreach(x = 1:100, .combine=c) %dopar% {
  sqrt(x)
}

# preschedule -- when will this be faster?

system.time({
  result <- foreach(x = 1:1000, .options.multicore=list(preschedule=FALSE)) %dopar% {
    sqrt(x)
  }
})

system.time({
  result <- foreach(x = 1:1000, .options.multicore=list(preschedule=TRUE)) %dopar% {
    sqrt(x)
  }
})

# when might prescheduling be slower?

deciseconds = sample(1:10, 50, replace = TRUE)
system.time({
  result <- foreach(x = deciseconds, .options.multicore=list(preschedule=FALSE)) %dopar% {
    Sys.sleep(x / 10)
  }
})

system.time({
  result <- foreach(x = deciseconds, .options.multicore=list(preschedule=TRUE)) %dopar% {
    Sys.sleep(x / 10)
  }
})



# back to slides


# random forests
wine <- read.csv( "winequality-red.csv", sep=';', header = TRUE ) 
head(wine)
y_dat = wine$quality
x_dat <- wine[,1:11]


library(randomForest)
num_trees = 500
system.time({
  randomForest(y = y_dat, x = x_dat, ntree = num_trees)
})


trees_per_core = floor(num_trees / numCores)
system.time({
  wine_model <- foreach(trees=rep(trees_per_core, numCores), .combine=combine, .multicombine=TRUE) %dopar% {
    randomForest(y = y_dat, x = x_dat, ntree = trees)
  }
})



#, .packages='randomForest'


# caret (Classification And REgression Training)

library(caret)
library(mlbench)
data(Sonar)

inTrain <- createDataPartition(y = Sonar$Class, p = .75, list = FALSE)
training <- Sonar[ inTrain,]
testing <- Sonar[-inTrain,]


ctrl <- trainControl(method = "repeatedcv", number = 8, repeats = 8)
grid_rf <- expand.grid(.mtry = c(2, 3, 4))
system.time({
  rf <- train(Class ~ ., data = training,  method = "rf", trControl = ctrl, ntree=750,  tuneGrid = grid_rf)
})

registerDoMC(1)
system.time({
  rf <- train(Class ~ ., data = training,  method = "rf", trControl = ctrl, ntree=750,  tuneGrid = grid_rf)
})


registerDoMC(numCores)