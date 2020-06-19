library(dplyr)
library(caret)
library(parallel)
library(doParallel)

gc()
rm(list = ls())

cluster = makeCluster(detectCores()-1)
registerDoParallel(cluster)


trdata = data.table::fread("./pml-training.csv") %>%
    as_tibble %>% select(-c(V1:num_window)) %>%
    mutate(classe = factor(classe))

naPercent = sapply(trdata, function(col) mean(is.na(col)))

summary(naPercent) %>% print

mdnNApt = median(naPercent)

selectedCol = (naPercent <= 1-mdnNApt)

trdata = trdata %>%
    select_if(selectedCol)

tsdata = data.table::fread("./pml-testing.csv") %>%
    as_tibble %>% select(-c(V1:num_window)) %>%
    select_if(selectedCol)


set.seed(911)
inTrain = createDataPartition(trdata$classe, p = 3/4, list = FALSE)
training = trdata[ inTrain,]
validation = trdata[-inTrain,]
testing = tsdata

set.seed(911)

modelRF <- train(classe~., data = training, method = "rf", verbose = FALSE, 
             trControl = trainControl(method = "cv", number = 3,
             allowParallel = TRUE))

stopCluster(cluster)
registerDoSEQ()

predtr = predict(modelRF, training)
conftr = confusionMatrix(predtr, training$classe)
conftr %>% print


predval = predict(modelRF, validation)
confval = confusionMatrix(predval, validation$classe)
confval %>% print

predtest = predict(modelRF, testing)
predtest %>% print

