# MODELLING AND PREDICTING STEPS
# the script is based on this post
# https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
library(tidyverse)
library(caret) # https://topepo.github.io/caret/index.html
library(randomForest)
library(sf)
rm(list = ls())

# Load data (soil clases and covariates)
set.seed(1)
data <- read_csv("input_data/soil_&_covariates.csv") %>% 
  na.omit() %>% # clean NAs values
  dplyr::select(z:wat_transtns) %>% # select columns
  dplyr::select(c(1, sample(2:234, size = 100))) # we remove some of the covariate because
# increasing covariates make the modelling step heavy, but this should be ran with all 
# covariates

# Setting for caret model
# for more information https://topepo.github.io/caret/using-your-own-model-in-train.html 
{customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
  customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2),
                                    label = c("mtry", "ntree"))
  customRF$grid <- function(x, y, len = NULL, search = "grid") {}
  customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
    randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
  }
  customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
    predict(modelFit, newdata)
  customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
    predict(modelFit, newdata, type = "prob")
  customRF$sort <- function(x) x[order(x[,1]),]
  customRF$levels <- function(x) x$classes}

control <- trainControl(method="repeatedcv", 
                        number=10, # 10 folds  
                        repeats=10,# repeat 10 times
                        verboseIter = TRUE,
                        classProbs = TRUE,
                        savePredictions = "all")

#### Optimisation of hyperparameters (ntree and mtry)
# set 3 values for each parameter (hyperparameters), such as c(7,10,13) for mtry and
# c(100, 300, 500) for mtree
tunegrid <- expand.grid(.mtry=c(10), .ntree=c(100)) 


#### set environment for spatial prediction
# raster for prediction
library(raster)
rasterOptions(tmpdir=paste0("/run/media/marcos/_home/marcos/tmp"),
              # set temp-file directory (it should existe already)
              progress= "text", 
              timer=TRUE, 
              chunksize=2e8, 
              maxmemory=1e8)

# load rasters
files <- list.files(path = "input_data/covs/", pattern = "tif", full.names = TRUE)
r <- stack(files)

# 
# How many samples has the minority class? (317 for this case)
table(data$z)
(minClass <- min(table(data$z)))

# x will collect the different predictions
x <- stack()
# accuracy is a dataframe to collect the accuracy statistics 
accuracy <- as.data.frame(matrix(nrow = 0,ncol = 3))
names(accuracy) <-  c("Accuracy", "Kappa", "Resample")
# run the process in parallel using raster::beginCluster
beginCluster()
for(i in 1:2){ # this should run as many times as possible (>50), so change to 1:50 or so
  # set random number generator
  set.seed(i) 
  # classify the data into 0 and 1
  d <- mutate(data, 
              bs = if_else(z==2, 1, z), 
              bs = as.factor(bs)) %>% 
    dplyr::select(-z) %>% 
    group_by(bs) %>% 
    # balance number of samples per class
    sample_n(minClass, replace = FALSE) 
    # dplyr::select(sample(1:233, size = 100, replace = FALSE))
  
  # convert 0 and 1 to names
  d$bs <- as.factor(make.names(d$bs))
  
  # train the model
  set.seed(i)
  #### this is the step for calibrating the model
  custom <- train(bs~., data = d, method=customRF, metric="Kappa", 
                  tuneGrid=tunegrid, trControl=control)
  # save accuracy statistics
  accuracy <- rbind(accuracy, custom$resample)
  #### in this step we use the model for spatial prediction
  prob <- clusterR(r, fun = predict,  args=list(model=custom$finalModel, type="prob"))
  names(prob) <- paste0("layer_",i)
  # save the prediction in a stack of rasters
  x <- stack(x,prob)
} 
# close cluster for paralleliztion
endCluster()
# estimate mean and sd of statistics
accuracy %>% 
  summarise(accuracy = mean(Accuracy),
            kappa = mean(Kappa),
            sd_acc = sd(Accuracy),
            sd_k = sd(Kappa))

###### Visualization of results
# plot all predictions
plot(1-x)
# plot predictions in two classes (0 and 1)
plot(round(1-x,0))
# estimate mean of predictions and plot
mx <- overlay(x, fun = mean)
plot(1-mx)
# estimate standard deviation and plot
sx <- overlay(x, fun = sd)
plot(sx)
# plot final map of BS
plot(round(1-mx,0))

# Save the resulting maps in the disk
writeRaster(1-mx, "output_data/mean_bs_probability.tif")
writeRaster(sx, "output_data/sd_bs_probability.tif")
writeRaster(x, "output_data/predictions.tif")



