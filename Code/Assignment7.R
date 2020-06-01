library(tidyverse)
library(tidymodels)
library(readr)
library(rattle)
library(here)
library(arules)
library(MASS)
library(rattle)   
library(magrittr)
library(rattle)   
library(magrittr) 

# data("Boston")
# write.csv(as_data_frame(Boston),here::here("Data","boston.csv"))

building <- TRUE
scoring  <- ! building
crv$seed <- 42 
fname         <- "file:///C:/Users/c.gendron1/Desktop/Project_Repository/DAT640/Data/boston.csv" 
crs$dataset <- read.csv(fname,
                        na.strings=c(".", "NA", "", "?"),
                        strip.white=TRUE, encoding="UTF-8")
set.seed(crv$seed)
crs$nobs <- nrow(crs$dataset)
crs$train <- sample(crs$nobs, 0.7*crs$nobs)
crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
  crs$validate
crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
  crs$test
crs$input     <- c("crim", "zn", "indus", "chas", "nox", "rm",
                   "age", "dis", "rad", "tax", "ptratio", "black",
                   "lstat")
crs$numeric   <- c("crim", "zn", "indus", "chas", "nox", "rm",
                   "age", "dis", "rad", "tax", "ptratio", "black",
                   "lstat")
crs$categoric <- NULL
crs$target    <- "medv"
crs$risk      <- NULL
crs$ident     <- "X"
crs$ignore    <- NULL
crs$weights   <- NULL
library(rpart, quietly=TRUE)
set.seed(crv$seed)
crs$rpart <- rpart(medv ~ .,
                   data=crs$dataset[crs$train, c(crs$input, crs$target)],
                   method="anova",
                   parms=list(split="information"),
                   control=rpart.control(usesurrogate=0, 
                                         maxsurrogate=0),
                   model=TRUE)
summary(crs$rpart)
print(crs$rpart)
printcp(crs$rpart)
cat("\n")
library(ggplot2)
crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)])
crs$eval <- evaluateRisk(crs$pr, crs$dataset[crs$validate, c(crs$input, crs$target)]$medv)
print(riskchart(crs$pr, 
                crs$dataset[crs$validate, c(crs$input, crs$target)]$medv, 
                title="Performance Chart Decision Tree boston.csv [validate] ", show.lift=FALSE, show.precision=FALSE, legend.horiz=FALSE))
set.seed(crv$seed)
crs$rf <- randomForest::randomForest(medv ~ .,
                                     data=crs$dataset[crs$train, c(crs$input, crs$target)], 
                                     ntree=500,
                                     mtry=3,
                                     importance=TRUE,
                                     na.action=randomForest::na.roughfix,
                                     replace=FALSE)
crs$rf
summary(crs$rf)
rn <- crs$rf %>%
  randomForest::importance() %>%
  round(2)
rn[order(rn[,1], decreasing=TRUE),]
p <- ggVarImp(crs$rf,
              title="Variable Importance Random Forest boston.csv")
p
plot(crs$rf, main="")
legend("topright", c(""), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest boston.csv",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
