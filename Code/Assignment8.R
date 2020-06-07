library(tidyverse)
library(tidymodels)
library(readr)
library(rattle)
library(here)
library(arules)
library(MASS)
library(rattle)   
library(magrittr)

# data("audit")
# write.csv(as_data_frame(audit),here::here("Data","audit.csv"))

rattle()

crv$seed <- 42 

fname         <- "file:///C:/Users/c.gendron1/Desktop/Project_Repository/DAT640/Data/audit.csv" 
crs$dataset <- read.csv(fname,
                        na.strings=c(".", "NA", "", "?"),
                        strip.white=TRUE, encoding="UTF-8")

#=======================================================================
# Rattle timestamp: 2020-06-01 00:41:26 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=2000 train=1400 validate=300 test=300

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

# The following variable selections have been noted.

crs$input     <- c("Age", "Employment", "Education", "Marital",
                   "Occupation", "Income", "Gender", "Deductions",
                   "Hours")

crs$numeric   <- c("Age", "Income", "Deductions", "Hours")

crs$categoric <- c("Employment", "Education", "Marital",
                   "Occupation", "Gender")

crs$target    <- "TARGET_Adjusted"
crs$risk      <- "RISK_Adjustment"
crs$ident     <- c("X", "ID")
crs$ignore    <- "IGNORE_Accounts"
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2020-06-01 00:41:35 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=2000 train=1400 validate=300 test=300

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

# The following variable selections have been noted.

crs$input     <- c("Age", "Employment", "Education", "Marital",
                   "Occupation", "Income", "Gender", "Deductions",
                   "Hours")

crs$numeric   <- c("Age", "Income", "Deductions", "Hours")

crs$categoric <- c("Employment", "Education", "Marital",
                   "Occupation", "Gender")

crs$target    <- "TARGET_Adjusted"
crs$risk      <- "RISK_Adjustment"
crs$ident     <- "ID"
crs$ignore    <- c("X", "IGNORE_Accounts")
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2020-06-01 00:41:44 x86_64-w64-mingw32 

# Build a Random Forest model using the traditional approach.

set.seed(crv$seed)

crs$rf <- randomForest::randomForest(as.factor(TARGET_Adjusted) ~ .,
                                     data=crs$dataset[crs$train, c(crs$input, crs$target)], 
                                     ntree=500,
                                     mtry=4,
                                     importance=TRUE,
                                     na.action=randomForest::na.roughfix,
                                     replace=FALSE)

# Generate textual output of the 'Random Forest' model.

crs$rf

# The `pROC' package implements various AUC functions.

# Calculate the Area Under the Curve (AUC).

pROC::roc(crs$rf$y, as.numeric(crs$rf$predicted), quiet=TRUE)

# Calculate the AUC Confidence Interval.

pROC::ci.auc(crs$rf$y, as.numeric(crs$rf$predicted), quiet=TRUE)FALSE

# List the importance of the variables.

rn <- round(randomForest::importance(crs$rf), 2)
rn[order(rn[,3], decreasing=TRUE),]

# Time taken: 1.35 secs

#=======================================================================
# Rattle timestamp: 2020-06-01 00:41:51 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# Risk Chart: requires the ggplot2 package.

library(ggplot2)

# Generate a risk chart.

# Rattle provides evaluateRisk() and riskchart().

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]),
                  type    = "prob")[,2]

crs$eval <- evaluateRisk(crs$pr, 
                         na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$TARGET_Adjusted, 
                         na.omit(crs$dataset[crs$validate, c(crs$input, crs$target, crs$risk)], crs$risk)$RISK_Adjustment)

riskchart(crs$pr,
          na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$TARGET_Adjusted, 
          na.omit(crs$dataset[crs$validate, c(crs$input, crs$target, crs$risk)], crs$risk)$RISK_Adjustment, 
          title="Risk Chart Random Forest audit.csv [validate] TARGET_Adjusted ", 
          risk.name="RISK_Adjustment", recall.name="TARGET_Adjusted",
          show.lift=TRUE, show.precision=TRUE, legend.horiz=FALSE) %>% print()

#=======================================================================

# Rattle is Copyright (c) 2006-2018 Togaware Pty Ltd.
# It is free (as in libre) open source software.
# It is licensed under the GNU General Public License,
# Version 2. Rattle comes with ABSOLUTELY NO WARRANTY.
# Rattle was written by Graham Williams with contributions
# from others as acknowledged in 'library(help=rattle)'.
# Visit https://rattle.togaware.com/ for details.

#=======================================================================
# Rattle timestamp: 2020-06-01 01:21:19 x86_64-w64-mingw32 

# Rattle version 5.3.0 user 'c.gendron1'

# This log captures interactions with Rattle as an R script. 

# For repeatability, export this activity log to a 
# file, like 'model.R' using the Export button or 
# through the Tools menu. Th script can then serve as a 
# starting point for developing your own scripts. 
# After xporting to a file called 'model.R', for exmample, 
# you can type into a new R Console the command 
# "source('model.R')" and so repeat all actions. Generally, 
# you will want to edit the file to suit your own needs. 
# You can also edit this log in place to record additional 
# information before exporting the script. 

# Note that saving/loading projects retains this log.

# We begin most scripts by loading the required packages.
# Here are some initial packages to load and others will be
# identified as we proceed through the script. When writing
# our own scripts we often collect together the library
# commands at the beginning of the script here.

library(rattle)   # Access the weather dataset and utilities.
library(magrittr) # Utilise %>% and %<>% pipeline operators.

# This log generally records the process of building a model. 
# However, with very little effort the log can also be used 
# to score a new dataset. The logical variable 'building' 
# is used to toggle between generating transformations, 
# when building a model and using the transformations, 
# when scoring a dataset.

building <- TRUE
scoring  <- ! building

# A pre-defined value is used to reset the random seed 
# so that results are repeatable.

crv$seed <- 42 

#=======================================================================
# Rattle timestamp: 2020-06-01 01:21:31 x86_64-w64-mingw32 

# Load a dataset from file.

fname         <- "file:///C:/Users/c.gendron1/Desktop/Project_Repository/Intro_Statistical_Learning/Data/Heart.csv" 
crs$dataset <- read.csv(fname,
                        na.strings=c(".", "NA", "", "?"),
                        strip.white=TRUE, encoding="UTF-8")

#=======================================================================
# Rattle timestamp: 2020-06-01 01:21:31 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=303 train=212 validate=45 test=46

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

# The following variable selections have been noted.

crs$input     <- c("Age", "Sex", "ChestPain", "RestBP", "Chol",
                   "Fbs", "RestECG", "MaxHR", "ExAng", "Oldpeak",
                   "Slope", "Ca", "Thal")

crs$numeric   <- c("Age", "Sex", "RestBP", "Chol", "Fbs",
                   "RestECG", "MaxHR", "ExAng", "Oldpeak", "Slope",
                   "Ca")

crs$categoric <- c("ChestPain", "Thal")

crs$target    <- "AHD"
crs$risk      <- NULL
crs$ident     <- "X"
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2020-06-01 01:21:46 x86_64-w64-mingw32 

# Build a Random Forest model using the traditional approach.

set.seed(crv$seed)

crs$rf <- randomForest::randomForest(AHD ~ .,
                                     data=crs$dataset[crs$train, c(crs$input, crs$target)], 
                                     ntree=500,
                                     mtry=4,
                                     importance=TRUE,
                                     na.action=randomForest::na.roughfix,
                                     replace=FALSE)

# Generate textual output of the 'Random Forest' model.

crs$rf

# The `pROC' package implements various AUC functions.

# Calculate the Area Under the Curve (AUC).

pROC::roc(crs$rf$y, as.numeric(crs$rf$predicted), quiet=TRUE)

# Calculate the AUC Confidence Interval.

pROC::ci.auc(crs$rf$y, as.numeric(crs$rf$predicted), quiet=TRUE)FALSE

# List the importance of the variables.

rn <- round(randomForest::importance(crs$rf), 2)
rn[order(rn[,3], decreasing=TRUE),]

# Time taken: 0.17 secs

#=======================================================================
# Rattle timestamp: 2020-06-01 01:21:50 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the rf model on Heart.csv [validate].

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]),
                  type    = "prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$AHD)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Random Forest Heart.csv [validate] AHD")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$AHD)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")


