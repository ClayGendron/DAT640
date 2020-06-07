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

#=======================================================================
# Rattle timestamp: 2020-06-01 01:30:15 x86_64-w64-mingw32 

# Load a dataset from file.

fname         <- "file:///C:/Users/c.gendron1/Desktop/Project_Repository/Intro_Statistical_Learning/Data/College.csv" 
crs$dataset <- read.csv(fname,
			na.strings=c(".", "NA", "", "?"),
			strip.white=TRUE, encoding="UTF-8")

#=======================================================================
# Rattle timestamp: 2020-06-01 01:30:16 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=777 train=544 validate=117 test=116

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

crs$input     <- c("Apps", "Accept", "Enroll", "Top10perc",
                   "Top25perc", "F.Undergrad", "P.Undergrad",
                   "Outstate", "Room.Board", "Books", "Personal",
                   "PhD", "Terminal", "S.F.Ratio", "perc.alumni",
                   "Expend", "Grad.Rate")

crs$numeric   <- c("Apps", "Accept", "Enroll", "Top10perc",
                   "Top25perc", "F.Undergrad", "P.Undergrad",
                   "Outstate", "Room.Board", "Books", "Personal",
                   "PhD", "Terminal", "S.F.Ratio", "perc.alumni",
                   "Expend", "Grad.Rate")

crs$categoric <- NULL

crs$target    <- "Private"
crs$risk      <- NULL
crs$ident     <- "X"
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2020-06-01 01:30:52 x86_64-w64-mingw32 

# Extreme Boost 

# The `xgboost' package implements the extreme gradient boost algorithm.

# Build the Extreme Boost model.

set.seed(crv$seed)

crs$ada <- xgboost(Private ~ .,
  data              = crs$dataset[crs$train,c(crs$input, crs$target)],
  max_depth         = 6,
  eta               = 0.3, 
  num_parallel_tree = 1, 
  nthread           = 2, 
  nround            = 50,
  metrics           = 'error',
  objective         = 'binary:logistic')

# Print the results of the modelling.

print(crs$ada)

cat('\nFinal iteration error rate:\n')
print(round(crs$ada$evaluation_log[crs$ada$niter, ], 2))

cat('\nImportance/Frequency of variables actually used:\n')
print(crs$imp <- importance(crs$ada, crs$dataset[crs$train,c(crs$input, crs$target)]))

# Time taken: 0.12 secs

#=======================================================================
# Rattle timestamp: 2020-06-01 01:30:59 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the xgb model on College.csv [validate].

crs$pr <- predict(crs$ada, crs$dataset[crs$validate, c(crs$input, crs$target)])

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]$Private)
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
p <- p + ggtitle("ROC Curve Extreme Boost College.csv [validate] Private")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]$Private)
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

#=======================================================================
# Rattle timestamp: 2020-06-01 01:31:19 x86_64-w64-mingw32 

# View the dataset. 

#=======================================================================
# Rattle timestamp: 2020-06-01 01:33:14 x86_64-w64-mingw32 

# Load a dataset from file.

fname         <- "file:///C:/Users/c.gendron1/Desktop/Project_Repository/Intro_Statistical_Learning/Data/Credit.csv" 
crs$dataset <- read.csv(fname,
			na.strings=c(".", "NA", "", "?"),
			strip.white=TRUE, encoding="UTF-8")

#=======================================================================
# Rattle timestamp: 2020-06-01 01:33:14 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=400 train=280 validate=60 test=60

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

crs$input     <- c("Income", "Limit", "Rating", "Age",
                   "Education", "Gender", "Student", "Married",
                   "Ethnicity", "Balance")

crs$numeric   <- c("Income", "Limit", "Rating", "Age",
                   "Education", "Balance")

crs$categoric <- c("Gender", "Student", "Married", "Ethnicity")

crs$target    <- "Cards"
crs$risk      <- NULL
crs$ident     <- "X"
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2020-06-01 01:33:51 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=400 train=280 validate=60 test=60

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

crs$input     <- c("Income", "Rating", "Cards", "Age",
                   "Education", "Gender", "Student", "Married",
                   "Ethnicity")

crs$numeric   <- c("Income", "Rating", "Cards", "Age",
                   "Education")

crs$categoric <- c("Gender", "Student", "Married", "Ethnicity")

crs$target    <- "Limit"
crs$risk      <- "Balance"
crs$ident     <- "X"
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2020-06-01 01:33:58 x86_64-w64-mingw32 

# Build a Random Forest model using the traditional approach.

set.seed(crv$seed)

crs$rf <- randomForest::randomForest(Limit ~ .,
  data=crs$dataset[crs$train, c(crs$input, crs$target)], 
  ntree=500,
  mtry=3,
  importance=TRUE,
  na.action=randomForest::na.roughfix,
  replace=FALSE)

# Generate textual output of the 'Random Forest' model.

crs$rf

# List the importance of the variables.

rn <- crs$rf %>%
    randomForest::importance() %>%
    round(2)
    rn[order(rn[,1], decreasing=TRUE),]

# Time taken: 0.17 secs

#=======================================================================
# Rattle timestamp: 2020-06-01 01:34:03 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# Risk Chart: requires the ggplot2 package.

library(ggplot2)

# Generate a risk chart.

# Rattle provides evaluateRisk() and riskchart().

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]))

crs$eval <- evaluateRisk(crs$pr, 
    na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$Limit, 
    na.omit(crs$dataset[crs$validate, c(crs$input, crs$target, crs$risk)], crs$risk)$Balance)

riskchart(crs$pr,
    na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$Limit, 
    na.omit(crs$dataset[crs$validate, c(crs$input, crs$target, crs$risk)], crs$risk)$Balance, 
    title="Risk Chart Random Forest Credit.csv [validate] Limit ", 
    risk.name="Balance", recall.name="Limit",
    show.lift=FALSE, show.precision=FALSE, legend.horiz=FALSE) %>% print()

