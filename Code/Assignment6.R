library(tidyverse)
library(tidymodels)
library(readr)
library(rattle)
library(here)
library(arules)

rattle()

# when scoring a dataset.

building <- TRUE
scoring  <- ! building

# A pre-defined value is used to reset the random seed 
# so that results are repeatable.

crv$seed <- 42 

library(arules, quietly=TRUE)

# Generate a transactions dataset.

crs$transactions <- as(split(crs$dataset[crs$train, crs$target],
                             crs$dataset[crs$train, crs$ident]),
                       "transactions")

# Generate the association rules.

crs$apriori <- apriori(crs$transactions, parameter = list(support=0.100, confidence=0.100, minlen=2))

# Summarise the resulting rule set.

generateAprioriSummary(crs$apriori)

# Time taken: 0.02 secs

# List rules.

inspect(sort(crs$apriori, by="support"))

# Interesting Measures.

interestMeasure(sort(crs$apriori, by="support"), c("chiSquare", "hyperLift", "hyperConfidence", "leverage", "oddsRatio", "phi"), crs$transactions)

# Apriori Code

dvdtrans <- read.csv(here::here('Data/dvdtrans.csv'))

dvdDS <- new.env()
dvdDS$data <- as(split(dvdtrans$Item, dvdtrans$ID), "transactions")

dvdDS$data

dvdAPRIORI <- new.env(parent = dvdDS)
evalq({
  model <- apriori(data, parameter = list(support = 0.2, confidence = 0.1))
}, dvdAPRIORI)

inspect(sort(dvdAPRIORI$model, by = "confidence")[1:5])
