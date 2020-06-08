library(tidyverse)
library(tidymodels)
library(rattle)
library(here)
library(skimr)
library(ggcorrplot)
library(reshape2)
library(grid)
library(caret)
library(klaR)
library(ROCR)

# data pull

coil <- read.delim(here::here("Data","FinalData","ticdata2000.txt"), header=FALSE)
colnames(coil) <- 
  c("MOSTYPE","MAANTHUI","MGEMOMV","MGEMLEEF","MOSHOOFD","MGODRK","MGODPR",
    "MGODOV","MGODGE","MRELGE","MRELSA","MRELOV","MFALLEEN","MFGEKIND",
    "MFWEKIND","MOPLHOOG","MOPLMIDD","MOPLLAAG","MBERHOOG","MBERZELF","MBERBOER",
    "MBERMIDD","MBERARBG","MBERARBO","MSKA","MSKB1","MSKB2","MSKC",
    "MSKD","MHHUUR","MHKOOP","MAUT1","MAUT2","MAUT0","MZFONDS",
    "MZPART","MINKM30","MINK3045","MINK4575","MINK7512","MINK123M",
    "MINKGEM","MKOOPKLA","PWAPART","PWABEDR","PWALAND","PPERSAUT","PBESAUT",
    "PMOTSCO","PVRAAUT","PAANHANG","PTRACTOR","PWERKT","PBROM","PLEVEN",
    "PPERSONG","PGEZONG","PWAOREG","PBRAND","PZEILPL","PPLEZIER","PFIETS",
    "PINBOED","PBYSTAND","AWAPART","AWABEDR","AWALAND","APERSAUT","ABESAUT",
    "AMOTSCO","AVRAAUT","AAANHANG","ATRACTOR","AWERKT","ABROM","ALEVEN",
    "APERSONG","AGEZONG","AWAOREG","ABRAND","AZEILPL","APLEZIER","AFIETS",
    "AINBOED","ABYSTAND","CARAVAN")
# write.csv(coil, file = here::here( "Data","FinalData","coil.csv"))

coil2 <- read.delim(here::here("Data","FinalData","ticeval2000.txt"), header=FALSE)
colnames(coil2) <- 
  c("MOSTYPE","MAANTHUI","MGEMOMV","MGEMLEEF","MOSHOOFD","MGODRK","MGODPR",
    "MGODOV","MGODGE","MRELGE","MRELSA","MRELOV","MFALLEEN","MFGEKIND",
    "MFWEKIND","MOPLHOOG","MOPLMIDD","MOPLLAAG","MBERHOOG","MBERZELF","MBERBOER",
    "MBERMIDD","MBERARBG","MBERARBO","MSKA","MSKB1","MSKB2","MSKC",
    "MSKD","MHHUUR","MHKOOP","MAUT1","MAUT2","MAUT0","MZFONDS",
    "MZPART","MINKM30","MINK3045","MINK4575","MINK7512","MINK123M",
    "MINKGEM","MKOOPKLA","PWAPART","PWABEDR","PWALAND","PPERSAUT","PBESAUT",
    "PMOTSCO","PVRAAUT","PAANHANG","PTRACTOR","PWERKT","PBROM","PLEVEN",
    "PPERSONG","PGEZONG","PWAOREG","PBRAND","PZEILPL","PPLEZIER","PFIETS",
    "PINBOED","PBYSTAND","AWAPART","AWABEDR","AWALAND","APERSAUT","ABESAUT",
    "AMOTSCO","AVRAAUT","AAANHANG","ATRACTOR","AWERKT","ABROM","ALEVEN",
    "APERSONG","AGEZONG","AWAOREG","ABRAND","AZEILPL","APLEZIER","AFIETS",
    "AINBOED","ABYSTAND")
# write.csv(coil2, file = here::here( "Data","FinalData","coiltest.csv"))

coil2caravan <- read.delim(here::here("Data","FinalData","tictgts2000.txt"), header=FALSE)
colnames(coil2caravan) <- c("CARAVAN")
# summarize data and understand distributions

summary(coil)
skim(coil)

train <- coil
test <- cbind(coil2,coil2caravan)

# correlation plots

cor_p <- cor(coil)
corrplot::corrplot(cor_p)

cor_zip_p <- cor(coil[,-c(44:85)])
corrplot::corrplot(cor_zip_p)

cor_products_p <- cor(coil[,-c(1:64)])
corrplot::corrplot(cor_products_p)

cor_dem_p <- cor(coil[-c(44:86)],coil$CARAVAN)

# other plots

plot_df <- coil %>% 
  group_by(MINKGEM) %>% 
  summarise(CARAVAN = mean(CARAVAN), POLICIES = n())

plot1 <- ggplot(plot_df, aes(x = MINKGEM)) + 
  geom_point(aes(y = CARAVAN))

plot2 <- ggplot(plot_df, aes(x = MINKGEM)) + 
  geom_line(aes(y = POLICIES))

grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2)))

# build model naive bayes

# remove non-variant predictors
mod_train <- train[,-c(50,53,71,74)]
colnames(mod_train) <- 
  c("MOSTYPE","MAANTHUI","MGEMOMV","MGEMLEEF","MOSHOOFD","MGODRK","MGODPR",
    "MGODOV","MGODGE","MRELGE","MRELSA","MRELOV","MFALLEEN","MFGEKIND",
    "MFWEKIND","MOPLHOOG","MOPLMIDD","MOPLLAAG","MBERHOOG","MBERZELF","MBERBOER",
    "MBERMIDD","MBERARBG","MBERARBO","MSKA","MSKB1","MSKB2","MSKC",
    "MSKD","MHHUUR","MHKOOP","MAUT1","MAUT2","MAUT0","MZFONDS",
    "MZPART","MINKM30","MINK3045","MINK4575","MINK7512","MINK123M",
    "MINKGEM","MKOOPKLA","PWAPART","PWABEDR","PWALAND","PPERSAUT","PBESAUT",
    "PMOTSCO","PAANHANG","PTRACTOR","PBROM","PLEVEN",
    "PPERSONG","PGEZONG","PWAOREG","PBRAND","PZEILPL","PPLEZIER","PFIETS",
    "PINBOED","PBYSTAND","AWAPART","AWABEDR","AWALAND","APERSAUT","ABESAUT",
    "AMOTSCO","AAANHANG","ATRACTOR","ABROM","ALEVEN",
    "APERSONG","AGEZONG","AWAOREG","ABRAND","AZEILPL","APLEZIER","AFIETS",
    "AINBOED","ABYSTAND","CARAVAN")

mod_test <- test[,-c(50,53,71,74)]
colnames(mod_test) <- 
  c("MOSTYPE","MAANTHUI","MGEMOMV","MGEMLEEF","MOSHOOFD","MGODRK","MGODPR",
    "MGODOV","MGODGE","MRELGE","MRELSA","MRELOV","MFALLEEN","MFGEKIND",
    "MFWEKIND","MOPLHOOG","MOPLMIDD","MOPLLAAG","MBERHOOG","MBERZELF","MBERBOER",
    "MBERMIDD","MBERARBG","MBERARBO","MSKA","MSKB1","MSKB2","MSKC",
    "MSKD","MHHUUR","MHKOOP","MAUT1","MAUT2","MAUT0","MZFONDS",
    "MZPART","MINKM30","MINK3045","MINK4575","MINK7512","MINK123M",
    "MINKGEM","MKOOPKLA","PWAPART","PWABEDR","PWALAND","PPERSAUT","PBESAUT",
    "PMOTSCO","PAANHANG","PTRACTOR","PBROM","PLEVEN",
    "PPERSONG","PGEZONG","PWAOREG","PBRAND","PZEILPL","PPLEZIER","PFIETS",
    "PINBOED","PBYSTAND","AWAPART","AWABEDR","AWALAND","APERSAUT","ABESAUT",
    "AMOTSCO","AAANHANG","ATRACTOR","ABROM","ALEVEN",
    "APERSONG","AGEZONG","AWAOREG","ABRAND","AZEILPL","APLEZIER","AFIETS",
    "AINBOED","ABYSTAND","CARAVAN")

mod_train$CARAVAN <- factor(mod_train$CARAVAN)
summary(mod_train)

# apply recipies

caravan_rec <- recipe(CARAVAN ~., data = mod_train) %>%
    step_normalize(all_numeric()) %>% 
    step_BoxCox(all_numeric())
train_rec <- prep(caravan_rec, training = mod_train)
nb_train <- bake(train_rec, new_data = mod_train)
nb_test <- bake(train_rec, new_data = mod_test)

# train model

nb_caravan_mod <- NaiveBayes(CARAVAN ~., data = nb_train)
train_p <- nb_caravan_mod %>% predict(nb_train)
test_p <- nb_caravan_mod %>% predict(nb_test)
mod_train_p <- cbind(nb_train,train_p)
mod_test_p <- cbind(nb_test,test_p)

# model assesment

cf_matrix <- table(mod_train_p$CARAVAN,mod_train_p$class)
caret::confusionMatrix(cf_matrix)

train_pred <- as.numeric(train_p$posterior[,c(2)])
train_lab <- as.numeric(nb_train$CARAVAN)
ROCRpred <- prediction(train_pred, train_lab)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")

plot(ROCRperf)

train_riskchart <- riskchart(train_pred,
                      nb_train$CARAVAN, 
                      title="Model Performance: Cross-Selling Caravan Insurance to Customers", 
                      recall.name="Targeted Cross-Selling", precision.name = "Strick Rate",
                      show.lift=TRUE, show.precision=TRUE, legend.horiz=FALSE, show.maximal = TRUE)
print(train_riskchart)

# assessing against the test data

cf_matrix_test <- table(mod_test_p$CARAVAN,mod_test_p$class)
caret::confusionMatrix(cf_matrix_test)

test_pred <- as.numeric(test_p$posterior[,c(2)])
test_lab <- as.numeric(nb_test$CARAVAN)

test_riskchart <- riskchart(test_pred,
                    nb_test$CARAVAN, 
                    title="(New Data) Model Performance: Cross-Selling Caravan Insurance to Customers", 
                    recall.name="Targeted Cross-Selling", precision.name = "Strick Rate",
                    show.lift=TRUE, show.precision=TRUE, legend.horiz=FALSE, show.maximal = TRUE)
print(test_riskchart)

