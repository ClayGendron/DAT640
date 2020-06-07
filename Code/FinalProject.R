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
    "AINBOED","ABYSTAND","CARAVAN")
#write.csv(coil2, file = here::here( "Data","FinalData","coiltest.csv"))

summary(coil)
skim(coil)

train <- coil
train <-  coil2

# correlation plots

cor_p <- cor(coil)
corrplot::corrplot(cor_p)

cor_zip_p <- cor(coil[,-c(44:85)])
corrplot::corrplot(cor_zip_p)

cor_products_p <- cor(coil[,-c(1:64)])
corrplot::corrplot(cor_products_p)

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


