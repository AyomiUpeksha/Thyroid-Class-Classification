# Loading the required packages
library(tidyverse)
library(magrittr)
library(MLDataR)
library(skimr)
library(naniar)
library(writexl)
library(patchwork)
library(GGally)
library(corrplot)
library(ggstatsplot) # Required dplyr >= 1.1.3
#library(dplyr)
library(cowplot)
library(tidymodels)
library(caret)
library(yardstick) # For ROC/AUC curves
library(ranger) # random Forest
library(caret) # For Confusion Matrix
#library(randomForest) # To get importance variables
library(xgboost) # For XGBoost Algorithm
library(e1071) # Naive Bayes
library(kernlab) # SVM
library(cvms) # To Visualization of confusion Matrix
library(rpart)
library(ROSE) # Handeling inbalanced class

# (2) Loading the data
data("thyroid_disease")
df_1 <- data.frame(thyroid_disease)


glimpse(df_1)


# (3) converting the factor and numerical variables in right format
df_1$ThryroidClass <- as.factor(df_1$ThryroidClass)
df_1$patient_gender <- as.factor(df_1$patient_gender)
df_1$presc_thyroxine <- as.factor(df_1$presc_thyroxine)
df_1$queried_why_on_thyroxine <- as.factor(df_1$queried_why_on_thyroxine)
df_1$presc_anthyroid_meds <- as.factor(df_1$presc_anthyroid_meds)
df_1$presc_anthyroid_meds <- as.factor(df_1$presc_anthyroid_meds)
df_1$sick <- as.factor(df_1$sick)
df_1$pregnant <- as.factor(df_1$pregnant)
df_1$thyroid_surgery <- as.factor(df_1$thyroid_surgery)
df_1$radioactive_iodine_therapyI131 <- as.factor(df_1$radioactive_iodine_therapyI131)
df_1$query_hypothyroid <- as.factor(df_1$query_hypothyroid)
df_1$query_hyperthyroid <- as.factor(df_1$query_hyperthyroid)
df_1$lithium <- as.factor(df_1$lithium)
df_1$goitre <- as.factor(df_1$goitre)
df_1$tumor <- as.factor(df_1$tumor)
df_1$hypopituitarism <- as.factor(df_1$hypopituitarism)
df_1$psych_condition <- as.factor(df_1$psych_condition)
df_1$TSH_measured <- as.factor(df_1$TSH_measured)
df_1$T3_measured <- as.factor(df_1$T3_measured)
df_1$T4_measured <- as.factor(df_1$T4_measured)
df_1$thyrox_util_rate_T4U_measured <- as.factor(df_1$thyrox_util_rate_T4U_measured)
df_1$FTI_measured <- as.factor(df_1$FTI_measured)
df_1$ref_src <- as.factor(df_1$ref_src)

df_1$patient_age <- round(df_1$patient_age, digits = 0)

glimpse(df_1)

# (4) Data Processing 

# (4.1) Missing values are eliminated
vis_miss(df_1, warn_large_data = FALSE)

gg_miss_upset(df_1)

df_2 <- df_1 %>% filter(.,!is.na(TSH_reading)) #n = 3403
df_3 <- df_2 %>% filter(.,!is.na(T3_reading)) # n = 2910
df_4 <- df_3 %>% filter(.,!is.na(T4_reading)) # n=2902
df_5 <- df_4 %>% filter(.,!is.na(thyrox_util_rate_T4U_reading)) # n=2752
df_6 <- df_5 %>% filter(.,!is.na(FTI_reading)) # n =2752
df_7 <- df_6 %>% filter(.,!is.na(patient_age)) # n =2751

vis_miss(df_7, warn_large_data = FALSE)

# (4.2) Unimportant columns are eliminated

df_8 <- df_7 %>% select(.,c("ThryroidClass","patient_age","patient_gender",
                            "presc_thyroxine","queried_why_on_thyroxine",
                            "presc_anthyroid_meds","sick","pregnant",
                            "thyroid_surgery","radioactive_iodine_therapyI131",
                            "query_hypothyroid","query_hyperthyroid","lithium",
                            "goitre","tumor","hypopituitarism","psych_condition",
                            "TSH_reading","T3_reading","T4_reading",
                            "thyrox_util_rate_T4U_reading","FTI_reading","ref_src"))


# For now it does not remove outliers at this moment

# (4.4) Class balancing (handling inbalncing Problem)

p1 <- ggplot(df_8,aes(x=ThryroidClass)) + 
  geom_bar() + xlab("Patient Age")

df_8 %>% filter(ThryroidClass=='negative') %>% dim() # 2533 negative classes
df_8 %>% filter(ThryroidClass=='sick') %>% dim() # 218 negative classes

# 4.4.1 - Oversampling - Minority class oversampling without replacement
Oversampled_size <- 2*(df_8 %>% filter(ThryroidClass=='negative') %>% nrow())
oversampled_data <- ovun.sample(ThryroidClass~.,data = df_8, method = "over",N = Oversampled_size,
                                seed = 1)$data

table(oversampled_data$ThryroidClass) # Negative = 2533 & Sick = 2533

# 4.4.2 - Undersampling (without replacement)
undersampled_size <- 2*(df_8 %>% filter(ThryroidClass=='sick') %>% nrow())
undersampled_data <- ovun.sample(ThryroidClass~.,data = df_8, method = "under", N = undersampled_size,
                                 seed = 1)$data

table(undersampled_data$ThryroidClass) # negative = 218 & Sick = 218
# This is leaded to data reduction & loss significant information. Hence this mehod is not recommended

# 4.4.3 - Unersampling + Oversampling 
#minority class is oversampled with replacement and majority class is undersampled without replacing

balanced_both <- ovun.sample(ThryroidClass~.,data = df_8,method = "both", p = 0.5, 
                             N= 2600, seed =1)$data

table(balanced_both$ThryroidClass) # negative = 1346 & sick =  1254
# As specified before undersampling leads to the significant data loss. not recommended for this case

# 4.4.4 - ROSE sampling
balnced_rose <- ROSE(ThryroidClass~., data = df_8, seed = 1)$data
table(balnced_rose$ThryroidClass) # negative = 1434 & sick = 1317

# Conclusion - ROSE method & Oversample method is better to moving forward for this study










