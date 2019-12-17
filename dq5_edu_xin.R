#import library 
library(tidyverse)
library(readxl)

#read the datasets
districts <- read_csv("data/districts.csv")

dis_to_county <- read_xls("data/data_district_to_county_crosswalk.xls") %>% 
  rename(system = `District Number`,
         county = `County Name`,
         county_number = `County Number`)

tvaas <- read_csv("data/tvaas.csv") %>%
  rename(system = `District Number`,
         `tvaas_composite` = `Composite`,
         `tvaas_literacy` = `Literacy`,
         `tvaas_numeracy` = `Numeracy`) %>%
  select(-`District Name`)

income <- read_csv("data/income.csv") %>% 
  rename(county_number  = county,
         median_income = Median_Income) %>%
  select(-NAME)

type_school <- read.csv("data/student_number.csv") %>% 
  mutate(public_school = male_public_school + female_public_school,
         private_school = male_private_school + female_private_school) %>%
  mutate(ratio_public_private = if_else(private_school == 0, 0, public_school / private_school)) %>% 
  select(county, ratio_public_private)
  
#merge the datasets
edu_tn <- districts %>%
  inner_join(tvaas, by = "system") %>%
  inner_join(dis_to_county, by = "system") %>%
  inner_join(income, by = "county_number") %>%
  inner_join(type_school, by = "county") %>%
  mutate(bhn = black + hispanic + native) %>% 
  select(-black,-hispanic,-native) %>% 
  filter(!is.na(grad))

#groupby county
by_county <- edu_tn %>%
  rowwise() %>%
  mutate(
    math_avg = mean(c(math, alg_1, alg_2), na.rm = TRUE),
    eng_avg = mean(c(ela, eng_1, eng_2, eng_3), na.rm = TRUE),
    sci_avg = mean(c(science, bio, chem), na.rm = TRUE)) %>% 
  select(county,math_avg,eng_avg,sci_avg,enrollment,el,swd,ed,expenditures,
         act_composite,chronic_abs,suspended,expelled,grad,dropout,region,
         tvaas_composite,median_income,ratio_public_private,bhn) %>% 
  group_by(county) %>% 
  summarise_if(is.numeric, mean) %>% 
  ungroup() 

by_county <- edu_tn %>%
  select(region, county) %>% 
  inner_join(by_county, by = "county") %>% distinct() 

#filling the missing values in by_county
#summary(by_county)

by_county <- by_county %>% 
  mutate(expenditures = if_else(is.na(expenditures), mean(expenditures, na.rm = TRUE), expenditures))

by_county <- by_county %>% 
  mutate(enrollment = if_else(is.na(enrollment), mean(enrollment, na.rm = TRUE), enrollment))

by_county <- by_county %>% 
  mutate(act_composite = if_else(is.na(act_composite), mean(act_composite, na.rm = TRUE), act_composite))

by_county <- by_county %>% 
  mutate(chronic_abs = if_else(is.na(chronic_abs), mean(chronic_abs, na.rm = TRUE), chronic_abs))

#EDA on the by_county


#the usual predictors
predictors <- c('region', 'bhn', 'math_avg', 'eng_avg', 'expenditures',
                'sci_avg', 'enrollment', 'el', 'swd', 'ed', 'act_composite',
                'chronic_abs', 'suspended', 'expelled', 'dropout',
                'tvaas_composite', 'median_income', 'ratio_public_private')

#slice down to only the predictor and response variables
graduation <- by_county %>%
  select(c(predictors, 'grad'))

#creat the training and test sets
library(caret)

set.seed(321)
index = createDataPartition(graduation$grad, p = 0.75, list = FALSE)

trainSet <- graduation[index,]
testSet <- graduation[-index,]

#train model to predict graduation rate
fitControl <- trainControl(
  method = "cv",
  number = 3)

rf_fit <- train(grad ~., data = trainSet, method = "ranger",
                trControl=fitControl, importance = 'impurity')

#performance on the training set
train_pred <- predict(rf_fit, newdata = trainSet)
MAE(pred = train_pred, obs = trainSet$grad)

#performance on the test set
test_pred <- predict(rf_fit, newdata = testSet)
MAE(pred = test_pred, obs = testSet$grad)

#seeing which were the most important features
rfImp <- varImp(rf_fit)
plot(rfImp)

#cor.test(edu_income$chronic_abs, edu_income$dropout, method = "pearson", use = "complete.obs")
