#import library 
library(tidyverse)
library(caret)
library(readxl)
library(tigris)
library(sf)
library(fastDummies)
library(beeswarm)
library(GGally)
library(RColorBrewer)

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
         private_school = male_private_school + female_private_school,
         male = male_public_school + male_private_school,
         female = female_public_school + female_private_school) %>%
  mutate(ratio_public_private = if_else(private_school == 0, 0, public_school / private_school),
         ratio_male_female = male / female) %>% 
  select(county, ratio_public_private, ratio_male_female)
  
#merge the datasets
edu_tn <- districts %>%
  inner_join(tvaas, by = "system") %>%
  inner_join(dis_to_county, by = "system") %>%
  inner_join(income, by = "county_number") %>%
  inner_join(type_school, by = "county") %>%
  mutate(bhn = black + hispanic + native) %>% 
  select(-black,-hispanic,-native) %>% 
  filter(!is.na(grad))

#groupby the data by county
by_county <- edu_tn %>%
  rowwise() %>%
  mutate(
    math_avg = mean(c(math, alg_1, alg_2), na.rm = TRUE),
    eng_avg = mean(c(ela, eng_1, eng_2, eng_3), na.rm = TRUE),
    sci_avg = mean(c(science, bio, chem), na.rm = TRUE)) %>% 
  select(county,math_avg,eng_avg,sci_avg,enrollment,el, swd,ed,expenditures,
         act_composite,chronic_abs,suspended,expelled,grad,dropout,region,
         tvaas_composite,median_income,ratio_public_private,ratio_male_female,bhn) %>% 
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
#the choropleth for graduation rate by county  
tn_shape <- st_read("data/TN_counties.shp") %>% 
  rename(county = NAMELSAD) %>% 
  geo_join(by_county, 'county', 'county', how = 'inner') %>% 
  select(grad, geometry) 

my_colors <- brewer.pal(9, "Reds") 
my_colors <- colorRampPalette(my_colors)(30)

# Attribute the appropriate color to each country
class_of_county <- cut(tn_shape$grad, 30)
my_colors <- my_colors[as.numeric(class_of_county)]

plot(tn_shape, col=my_colors) 
title("Graduation rate of the high schools in Tennessee ")

#the overall distribution of graduation rate
#option 1:
ggplot(by_county, aes(x = grad)) + 
  geom_histogram(breaks=seq(70, 100 ,5), 
                 col="black",
                 fill="skyblue3",
                 alpha = .8) + 
  labs(title="Histogram for Graduation", x="Graduation rate", y="Count") + 
  xlim(c(70,100)) 

#option 2:
hist(by_county$grad, 
     main="Histogram for Graduation", 
     xlab="Graduation rate", 
     border="black", 
     col="skyblue3",
     breaks=seq(75, 100 ,5))

#graduation rate by region
#library('ggbeeswarm')
#ggplot2::ggplot(by_county,aes(region, grad)) + geom_quasirandom()

beeswarm(grad ~ region, data = by_county, pch =16,
         col = rainbow(8), ylab = "graduation rate",
         main = 'Graduation rate by region')


#the usual predictors
predictors <- c('region', 'bhn', 'math_avg', 'eng_avg', 'expenditures',
                'sci_avg', 'enrollment', 'el', 'swd', 'ed', 'act_composite',
                'chronic_abs', 'suspended', 'expelled', 'tvaas_composite', 
                'median_income', 'ratio_public_private', 'ratio_male_female')

#slice down to only the predictor and response variables
graduation <- by_county %>%
  select(c(predictors, 'grad'))

ggcorr(graduation, method = c("everything", "pearson")) + 
  labs(title="Correlation matrix of predictors and the response variable")

#graduation <- by_county %>%
#  select(c(predictors, 'grad')) %>% 
#  dummy_cols(select_columns = 'region', remove_first_dummy = TRUE) %>% select(-region)
  

#creat the training and test sets
set.seed(321)
index = createDataPartition(graduation$grad, p = 0.90, list = FALSE)

trainSet <- graduation[index,]
testSet <- graduation[-index,]

#train model to predict graduation rate
rf_fit <- train(grad ~., data = trainSet, method = "ranger",
                importance = 'impurity')

#performance on the training set
train_pred <- predict(rf_fit, newdata = trainSet)
MAE(pred = train_pred, obs = trainSet$grad)

#performance on the test set
test_pred <- predict(rf_fit, newdata = testSet)
MAE(pred = test_pred, obs = testSet$grad)

#seeing which were the most important features
rfImp <- varImp(rf_fit)
plot(rfImp)

cor.test(by_county$bhn, by_county$grad, method = "pearson", use = "complete.obs")



