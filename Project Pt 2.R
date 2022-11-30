#Cleaning and Isolating the Household dataset down to variables that we want to use
#I am using the hosuehold data csv file in the most recent version
#You should import the dataset manually from your computer and then select and hit import dataset again and make sure it is in your environment

library(readr)
library(tidyverse)
library(randomForest)

Household <- read_csv("Household.csv")

data <- Household


for (i in 1:nrow(data)){
  if (data$mo_wageinc1[i] == ".n"){
    data$mo_wageinc1[i] <- 0
  }
  
  if (data$mo_nonwageinc1[i] == ".n"){
    data$mo_nonwageinc1[i] <- 0
  }
  
  if (data$mo_wageinc2[i] == ".n"){
    data$mo_wageinc2[i] <- 0
  }
  
  if (data$mo_nonwageinc2[i] == ".n"){
    data$mo_nonwageinc2[i] <- 0
  }
}

data_1 <- data %>%
  select(newid, hsize1, village, migrant_any, wrkstat, age, sex, marital, happy, happy_future, ownhouse, mo_wageinc1, mo_nonwageinc1, mo_wageinc2, mo_nonwageinc2, happy_past, satcur_job, satcur_achieve, satcur_livstandard, satcur_health, satcur_safe, satcur_comm, satcur_finsec, satcur_leistime, satcur_socenv, satcur_fam) %>%
  mutate(hsize1 = as.numeric(hsize1)) %>%
  mutate(age = as.numeric(age)) %>%
  mutate(mo_wageinc1 = as.numeric(mo_wageinc1)) %>%
  mutate(mo_nonwageinc1 = as.numeric(mo_nonwageinc1)) %>%
  mutate(mo_wageinc2 = as.numeric(mo_wageinc2)) %>%
  mutate(mo_nonwageinc2 = as.numeric(mo_nonwageinc2)) %>%
  filter(happy != ".m") %>%
  filter(happy_past != ".m") %>%
  filter(happy_future != ".m") %>%
  filter(satcur_job != ".n") %>%
  filter(satcur_job != ".m" & satcur_achieve != ".m" & satcur_livstandard != ".m" & satcur_health != ".m" & satcur_safe != ".m" & satcur_comm != ".m" & satcur_finsec != ".m" & satcur_leistime != ".m" & satcur_socenv != ".m" & satcur_fam  != ".m") %>%
  mutate(across(c("happy", "satcur_job", "satcur_achieve", "satcur_livstandard", "satcur_health", "satcur_safe", "satcur_comm", "satcur_finsec", "satcur_leistime", "satcur_socenv", "satcur_fam"), as.factor)) %>%
  mutate(across(c("happy", "satcur_job", "satcur_achieve", "satcur_livstandard", "satcur_health", "satcur_safe", "satcur_comm", "satcur_finsec", "satcur_leistime", "satcur_socenv", "satcur_fam"), ~ fct_relevel(., c("Not at all satisfied", "Somewhat unsatisfied", "Satisfied", "Quite satisfied", "Completely satisfied")))) %>%
  mutate(happy_past = as.factor(happy_past)) %>%
  mutate(happy_past = fct_relevel(happy_past, c("Worse", "Same", "Better"))) %>%
  mutate(happy_future = as.factor(happy_future)) %>%
  mutate(happy_future = fct_relevel(happy_future, c("Worse", "Same", "Better"))) %>%
  mutate(happy_agg = as.numeric(satcur_job) + as.numeric(satcur_achieve) + as.numeric(satcur_livstandard) + as.numeric(satcur_health) + as.numeric(satcur_safe) + as.numeric(satcur_comm) + as.numeric(satcur_finsec) + as.numeric(satcur_leistime) + as.numeric(satcur_socenv) + as.numeric(satcur_fam) - 10) %>%
  mutate(class_2 = as.factor(happy_agg > 20))
  
View(data_1) 



max(data_1$happy_agg)


#graph 1: Happiness and income scatter plot divided by sex and faceted by village

data_1 %>%
  select(mo_wageinc1, mo_nonwageinc1, mo_wageinc2, mo_nonwageinc2, village, sex, happy_agg) %>%
  na.omit() %>%
  mutate(total_monthly_income = mo_wageinc1 + mo_nonwageinc1 + mo_wageinc2 + mo_nonwageinc2) %>%
  ggplot(aes(x = total_monthly_income, y = happy_agg, color = village)) +
  geom_point() +
  scale_x_continuous(limits = c(0, 30000)) +
  scale_y_continuous(limits = c(0, 40)) +
  theme_bw() +
  geom_smooth(method = lm, se = F)



#graph 2: Happiness and age scatter plot divided by sex and faceted by village

data_1 %>%
  select(age, village, sex, happy_agg) %>%
  na.omit() %>%
  ggplot(aes(x = age, y = happy_agg, color = sex)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = lm, se = F)

#plot 3: Random Forest var importance

rf_data <- data_1 %>%
  mutate(total_monthly_income = mo_wageinc1 + mo_nonwageinc1 + mo_wageinc2 + mo_nonwageinc2) %>%
  select(hsize1, village, migrant_any, wrkstat, age, sex, ownhouse, class_2, total_monthly_income) %>%
  na.omit() 

set.seed(346587)
split_vec <- sample(x = nrow(rf_data), size = round(.8*nrow(rf_data)), replace = FALSE)


rf_data_train <- rf_data %>%
  slice(split_vec)

rf_data_test <- rf_data %>%
  anti_join(rf_data_train)
  
rf_x_data_train <- rf_data_train %>%
  select(hsize1, village, migrant_any, wrkstat, age, sex, ownhouse, total_monthly_income)

rf_y_data_train <- rf_data_train$class_2
  
rf_x_data_test <- rf_data_test %>%
  select(hsize1, village, migrant_any, wrkstat, age, sex, ownhouse, total_monthly_income)
  
rf_y_data_test <- rf_data_test$class_2

rf1 <- randomForest(x = rf_x_data_train, y = rf_y_data_train, xtest = rf_x_data_test, ytest = rf_y_data_test, keep.forest=TRUE) 

varImpPlot(rf1, main = "Random Forest Variable Importance")


#graph 4: work status effect on happiness by gender
data_1 %>%
  select(wrkstat, happy_agg, sex) %>%
  filter(wrkstat!=".m") %>%
  na.omit() %>%
  ggplot(aes(x = wrkstat, y= happy_agg, color=sex))+
  geom_boxplot() +
  theme_bw() +
  geom_smooth(method = lm, se = F)
