#Cleaning and Isolating the Household dataset down to variables that we want to use
#I am using the hosuehold data csv file in the most recent version
#You should import the dataset manually from your computer and then select and hit import dataset again and make sure it is in your environment



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
  select(newid, hsize1, village, migrant_any, wrkstat, age, sex, marital, happy, happy_future, ownhouse, mo_wageinc1, mo_nonwageinc1, mo_wageinc2, mo_nonwageinc2, happy_past) %>%
  mutate(hsize1 = as.numeric(hsize1)) %>%
  mutate(age = as.numeric(age)) %>%
  mutate(mo_wageinc1 = as.numeric(mo_wageinc1)) %>%
  mutate(mo_nonwageinc1 = as.numeric(mo_nonwageinc1)) %>%
  mutate(mo_wageinc2 = as.numeric(mo_wageinc2)) %>%
  mutate(mo_nonwageinc2 = as.numeric(mo_nonwageinc2)) %>%
  filter(happy != ".m") %>%
  filter(happy_past != ".m") %>%
  filter(happy_future != ".m") %>%
  mutate(happy = as.factor(happy)) %>%
  mutate(happy = fct_relevel(happy, c("Not at all satisfied", "Somewhat unsatisfied", "Satisfied", "Quite satisfied", "Completely satisfied"))) %>%
  mutate(happy_past = as.factor(happy_past)) %>%
  mutate(happy_past = fct_relevel(happy_past, c("Worse", "Same", "Better"))) %>%
  mutate(happy_future = as.factor(happy_future)) %>%
  mutate(happy_future = fct_relevel(happy_future, c("Worse", "Same", "Better")))


  
  
#graph 1: Happiness and income scatter plot divided by sex and faceted by village
  
data_1 %>%
  select(mo_wageinc1, mo_nonwageinc1, mo_wageinc2, mo_nonwageinc2, village, sex, happy) %>%
  na.omit() %>%
  mutate(total_monthly_income = mo_wageinc1 + mo_nonwageinc1 + mo_wageinc2 + mo_nonwageinc2) %>%
  ggplot(aes(x = total_monthly_income, y = happy, color = sex)) +
  geom_point() +
  scale_x_continuous(limits = c(0, 30000)) +
  facet_wrap(vars(village)) +
  theme_bw()
  
  
  
#graph 2: Happiness and age scatter plot divided by sex and faceted by village

data_1 %>%
  select(age, village, sex, happy) %>%
  na.omit() %>%
  ggplot(aes(x = age, y = happy, color = sex)) +
  geom_point() +
  facet_wrap(vars(village)) +
  theme_bw()

