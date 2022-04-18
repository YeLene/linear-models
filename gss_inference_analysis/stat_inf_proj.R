library(ggplot2)
library(dplyr)
library(statsr)

getwd()
setwd("/Users/elenakocherova/Desktop/R+statistics/Duke University course Statistics/II Inferential Statistics")

load("gss.Rdata")

str(gss)
table(gss$region)

abor_age <- select(gss, year, abany, age, sex) %>%
  # Filter out NAs
  filter(is.na(abany) == FALSE, is.na(age) == FALSE, is.na(sex) == FALSE) %>%
  # Convert abany numerical to categorical
 # mutate(abany_factor = as.factor(abany)) %>%
  # Convert sex numerical to categorical
 # mutate(sex_factor = as.factor(sex)) %>%
  # Group sex
  group_by(sex)

abor_age

table(abor_age$year)

#for ALL years: ==============================================================================

#number of observations
nrow(abor_age)

#cutting age into age groups so that each group would contain close number of observations
abor_age["age_group"] = cut(abor_age$age, c(18, 25, 30, 35, 40, 45, 50, 60, 70, Inf), c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-59", "60-69", ">70"), include.lowest=TRUE)

table(abor_age$age_group)
table(abor_age$age_group,abor_age$abany)

#Таким образом, у нас 2 категориальные переменные, одна их которых имеет более 2 уровней - для анализа нужно использовать the Chi square independence test.

#Contingency table
cont_t <- table(abor_age$abany, abor_age$age_group)
cont_t_test <- table(abor_age$age_group, abor_age$abany)

#Research question: Does there appear to be a relationship between respondent's age group and respondent's opinion on abortions?

#H0: independent
#HA: dependent

#pchisq()

chisq <- chisq.test(cont_t)
chisq
chisq_test <- chisq.test(cont_t_test)
chisq_test #если contingency table транспонировать, результат теста не изменяется

chisq$observed
round(chisq$expected,2)
round(chisq$residuals, 3)

#Let’s visualize Pearson residuals using the package corrplot - shows the deviations from what would be expected based on sampling variation
install.packages("corrplot")
library(corrplot)
corrplot(chisq$residuals, is.cor = FALSE)

#1. Positive residuals are in blue. Positive values in cells specify an attraction (positive association) between the corresponding row and column variables.

# for 2012 year: ==================================================================

abor_age_2012 <- abor_age[abor_age$year == 2012,]
str(abor_age_2012)
abor_age_2012

#alternatively-------------------
gss %>%
  filter(year = 2012 &
           !is.na(abany) &
           !is.na(sex) &
           !is.na(age)) %>%
  select(abany,age)  -> gss_abany

dim(gss_abany)
#--------------------------------

#cutting age into age groups so that each group would contain close number of observations
abor_age_2012["age_group"] = cut(abor_age_2012$age, c(18, 25, 30, 35, 40, 45, 50, 55, 60, 70, Inf), c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-69", ">70"), include.lowest=TRUE)

table(abor_age_2012$age_group)
table(abor_age_2012$age_group,abor_age_2012$abany)

#Contingency table
cont_t_2012 <- table(abor_age_2012$abany, abor_age_2012$age_group)

cont_t_2012_m <- table( abor_age_2012$age_group, abor_age_2012$abany)
addmargins(cont_t_2012_m)

#proportion table - overall
round(prop.table(cont_t_2012)*100,2)
#proportion table - by row (by age group)
round(prop.table(cont_t_2012, margin = 1)*100,2)
#proportion table - by column (which age group gave the biggest contribution)
round(prop.table(cont_t_2012, margin = 2)*100,2)

ggplot(abor_age_2012) +
  aes(x = age_group, fill = abany) +
  geom_bar(position = "fill")
#According to the plot people in age group of 35-49 have the highest rate of Yes answers, which is not surprising as this "Sandwich Generation" usually carry the burden of looking after children and older relatives. The lowest rate of Yes answers was for people in age group >70. 

library("graphics")
mosaicplot(cont_t_2012_m, shade = TRUE, las=2,
           main = "age_group")

#Blue color indicates that the observed value is higher than the expected value if the data were random

#Red color specifies that the observed value is lower than the expected value if the data were random

#Let's check if this differences are statistically significant:
chisq_2012 <- chisq.test(cont_t_2012)
chisq_2012

chisq_2012$observed
round(chisq_2012$expected,2)
round(chisq_2012$residuals, 3)

corrplot(chisq_2012$residuals, is.cor = FALSE)
#For a given cell, the size of the circle is proportional to the amount of the cell contribution

## Contibution of each cell in percentage (%)
contrib <- 100*chisq_2012$residuals^2/chisq_2012$statistic
round(contrib, 3)

## Visualize the contribution
corrplot(contrib, is.cor = FALSE)

12.75+10.21+22.58+18.10

#The relative contribution of each cell to the total Chi-square score give some indication of the nature of the dependency between rows and columns of the contingency table

#From the image above, it can be seen that the most contributing cells to the Chi-square are 45-49/Yes (12.75%), 45-49/No (10.21%), >70/Yes (22.58%), >70/No (18.10%).

#These cells contribute about 63.64% to the total Chi-square score and thus account for most of the difference between expected and observed values.


#======= now we'll compare proportions for two sexes, performing hypothesis test ===============

table(abor_age_2012$sex)
cont_t_sex_2012 <- table(abor_age_2012$sex,abor_age_2012$abany)
cont_t_sex_2012
str(cont_t_sex_2012)

# H0: p_male - p_female = 0
# HA: p_male - p_female != 0

total_successes <- sum(cont_t_sex_2012[,1])
total_n <- sum(cont_t_sex_2012)
total_successes
total_n
nrow(abor_age_2012)

p_pool <- total_successes / total_n
p_pool

n_male <- sum(cont_t_sex_2012[1,])
n_female <- sum(cont_t_sex_2012[2,])
n_male
n_female

SE <- sqrt(p_pool * (1 - p_pool) * (1/n_male + 1/n_female))
SE

#Alternatively-----------------------------------
#gss_gender_self %>%
  # summarise(p_pool = sum(wrkslf=="Self-Employed")/n(),
  #           n_1 = sum(sex == "Female"),
  #           n_2 = sum(sex == "Male"),
  #           n_1_success = p_pool*n_1,
  #           n_1_fails = (1-p_pool)*n_1,
  #           n_2_success = p_pool*n_2,
  #           n_2_fails = (1-p_pool)*n_2,
  #           SE = sqrt((p_pool*(1 - p_pool)/n_1) + (p_pool*(1 - p_pool)/n_2)))
#-------------------------------------------------
# conditions:
# 1. independence - random sample and < 10% of population
# 2. for males and females: numbers of successes and failures are all > 10
# Hence p_male - p_female ~ N(mean = 0, SE = sqrt(p_pool * (1 - p_pool) * (1/n_male + 1/n_female)))

# hypothesis test if males and females are equally likely to answer Yes to the question about whether .....:

# point estimate p_hut_male - p_hut_female 
p_hut_male <- cont_t_sex_2012[1,1] / n_male
p_hut_female <- cont_t_sex_2012[2,1] / n_female
p_hut_male
p_hut_female

point_estimate <- p_hut_male - p_hut_female
Z <- (point_estimate - 0) / SE
Z
pnorm(Z)
#there is no sufficient evidence to reject H0

#let's construct the confidence interval for the difference between two proportions

SE_p_huts <- sqrt(p_hut_male * (1 - p_hut_male) / n_male + p_hut_female * (1 - p_hut_female)/ n_female)
Z_star <- 1.96 # for 95% conf int
SE_p_huts

point_estimate - Z_star * SE_p_huts
point_estimate + Z_star * SE_p_huts
#this conf interval contains 0 => we cannot reject H0

#alternatively - test:----------------
#One-Sided Independent Sample Proportion t--test

inference(y = abany, x = sex, data = abor_age_2012, statistic = "proportion", type = "ht", null = 0, success="Yes", alternative ="greater", method = "theoretical")

#Public opinion dynamics in Texas ================================

gss %>%
  filter(region == "W. Sou. Central" &
           !is.na(abany) &
           !is.na(sex) &
           !is.na(age)) %>%
  select(year,abany,age,sex)  -> abany_south

dim(abany_south)

#the timeline of public opinion change in Texas region
table(abany_south$year, abany_south$abany)

ggplot(data = abany_south, aes(x = year)) + geom_bar( aes(fill=abany), position = 'dodge')

plot(table(abany_south$year, abany_south$abany))

abany_south %>%
  group_by(year) %>%
  summarise(prop = sum(abany == "Yes")/n()) -> abany_prop

ggplot(data=abany_prop, aes(x=year,y=prop)) + geom_smooth()

# gss %>%
#   filter( !is.na(abany) &
#            !is.na(sex) &
#            !is.na(age)) %>%
#   select(abany,age,region) -> gss_region
# 
# table(gss_region$region)

#cutting age into age groups so that each group would contain close number of observations
abany_south["age_group"] = cut(abany_south$age, c(18, 25, 30, 35, 40, 45, 50, 55, 60, 70, Inf), c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-69", ">70"), include.lowest=TRUE)

table(abany_south$age_group,abany_south$year, abany_south$abany) #не выполнено условие np>10 и n(p-1)>10 - по возрасту не получится посмотреть расклад
 
#А если укрупнить возрастные группы? Пробуем

gss %>%
  filter(region == "W. Sou. Central" &
           !is.na(abany) &
           !is.na(sex) &
           !is.na(age)) %>%
  select(year,abany,age,sex)  -> abany_south_10yrs

abany_south_10yrs["age_group"] = cut(abany_south$age, c(18, 30, 40, 50, 60, 70, Inf), c("18-29", "30-39", "40-49", "50-59", "60-69", ">70"), include.lowest=TRUE)

table(abany_south_10yrs$age_group,abany_south_10yrs$year, abany_south_10yrs$abany) 
#не помогло

#==========================================================================================
# is there a correlation to respondents' education?

#DEGREE
#RS HIGHEST DEGREE
#If finished 9th-12th grade: Did you ever get a high school diploma or a GED certificate?
  #  VALUE	LABEL
# 0	LT HIGH SCHOOL
# 1	HIGH SCHOOL
# 2	JUNIOR COLLEGE
# 3	BACHELOR
# 4	GRADUATE

abor_edu <- select(gss, year, abany, degree, sex) %>%
  # Filter out NAs
  filter(is.na(abany) == FALSE, is.na(degree) == FALSE, is.na(sex) == FALSE) %>%
  # Convert abany numerical to categorical
  # mutate(abany_factor = as.factor(abany)) %>%
  # Convert sex numerical to categorical
  # mutate(sex_factor = as.factor(sex)) %>%
  # Group sex
  group_by(sex)

abor_edu

table(abor_edu$year)
table(abor_edu$degree)

#Таким образом, у нас опять 2 категориальные переменные, одна их которых имеет более 2 уровней - для анализа нужно использовать the Chi square independence test.

#Contingency table
cont_t_edu <- table(abor_edu$abany, abor_edu$degree)
cont_t_test_edu <- table(abor_edu$degree, abor_edu$degree)

cont_t_edu

#Research question: Does there appear to be a relationship between respondent's degree and respondent's opinion on abortions?

#H0: independent
#HA: dependent

#pchisq()

chisq_edu <- chisq.test(cont_t_edu)
chisq_edu

chisq_edu$observed
round(chisq_edu$expected,2)
round(chisq_edu$residuals, 3)

#Let’s visualize Pearson residuals using the package corrplot - shows the deviations from what would be expected based on sampling variation
corrplot(chisq_edu$residuals, is.cor = FALSE)

# for 2012 year: ==================================================================

abor_edu_2012 <- abor_edu[abor_edu$year == 2012,]
str(abor_edu_2012)
abor_edu_2012

table(abor_age_2012$age_group)
table(abor_age_2012$age_group,abor_age_2012$abany)

#Contingency table
cont_t_edu_2012 <- table(abor_edu_2012$abany, abor_edu_2012$degree)

cont_t_edu_2012_m <- table( abor_edu_2012$degree, abor_edu_2012$abany)
addmargins(cont_t_edu_2012_m)
cont_t_edu_2012_m

#proportion table - overall
round(prop.table(cont_t_edu_2012)*100,2)
#proportion table - by row (by degree)
round(prop.table(cont_t_edu_2012, margin = 1)*100,2)
#proportion table - by column (which degree group gave the biggest contribution)
round(prop.table(cont_t_edu_2012, margin = 2)*100,2)

mosaicplot(cont_t_edu_2012_m, shade = TRUE, las=2,
           main = "degree")

ggplot(abor_edu_2012) +
  aes(x = degree, fill = abany) +
  geom_bar(position = "fill")

#Blue color indicates that the observed value is higher than the expected value if the data were random

#Red color specifies that the observed value is lower than the expected value if the data were random

chisq_edu_2012 <- chisq.test(cont_t_edu_2012)
chisq_edu_2012

chisq_edu_2012$observed
round(chisq_edu_2012$expected,2)
round(chisq_edu_2012$residuals, 3)

corrplot(chisq_edu_2012$residuals, is.cor = FALSE)
#For a given cell, the size of the circle is proportional to the amount of the cell contribution

## Contibution of each cell in percentage (%)
contrib_edu <- 100*chisq_edu_2012$residuals^2/chisq_edu_2012$statistic
round(contrib_edu, 3)

## Visualize the contribution
corrplot(contrib_edu, is.cor = FALSE)
