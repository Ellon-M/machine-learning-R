# loading the needed packages----
library(dplyr)
library(ggplot2)
library(readr)
library(rpart)
library(caret)
library(rpart.plot)
library(caTools)
library(party)
library(plotly)
library(Amelia)
library(tidyr)
library(beeswarm)
library(randomForest)
library(e1071)
library(ROSE)

# data----
HR_data <- read.csv("data/HR_data.csv", na.strings=c(""))


head(HR_data)

glimpse(HR_data)

#data pre-processing----

colnames(HR_data)

#### dropping unneeded columns----
HR_data <- HR_data[,-c(1,2)]

colnames(HR_data)

### missing values----
colSums(is.na(HR_data))

round(colMeans(is.na(HR_data)) * 100,3)

missmap(HR_data, main = 'Missing Values', col = c('purple', 'black'))

# dropping rows for the columns with little missing data
HR_data <- HR_data %>% 
  filter(complete.cases(education_level,experience,last_new_job,enrolled_university))

round(colMeans(is.na(HR_data)) * 100,3)

# dealing with gender missing data
unique(HR_data$gender)

HR_data %>% 
  group_by(gender) %>% 
  summarise(count=n())

other <- which(HR_data$gender == "Other")

for (i in other) {
  HR_data$gender[i] <- "Male"
}    

unique(HR_data$gender)

complete <- HR_data %>% 
  filter(complete.cases(gender))

nrow(complete)/nrow(HR_data) * 100

HR_data <- complete


sum(is.na(HR_data$gender))

# dealing with major discipline missing values
unique(HR_data$major_discipline)

HR_data %>% 
  group_by(major_discipline) %>% 
  summarise(count=n())

HR_data$major_discipline = replace_na(HR_data$major_discipline,'STEM')

sum(is.na(HR_data$major_discipline))

# dealing with company size missing values
unique(HR_data$company_size)

HR_data$company_size = replace_na(HR_data$company_size,'Unknown')

sum(is.na(HR_data$company_size))

# dealing with company type missing values
unique(HR_data$company_type)

HR_data$company_type = replace_na(HR_data$company_type,'Other')

sum(is.na(HR_data$company_type))

colSums(is.na(HR_data))

### data types----
glimpse(HR_data)

unique(HR_data$gender)
HR_data$gender <- factor(HR_data$gender)

unique(HR_data$relevent_experience)
HR_data$relevent_experience <- factor(HR_data$relevent_experience)

unique(HR_data$enrolled_university)
HR_data$enrolled_university <- factor(HR_data$enrolled_university, levels = c("Full time course", "Part time course", "no_enrollment"))

unique(HR_data$education_level)
HR_data$education_level <- factor(HR_data$education_level, levels = c("Primary School", "High School", "Graduate", "Masters", "Phd"))

unique(HR_data$major_discipline)
HR_data$major_discipline <- factor(HR_data$major_discipline)

unique(HR_data$experience)
HR_data$experience <- factor(HR_data$experience)

unique(HR_data$company_size)
HR_data$company_size<- factor(HR_data$company_size, levels = c("<10","10-49","50-99","100-500","500-999","1000-4999","5000-9999","10000+","Unknown"))

unique(HR_data$company_type)
HR_data$company_type<- factor(HR_data$company_type)

unique(HR_data$last_new_job)
HR_data$last_new_job<- factor(HR_data$last_new_job, levels = c("never", "1", "2", "3", "4", ">4"))

unique(HR_data$target)
HR_data$target<- factor(HR_data$target)

glimpse(HR_data)

### Other----

# relevant experience
HR_data <- HR_data %>% 
        rename(
          relevant_experience = "relevent_experience"
        )

# enrolled university

HR_data$enrolled_university <- as.character(HR_data$enrolled_university)

enrolled <- which(HR_data$enrolled_university == "no_enrollment")

for (i in enrolled) {
  HR_data$enrolled_university[i] <- "no enrollment"
}

HR_data$enrolled_university <- as.factor(HR_data$enrolled_university)

# company size

HR_data$company_size <- as.character(HR_data$company_size)

limit <- which(HR_data$company_size == "10/49")

for (i in limit) {
  HR_data$company_size[i] <- "10-49"
}

HR_data$company_size <- as.factor(HR_data$company_size)

# Dependent variable analysis----

HR_data$target <- ifelse(HR_data$target == 1, "Job change", "No job change")

count_targ <- HR_data %>% 
  group_by(target) %>% 
     summarise(count=n())

count_targ

# looking for a job change
(3259/(3259+10892)) * 100

# not looking for a job change
(10892/(3259+10892)) * 100

ggplotly(ggplot(count_targ, aes(x=target, y=count, fill=target))+
  geom_col(alpha=0.5)+
    scale_fill_brewer(palette = "PuOr")+
  theme_dark())

# Independent variable analysis----

### City development index----

citydev <- HR_data %>% 
   summarise(max=max(city_development_index), min=min(city_development_index), median=median(city_development_index))

ggplot(HR_data, aes(x=city_development_index, fill=target))+
           geom_density(alpha=0.5)+
           labs(title = "RELATIONSHIP BETWEEN JOB CHANGE AND THE \nCITY DEVELOPMENT INDEX")+
           scale_fill_brewer(palette = "PuOr")+
           theme_dark()

ggplot(HR_data, aes(x=target, y=city_development_index, fill=target))+
           geom_boxplot(alpha=0.5)+
           scale_fill_brewer(palette = "PuOr")+
           theme_dark()
           

### Gender----
HR_data %>% 
  group_by(gender) %>% 
   summarise(count=n())

 HR_data %>%
  filter(target=="Job change") %>% 
  group_by(gender) %>%
  summarise(count=n())

# females looking for a job change
 (313/1206) * 100

# males looking for a job change
 (2946/12945) * 100

 ggplotly(ggplot(HR_data, aes(x=gender, fill=target))+
   geom_bar(position = "stack")+
     scale_fill_brewer(palette = "PuOr")+
     theme_dark())

 ### Relevant experience----
 
 HR_data %>% 
   group_by(relevant_experience) %>% 
   summarise(count=n())

 HR_data %>% 
   filter(target=="Job change") %>% 
   group_by(relevant_experience) %>%
   summarise(count=n())

 # those with relevant experience looking for a job change
 (2210/10664) * 100

 # those with no relevant experience looking for a job change
 (1149/3487) * 100
 
 
 ggplotly(ggplot(HR_data, aes(x=relevant_experience, fill=target))+
            geom_bar(position = "stack")+
            scale_fill_brewer(palette = "PuOr")+
            theme_dark())
 
 
 ### Enrolled university----
 
 HR_data %>% 
   group_by(enrolled_university) %>% 
   summarise(count=n())
 
 HR_data %>% 
   filter(target=="Job change") %>% 
   group_by(enrolled_university) %>%
   summarise(count=n())

 # full time course looking for a job change
 (928/2516) * 100
 
 # part time course looking for a job change
 (189/862) * 100
 
 #  no enrollment looking for a job change
 (2142/10773) * 100
 
 unique(HR_data$enrolled_university)
 
 ggplot(HR_data, aes(x=relevant_experience, fill=relevant_experience))+
    geom_bar()+
    scale_fill_brewer(palette = "PuOr")+
    labs(title=" A BAR PLOT SHOWING THE RELATIONSHIP \nBETWEEN RELEVANT EXPERIENCE AND JOB CHANGE")+
    theme_dark()
 
 
 ### education level ----
 
 HR_data %>% 
   group_by(education_level) %>% 
   summarise(count=n())
 
 HR_data %>% 
   filter(target=="Job change") %>% 
   group_by(education_level) %>%
   summarise(count=n())


 # primary level looking for a job change
 (21/206) * 100

 # High school level looking for a job change
 (271/1459) * 100
 
 # Graduates looking for a job change
 (2297/8889) * 100

 # masters level looking for a job change
 (627/3272) * 100

 # phd level looking for a job change
 (42/325) * 100

 
 
 
 ggplot(HR_data, aes(x=education_level, fill=target))+
            geom_bar(position = "stack")+
            labs(title = "EDUCATION LEVELS AND JOB CHANGE RELATIONSHIP")+
            scale_fill_brewer(palette = "PuOr")+
            theme_dark()
 
 ### major discipline----
 
 mjd_count <- HR_data %>% 
   group_by(major_discipline) %>% 
   summarise(count=n())
 
 mjd_change <- HR_data %>% 
   filter(target=="Job change") %>% 
   group_by(major_discipline) %>%
   summarise(count=n())

 (mjd_change$count/mjd_count$count) * 100 

### experience ----

 exper_count <- HR_data %>% 
   group_by(experience) %>% 
   summarise(count=n())
 
 exper_count<- arrange(exper_count, desc(count))
 
 
 HR_data %>% 
   group_by(experience) %>% 
   filter(experience == "<1" | experience ==">20") %>% 
   summarise(count=n())

 exper <-  HR_data %>% 
   filter(experience != "<1" & experience !=">20") 
 

 ggplot(exper, aes(x=as.numeric(experience), fill=target))+
           geom_density(alpha=0.5)+
            scale_fill_brewer(palette = "PuOr")+
            theme_dark()

 
 ### company size----
 
 comp_count <- HR_data %>% 
   group_by(company_size) %>% 
   summarise(count=n()) 
 
 comp_count
 
 ggplotly(ggplot(HR_data, aes(x=company_size, fill=company_size))+
            geom_bar(alpha=0.5)+
            scale_fill_brewer(palette = "PuOr")+
            theme_dark())
 
 comp_change <- HR_data %>% 
   filter(target=="Job change") %>% 
   group_by(company_size) %>%
   summarise(count=n())
 
 comp_change$change <- ""
 comp_change$change  <- (comp_change$count/comp_count$count) * 100 

 comp_change

 ggplotly(ggplot(comp_change, aes(x=company_size, y = change, fill=company_size))+
             geom_col(alpha=0.5)+
             scale_fill_brewer(palette = "PuOr")+
             theme_dark())

 
 ### company type---- 
 
 type_count <- HR_data %>% 
    group_by(company_type) %>% 
     summarise(count=n())

 type_count
 
 ggplotly(ggplot(HR_data, aes(x=company_type, fill=company_type))+
             geom_bar(alpha=0.5)+
             scale_fill_brewer(palette = "PuOr")+
             theme_dark())

 type_change <-   HR_data %>% 
    filter(target=="Job change") %>% 
    group_by(company_type) %>%
    summarise(count=n())
 
 type_change$change <- ""
 type_change$change  <- (type_change$count/type_count$count) * 100 

 type_change 
 
 ggplotly(ggplot(type_change, aes(x=company_type, y = change, fill=company_type))+
             geom_col(alpha=0.5)+
             scale_fill_brewer(palette = "PuOr")+
             theme_dark())
 
 
 ### last new job----
 last_job_count <- HR_data %>% 
    group_by(last_new_job) %>% 
     summarise(count=n())
 
 ggplotly(ggplot(HR_data, aes(x=last_new_job, fill=last_new_job))+
             geom_bar(alpha=0.5)+
             scale_fill_brewer(palette = "PuOr")+
             theme_dark()) 
 
 
 last_job_change <- HR_data %>% 
    filter(target=="Job change") %>% 
    group_by(last_new_job) %>%
    summarise(count=n())

 last_job_change$change <- ""
 last_job_change$change  <- (last_job_change$count/last_job_count$count) * 100 

 last_job_change 
 
 
 ggplotly(ggplot(last_job_change, aes(x=last_new_job, y = change, fill=last_new_job))+
             geom_col(alpha=0.5)+
             scale_fill_brewer(palette = "PuOr")+
             theme_dark())

 
 ### training hours----
 
 HR_data %>% 
     summarise(max=max(training_hours),median=median(training_hours),min=min(training_hours))

 ggplotly(ggplot(HR_data, aes(x=training_hours, fill=target))+
             geom_density(alpha=0.5)+
             scale_fill_brewer(palette = "PuOr")+
             theme_dark()) 
 
 
 # model building----
 
 ### logistic regression ----
 
 unique(HR_data$target)

 HR_data$target <- ifelse(HR_data$target == "Job change", 1, 0)

 # randomize
 random <- sample(1:(nrow(HR_data)))
HR_data <- HR_data[random,] 

# split
split <- c(1:(nrow(HR_data) * 0.7))

hr_train <- HR_data[split,]
hr_test <- HR_data[-split,]

glimpse(hr_train)

hr_train <- hr_train[,-11]


hr_log_fit <- glm(target~., data=hr_train, family="binomial")
summary(hr_log_fit)

# test

colnames(hr_test)
hr_test$predict_log <- ""
hr_test$predict_log <- predict(hr_log_fit, hr_test, type = "response")
hr_test$predict_log <- ifelse(hr_test$predict_log >= 0.5, "Job change", "No job change")
hr_test$target <- ifelse(hr_test$target == 1, "Job change", "No job change")



# confusion matrix
table(hr_test$target)

xtabs(~target+predict_log, data = hr_test)


### decision tree----
hr_train_tree <- hr_train

hr_train_tree$target <- ifelse(hr_train_tree$target == 1, "Job change", "No job change")

hr_tree_fit <- rpart(target~. , method = "class", data = hr_train_tree)
rpart.plot(hr_tree_fit)

hr_test$predict_tree <- ""
hr_test$predict_tree <- predict(hr_tree_fit, hr_test, type = "class")

# confusion matrix
table(hr_test$target)

xtabs(~target+predict_tree, data = hr_test)

# accuracy
((644+2809)/(984+3262)) * 100

# sensitivity
644/(644+340) * 100


### random forest----

hr_train_for <- hr_train

# oversampling - imbalanced data set

hr_train_for %>% 
   group_by(target) %>% 
    summarise(count=n())

#ROSE
balanced_over_for <- ovun.sample(target ~ ., data = hr_train_for, method = "over",N =15260)$data
 
table(balanced_over_for$target)

# randomize
random <- sample(1:(nrow(balanced_over_for)))
balanced_over_for <- balanced_over_for[random,]

balanced_over_for$target <- ifelse(balanced_over_for$target == 1, "Job change", "No job change")


balanced_over_for$target <- factor(balanced_over_for$target)

# tuning the number of variables randomly sampled as candidates at each split - mtry
mtry <- tuneRF(balanced_over_for[-11],balanced_over_for$target, ntreeTry=800,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)


best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

# model
hr_for_fit <- randomForest(target~., data = balanced_over_for, ntree = 800, mtry = 6, importance = TRUE, replace = TRUE)

hr_for_fit

# test
hr_test$predict_for <- ""
hr_test$predict_for <- predict(hr_for_fit, hr_test, type = "class")

xtabs(~target+predict_for, data = hr_test)



