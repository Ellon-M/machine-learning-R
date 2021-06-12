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
library(tidyr)
library(ROSE)
library(e1071)
library(randomForest)

# data----
glimpse(undergrad_dropout)

# data changes----

### missing values----
colSums(is.na(undergrad_dropout))

### changing data types----
undergrad_dropout$gender <- factor(undergrad_dropout$gender)
undergrad_dropout$economic_level <- factor(undergrad_dropout$economic_level)
undergrad_dropout$scholarship <- factor(undergrad_dropout$scholarship)
undergrad_dropout$payment_delay <- factor(undergrad_dropout$payment_delay)
undergrad_dropout$career_change <- factor(undergrad_dropout$career_change)
undergrad_dropout$dropout <- factor(undergrad_dropout$dropout)

glimpse(undergrad_dropout)

# response/dependant variable analysis----
dropout_count <- undergrad_dropout %>% 
  group_by(dropout) %>% 
   summarise(count=n())

ggplot(dropout_count, aes(x=dropout, y=count, fill=dropout))+
  geom_col()+
  theme_dark()

#(inference)



# predictor variable analysis----

### Age----

#(descriptive analysis)

# visual
ggplot(undergrad_dropout, aes(x=age, fill=dropout))+
  geom_density()+
  theme_dark()


#(inference)

### Gender----

#(descriptive analysis)

# visual
ggplot(undergrad_dropout, aes(x=gender, fill=dropout))+
  geom_bar(position = "stack")+
  theme_dark()


#(inference)



### entrance grade----

#(descriptive analysis)


# visual
ggplot(undergrad_dropout, aes(x=entrance_grade, fill=dropout))+
  geom_density()+
  theme_dark()

#(inference)


### final high school grade----

#(descriptive analysis)

# visual
ggplot(undergrad_dropout, aes(x=final_hs_grade, fill=dropout))+
  geom_density()+
  theme_dark()

#(descriptive analysis)



### economic level----

# descriptive
econ_dropout <- undergrad_dropout %>% 
                  group_by(economic_level) %>% 
                  filter(dropout == 1) %>% 
                   summarise(count=n())

econ_dropout <- econ_dropout %>% 
  rename(
   total ="total"
  )

econ_count <- undergrad_dropout %>% 
                  group_by(economic_level) %>% 
                  summarise(count=n())

econ_dropout$total <- ""
econ_dropout$total <- econ_count$count
econ_dropout$ratio <- ""
econ_dropout$ratio <- round((econ_dropout$dropout/econ_dropout$total) * 100,2)

# visual
ggplot(econ_dropout, aes(x=economic_level, y=ratio, fill=economic_level))+
  geom_col()+
  theme_dark()
  

### percentage absent----

# (descriptive analysis)

#sem 1

# visual
ggplot(undergrad_dropout, aes(x=perc_absent_1, fill=dropout))+
  geom_density(alpha=0.4)+
  theme_minimal()

#sem 2

# visual
ggplot(undergrad_dropout, aes(x=perc_absent_2, fill=dropout))+
  geom_density(alpha=0.4)+
  theme_minimal()

#(inferences)

### average grade----

# (descriptive analysis)

# sem 1

# visuals
ggplotly(ggplot(undergrad_dropout, aes(x=dropout, y=average_grade_1, fill=dropout))+
  geom_boxplot()+
  theme_dark())

ggplot(undergrad_dropout, aes(x=average_grade_1, fill=dropout))+
  geom_density(alpha=0.4)+
  theme_minimal()

# (inferences)

# sem 2

# visuals
ggplotly(ggplot(undergrad_dropout, aes(x=dropout, y=average_grade_2, fill=dropout))+
  geom_boxplot()+
  theme_dark())

ggplot(undergrad_dropout, aes(x=average_grade_2, fill=dropout))+
  geom_density(alpha=0.4)+
  theme_minimal()

# (inferences)



### library usage ----

#(descriptive analysis)

# sem 1

# visuals
ggplotly(ggplot(undergrad_dropout, aes(x=dropout, y=library_usage_1, fill=dropout))+
           geom_boxplot()+
           theme_dark())

ggplot(undergrad_dropout, aes(x=library_usage_1, fill=dropout))+
  geom_density(alpha=0.4)+
  theme_minimal()

#(inferences)

# sem 2

# visuals
ggplotly(ggplot(undergrad_dropout, aes(x=dropout, y=library_usage_2, fill=dropout))+
           geom_boxplot()+
           theme_dark())

ggplot(undergrad_dropout, aes(x=library_usage_2, fill=dropout))+
  geom_density(alpha=0.4)+
  theme_minimal()

#(inferences)



### scholarship ----

#(descriptive analysis)

# visual
ggplot(undergrad_dropout, aes(x=scholarship, fill=dropout))+
  geom_bar(position = "stack")+
  theme_bw()

#(inferneces)

### payment delay ----

# (descriptive analysis)

# visual
ggplot(undergrad_dropout, aes(x=payment_delay, fill=dropout))+
  geom_bar(position = "stack")+
  theme_bw()

#(inferences)

### career change----

#(descriptive analysis)

# visual
ggplot(undergrad_dropout, aes(x=career_change, fill=dropout))+
  geom_bar(position = "stack")+
  theme_bw()

#(inferences)





# model ----

# randomize
random <- sample(1:(nrow(undergrad_dropout)))
undergrad_dropout <- undergrad_dropout[random,]

# split
split <- c(1: (nrow(undergrad_dropout) * 0.7))

dropout_train <- undergrad_dropout[split,]
dropout_test <- undergrad_dropout[-split,]

### logistic----

glimpse(dropout_train)

# drop

dropout_train <- dropout_train[,-c(1,2,5,15)]

colnames(dropout_train)

# fit model
dropout_log_fit <- glm(dropout~., data = dropout_train, family = "binomial")
summary(dropout_log_fit)

# test on test data
dropout_test$predict_log <- ""
dropout_test$predict_log <- predict(dropout_log_fit, dropout_test, type = "response")

dropout_test$predict_log <- ifelse(dropout_test$predict_log >= 0.5, "Dropout", "No dropout")
dropout_test$dropout <- ifelse(dropout_test$dropout == 1, "Dropout", "No dropout")

table(dropout_test$dropout)
xtabs(~dropout+predict_log, data = dropout_test)


#(inferences)




### decision tree ----
dropout_train_tree <- dropout_train

dropout_train$dropout <- factor(ifelse(dropout_train$dropout == 1, "Dropout", "No dropout"), levels = c("Dropout", "No dropout"))

dropout_fit <- rpart(dropout ~ . , method = "class", data = dropout_train_tree)

rpart.plot(dropout_fit)

dropout_test$predict_tree <- ""
dropout_test$predict_tree <- predict(dropout_fit, dropout_test, type = "class")

dropout_test$predict_tree <- ifelse(dropout_test$predict_tree == 1, "Dropout", "No dropout")

table(dropout_test$predict_tree)

xtabs(~dropout+predict_tree, data = dropout_test)

((67+190)/(74+ 199)) * 100


#(inferences)

###


# prun the tree- reduce overfitting - pre prunning

dropout_fit_preprun <- rpart(dropout ~ . , method = "class", data = dropout_train_tree, control = rpart.control(cp = 0, maxdepth = 3, minsplit = 200))

rpart.plot(dropout_fit_preprun)



dropout_test$predict_tree_prun <- ""
dropout_test$predict_tree_prun <- predict(dropout_fit_preprun, dropout_test, type = "class")

dropout_test$predict_tree_prun <- ifelse(dropout_test$predict_tree_prun == 1, "Dropout", "No dropout")

table(dropout_test$predict_tree_prun)
xtabs(~dropout+predict_tree_prun, data = dropout_test)

# random forest

dropout_train_for <- dropout_train

dropout_train_for %>% 
  group_by(dropout) %>% 
    summarise(count=n())

balanced_dropout <- ovun.sample(dropout ~ ., data = dropout_train_for, method = "over",N = 920)$data

table(balanced_dropout$dropout)

random <- sample(1:(nrow(balanced_dropout)))
balanced_dropout <- balanced_dropout[random,]

mtry <- tuneRF(balanced_dropout[-12],balanced_dropout$dropout, ntreeTry=1000,
               stepFactor=1.5,improve=0.01, trace=TRUE)


best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

dropout_fit_for <- randomForest(dropout~., data = balanced_dropout, ntree = 1000, mtry = 4, importance = TRUE)

dropout_fit_for

dropout_test$predict_for <- ""
dropout_test$predict_for <- predict(dropout_fit_for, dropout_test, type = "class")

xtabs(~dropout+predict_for, data = dropout_test)
