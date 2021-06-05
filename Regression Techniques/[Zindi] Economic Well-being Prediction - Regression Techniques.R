# libraries----
library(dplyr)
library(caret)
library(ggplot2)
library(reshape2)
library(glmnet)
library(MASS)
library(leaps)

# data----
econ_wb <- read.csv("data/Train.csv")

glimpse(econ_wb)


# correlation & hypothesis tests----

# ANOVA test  - country
oneway.test(Target~country, data = econ_wb)

ftest_country <- aov(Target~country, data = econ_wb)
summary(ftest_country)

# high correlation - very small p-value and high test statistic



# correlation - rest
cor_econ <- econ_wb[, c(5:19)]
cormat_econ <- round(cor(cor_econ),3)
cormat_econ_df <- as.data.frame(cormat_econ)
cormat_econ_df

cor_t <- cormat_econ_df[, c(1,15)]


melt_cormat_econ <- melt(cormat_econ)

ggplot(melt_cormat_econ, aes(x=Var1, y=Var2, fill=value))+
  geom_tile()


unique(econ_wb$country)
econ_wb$country <- as.factor(econ_wb$country)
econ_wb$urban_or_rural <- ifelse(econ_wb$urban_or_rural == "U", 1, 0)
econ_wb$urban_or_rural <- as.factor(econ_wb$urban_or_rural)
unique(econ_wb$urban_or_rural)

glimpse(econ_wb)

unique(econ_wb$year)
econ_wb$year <- as.factor(econ_wb$year)

ec_data <- econ_wb[,-1]
colnames(ec_data)

random <- sample(1:nrow(ec_data))
ec_data <- ec_data[random,]

split <- c(1: (nrow(ec_data) * 0.7)) 
train <- ec_data[split,]
test <- ec_data[-split,]

glimpse(train)
cor_t


#pre-processing----
# dummies

dmy <- dummyVars("~.", data = train)
train_dmy <- data.frame(predict(dmy, newdata = train))

colnames(train_dmy)

train_dmy <- train_dmy[, -c(19:21,24,25,29)]
train_dmy <- train_dmy[, -c(1:18)]




dmy <- dummyVars("~.", data = test)
test_dmy <- data.frame(predict(dmy, newdata = test))

colnames(test_dmy)

test_dmy <- test_dmy[, -c(19:21,24,25,29)]
test_dmy <- test_dmy[, -c(1:18)]


# regression models----
#### polynomial----

pm1 <- lm(Target ~ poly(landcover_urban_fraction, 3) + poly(nighttime_lights, 3) + poly(ghsl_pop_density, 3) + poly(ghsl_built_1975_to_1990, 3) +  poly(ghsl_built_pre_1975, 3) +  poly(ghsl_built_2000_to_2014, 3) +  poly(ghsl_built_1990_to_2000, 3) +  poly(ghsl_not_built_up, 3) + poly(landcover_crops_fraction, 3) + poly(dist_to_shoreline, 3) + year.1998 + year.1999 + year.2005 + year.2006 + year.2007 + year.2010 + year.2011 + year.2012 + year.2013 + year.2014 + year.2015 + year.2016 + urban_or_rural.0 + urban_or_rural.1, data = train_dmy)
summary(pm1)



pm2 <- lm(Target ~ poly(landcover_urban_fraction, 3) + poly(nighttime_lights, 3) + poly(ghsl_pop_density, 3) + poly(ghsl_built_1975_to_1990, 2) +  poly(ghsl_built_pre_1975, 1) + poly(ghsl_built_2000_to_2014, 3) +  poly(ghsl_not_built_up, 2) + poly(dist_to_shoreline, 1) +  poly(ghsl_built_1990_to_2000, 1) + year.1998 + year.1999 + year.2005 + year.2006 + year.2007 + year.2010 + year.2011 + year.2012 + year.2013 + year.2014 + year.2015 + year.2016 +  urban_or_rural.0 + urban_or_rural.1, data = train_dmy)
summary(pm2)


### linear----
lm1 <- lm(Target ~ . , data = train_dmy)
summary(lm1)



### ridge----



ridge_cv <- cv.glmnet(as.matrix(train_dmy[,-30]), train$Target, alpha = 0)
ridge_cv$lambda.min

rm2 <- glmnet(as.matrix(train_dmy[,-30]), train_dmy$Target, alpha = 0, lambda = ridge_cv$lambda.min)
coef(rm2)



pred <- predict(rm2, s=ridge_cv$lambda.min, newx=as.matrix(test_dmy[,-30]))

R2(pred, test_dmy$Target)
RMSE(pred, test_dmy$Target)


### lasso----

lasso_cv <- cv.glmnet(as.matrix(train_dmy[,-30]), train$Target, alpha = 1)
lasso_cv$lambda.min

lsm1 <-  glmnet(as.matrix(train_dmy[,-30]), train_dmy$Target, alpha = 1, lambda = lasso_cv$lambda.min)
coef(lsm1)

pred2 <- predict(lsm1, s=lasso_cv$lambda.min, newx=as.matrix(test_dmy[,-30]))


R2(pred2, test_dmy$Target)
RMSE(pred2, test_dmy$Target)


### stepwise----

train_control <- trainControl(method = "cv", number = 10)

stm1 <- train(
       Target~., data = train_dmy,
       method = "leapBackward", 
       tuneGrid = data.frame(nvmax = 1:5),
       trControl = train_control
)

stm1$results
stm1$bestTune
summary(stm1$finalModel)
coef(stm1$finalModel, 5)



### elasticnet----


elm2 <- train(
  Target ~., data = train_dmy, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)

elm2$bestTune

coef(elm2$finalModel, elm2$bestTune$lambda)

pred <- elm2 %>% 
  predict(as.matrix(test_dmy[,-30]))

R2(pred, test_dmy$Target)
RMSE(pred, test_dmy$Target)








# ---------------------------------------------------------------#



# validation

econ_test <- read.csv("./data/Test.csv")
glimpse(econ_test)

econ_test$country <- as.factor(econ_test$country)
econ_test$urban_or_rural <- ifelse(econ_test$urban_or_rural == "U", 1, 0)
econ_test$urban_or_rural <- as.factor(econ_test$urban_or_rural)
econ_test$year <- as.factor(econ_test$year)

unique(econ_test$country)
unique(econ_wb$country)

unique(econ_test$year)
unique(econ_wb$year)



glimpse(val_econ)


val_econ <- econ_test[,-1]

dmyVal <- dummyVars("~.", data = val_econ)
val_dmy <- data.frame(predict(dmyVal, newdata = val_econ))

colnames(val_dmy)

val_dmy <- val_dmy[,-c(1:7)]

val_dmy$Target <- ""
val_dmy$Target <- predict(pm1, val_dmy)


subm7 <- as.data.frame(cbind(econ_test$ID, val_dmy$Target))

subm7 <- subm7 %>% 
         rename(
           ID = "V1",
           Target = "V2"
         )

write.csv(subm7, "./data/(poly1(3))submission7.csv")
