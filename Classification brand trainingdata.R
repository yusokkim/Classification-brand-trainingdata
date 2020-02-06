install.packages("caret", dependencies = c("Depends", "Suggests"))

summary(CompleteResponses)

#check for missing values, although I know this is the complete dataset
any(is.na(CompleteResponses))

#Check names of attributes
names(CompleteResponses)

#Boxplots for eacht attribute, and see if there are outliers:
boxplot(CompleteResponses$salary)
boxplot(CompleteResponses$age)
hist(CompleteResponses$age)
hist(CompleteResponses$elevel) #error, x must be numeric
hist(CompleteResponses$car) #error, x must be numeric
hist(CompleteResponses$zipcode) #error, x must be numeric
boxplot(CompleteResponses$credit)
hist(CompleteResponses$brand) #error, x must be numeric

#checking the data types
str(CompleteResponses)

#Convert data type elevel: int to ordinal
CompleteResponses$elevel <- as.ordered(CompleteResponses$elevel)

#Convert data type car: int to factor
CompleteResponses$car <- as.factor(CompleteResponses$car)

#Convert data type zipcode: int to factor
CompleteResponses$zipcode <- as.factor(CompleteResponses$zipcode)

#Convert data type brand: int to factor, 0 (acer) = false and 1 (Sony) = true
CompleteResponses$brand <- as.factor(CompleteResponses$brand)

#changing false and true to acer and sony
levels(CompleteResponses$brand) <-c('Acer','Sony')

#checking the data types again after converstion
str(CompleteResponses)

#Making boxplots again for the converted attributes:
plot(CompleteResponses$elevel) #instead of hist which is only for numeric values
plot(CompleteResponses$car) #instead of hist which is only for numeric values
plot(CompleteResponses$zipcode) #instead of hist which is only for numeric values

#how to plot brand
plot(CompleteResponses$brand)
#pie chart
mytable <- table(CompleteResponses$brand)
pie(mytable, main="Pie Chart of Brands")

library(ggplot2)

#changing numeric value to categorical (discretization): salary, age and credit
Catsalary <- cut(CompleteResponses$salary, breaks=c(0,30000,60000,90000,120000,150000), labels = c("Salary 0-30000", "Salary 30000-60000","Salary 60000-90000","Salary 90000-120000","Salary 120000-150000")) #5 bins
Catage <- cut(CompleteResponses$age, breaks=c(20,40,60,81), labels = c("Age 20-40","Age 40-60","Age 60-80"), right=FALSE) #3 bins

#add extra column
CompleteResponses["Catage"] <- Catage
CompleteResponses["Catsalary"] <- Catsalary

plot(Catage)
plot(Catsalary)
Catage[1:10]

#make plot: difference in salary between brand preference acer vs. sony
ggplot(data = CompleteResponses) +
  geom_boxplot(aes(x = brand, y = salary)) #people buying sony have higher salary

ggplot(data = CompleteResponses, aes(x = salary)) +
  geom_histogram(aes(fill=brand), bins = 6) +
  facet_wrap(~zipcode)

#make plot: difference in age between brand preference acer vs. sony
boxplot(CompleteResponses$age ~ CompleteResponses$brand)
ggplot(data = CompleteResponses) +
  geom_boxplot(aes(x = brand, y = age)) #no difference in age between acer vs. sony

ggplot(data = CompleteResponses) +
  geom_jitter(aes(x = brand, y = age))

ggplot(data = CompleteResponses, aes(x = age)) +
  geom_bar(stat = "count", aes(fill=brand)) +
  facet_wrap(~zipcode) #different zipcodes





#relationship between age vs. salary for acer and sony
#geom_point
ggplot(data = CompleteResponses) +
  geom_point(aes(x = age,y = salary, col=brand)) +
  facet_wrap(~brand) 
#geom_jitter
ggplot(data = CompleteResponses) +
  geom_jitter(aes(x = age, y = salary, col=brand)) +
  facet_wrap(~brand)
#categorized
ggplot(data = CompleteResponses) +
  geom_jitter(aes(x = Catage, y = Catsalary, col=brand)) +
  facet_wrap(~brand) +
  theme_bw()

#relationship between salary (5 categories) vs. brand, for age (3 categories)
#geom_jitter
ggplot(data = CompleteResponses) +
  geom_jitter(aes(x = brand, y = Catsalary, col=Catage))
ggplot(data = CompleteResponses) +
  geom_jitter(aes(x = brand, y = Catsalary, col=brand)) +
  facet_wrap(~Catage) +
  theme_bw()

#relationship between age (3 categories) vs. brand, for salary (5 categories)
#geom_jitter
ggplot(data = CompleteResponses) +
  geom_jitter(aes(x = brand, y = Catage, col=brand)) +
  facet_wrap(~Catsalary) +
  theme_bw()




#make plot: difference in elevel between acer vs. sony
ggplot(data = CompleteResponses, aes(x = elevel)) +
  geom_bar(stat = "count", aes(fill=brand)) +
  facet_wrap(~brand) #no difference in elevel between acer vs. sony

#make plot: difference in primary car between acer vs. sony
ggplot(data = CompleteResponses, aes(x = car)) +
  geom_bar(stat = "count", aes(fill=brand)) +
  facet_wrap(~brand) #no difference in primary car between acer vs. sony

#make plot: difference in zipcode between acer vs. sony
ggplot(data = CompleteResponses, aes(x = zipcode)) +
  geom_bar(stat = "count", aes(fill=brand)) +
  facet_wrap(~brand) #no difference in zipcode between acer vs. sony

#make plot: difference in credit between brand preference acer vs. sony
ggplot(data = CompleteResponses) +
  geom_boxplot(aes(x = brand, y = credit)) 

ggplot(data = CompleteResponses, aes(x = credit)) +
  geom_histogram(aes(fill=brand), bins = 10) +
  facet_wrap(~zipcode)

#delete columns in dataset: Catage and Catsalary
CompleteResponses <- CompleteResponses[,-8]
CompleteResponses <- CompleteResponses[,-8]

library(caret)
library(lattice)
set.seed(688)
inTraining <- createDataPartition(CompleteResponses$brand, p = .75, list = FALSE)
trainSet <- CompleteResponses[inTraining,]
testSet <- CompleteResponses[-inTraining,]

#decision tree C5.0
#10 fold cross validation, repeat = 1
fitControl <- trainControl(method = "repeatedcv", 
                           number = 10, 
                           repeats = 1)

#Train model using C5.0, with all independent variables to predict brand, tunelength = 2
dt_model_all <- train(brand~.,
                      data = trainSet,
                      method = "C5.0",
                      tunelength = 2)

#performance of the model
dt_model_all

#Train model using C5.0, with independent variables age and salary to predict brand, tunelength = 2
dt_model_2 <- train(brand~age+salary,
                   data = trainSet,
                   method = "C5.0",
                  tunelength = 2)

#performance of the model
dt_model_2


#how the model prioritized each feature in the training
plot(varImp(dt_model_all))

#random forest
#10 fold cross validation, repeat = 1
fitControl <- trainControl(method = "repeatedcv", 
                           number = 10, 
                           repeats = 1)

#train Random Forest Regression model with age and salary as predictors for brand 
#with a tuneLenght = 1 (trains with 1 mtry value for RandomForest)
rf_model_2 <- train(brand~age+salary,
                  data = trainSet,
                  method = "rf",
                  trControl=fitControl,
                  tunelength = 1)

#results
rf_model_2

rf_model_2_1 <- rf_model_2

#train Random Forest Regression model with age and salary as predictors for brand 
#with a tuneLenght = 2 (trains with 2 mtry value for RandomForest)
rf_model_2_2 <- train(brand~age+salary,
                  data = trainSet,
                  method = "rf",
                  trControl=fitControl,
                  tunelength = 2)
#results
rf_model_2_2 

#train Random Forest Regression model with age and salary as predictors for brand 
#with a tuneLenght = 3 (trains with 2 mtry value for RandomForest)
rf_model_2_3 <- train(brand~age+salary,
                      data = trainSet,
                      method = "rf",
                      trControl=fitControl,
                      tunelength = 3)
#results
rf_model_2_3

#train Random Forest Regression model with age, salary and credit as predictors for brand 
#with a tuneLenght = 1 (trains with 1 mtry value for RandomForest)
rf_model_3_1 <- train(brand~age+salary+credit,
                      data = trainSet,
                      method = "rf",
                      trControl=fitControl,
                      tunelength = 1)
#results
rf_model_3_1

#train Random Forest Regression model with age, salary and credit as predictors for brand 
#with a tuneLenght = 2 (trains with 2 mtry value for RandomForest)
rf_model_3_2 <- train(brand~age+salary+credit,
                      data = trainSet,
                      method = "rf",
                      trControl=fitControl,
                      tunelength = 2)
#results
rf_model_3_2


#train Random Forest Regression model with all variables as predictors for brand 
#with a tuneLenght = 1 (trains with 1 mtry value for RandomForest)
rf_model_all_1 <- train(brand~.,
                    data = trainSet,
                    method = "rf",
                    trControl=fitControl,
                    tunelength = 1)

#results
rf_model_all_1

#train Random Forest Regression model with all variables as predictors for brand 
#with a tuneLenght = 2 (trains with 2 mtry value for RandomForest)
rf_model_all_2 <- train(brand~.,
                        data = trainSet,
                        method = "rf",
                        trControl=fitControl,
                        tunelength = 2)

#results
rf_model_all_2

#train Random Forest Regression model with all variables as predictors for brand 
#with a tuneLenght = 3 (trains with 3 mtry value for RandomForest)
rf_model_all_3 <- train(brand~.,
                        data = trainSet,
                        method = "rf",
                        trControl=fitControl,
                        tunelength = 3)

#results
rf_model_all_3
plot(rf_model_all_3)

#train Random Forest Regression model with all variables as predictors for brand 
#with a tuneLenght = 4 (trains with 4 mtry value for RandomForest)
rf_model_all_4 <- train(brand~.,
                        data = trainSet,
                        method = "rf",
                        trControl=fitControl,
                        tunelength = 4)

#results
rf_model_all_4

#train Random Forest Regression model with all variables as predictors for brand 
#with a tuneLenght = 5 (trains with 5 mtry value for RandomForest)
rf_model_all_5 <- train(brand~.,
                        data = trainSet,
                        method = "rf",
                        trControl=fitControl,
                        tunelength = 5)

#results
rf_model_all_5

#manual Grid
#10 fold cross validation, repeat = 1
fitControl <- trainControl(method = "repeatedcv", 
                           number = 10, 
                           repeats = 1)

#dataframe for manual tuning of mtry
rfGrid <- expand.grid(mtry=c(1,2,3))

#train Random Forest Regression model with age and salary as predictors for brand 
#with a tuneLenght = 1 (trains with 1 mtry value for RandomForest)
rf_modelmanual_1 <- train(brand~.,
                    data = trainSet,
                    method = "rf",
                    trControl=fitControl,
                    tuneGrid=rfGrid)
#results
rf_modelmanual_1

#predict on new data, model: dt C5.0, predictors: age and salary, accuracy 0.913, kappa 0.815
pred_brand_dt <- predict(dt_model_2, newdata = SurveyIncomplete)

#postresample, comparing accuracy testSet
postResample(pred_brand_dt,testSet$brand)
# accuracy 0.527, kappa -0.0017, so C5.0 model is overfitting

#predict on new data, model: rf, predictors: age and salary, accuracy 0.913, kappa 0.815
pred_brand_rf <- predict(rf_model_2_1, newdata = SurveyIncomplete)

#postresample, comparing accuracy testSet
postResample(pred_brand_rf,testSet$brand) # accuracy 0.624, kappa 0, so rf model is overfitting

###########################################

#going back to model random forest.

#varImp of rf model:
varImp(rf_model_all_1) # salary 100.00, age 64

#10 fold cross validation, repeat = 1
fitControl <- trainControl(method = "repeatedcv", 
                           number = 10, 
                           repeats = 1)

#training new Random forest model using only 1 predictor salary for output variable brand 
#with a tuneLenght = 1 (trains with 1 mtry value for RandomForest)
rf_model_1_1 <- train(brand~salary,
                        data = trainSet,
                        method = "rf",
                        trControl=fitControl,
                        tunelength = 1)

#results of model
rf_model_1_1 #accuracy 0.644, kappa 0.24

#predict on new data, model: rf, predictors: salary, accuracy 0.644, kappa 0.24
pred_brand_rf1 <- predict(rf_model_1_1, newdata = SurveyIncomplete)

#postresample, comparing accuracy testSet
postResample(pred_brand_rf1,testSet$brand) # accuracy 0.521, kappa -0.01889, low accuracy for both training and testSet.

#######now selecting model with 3 predictors (age, salary, credit) and see what the accuracy is in testSet.

#predict on new data, model: rf, predictors: age, salary, credit
pred_brand_rf3 <- predict(rf_model_3_1, newdata = SurveyIncomplete)

#postresample, comparing accuracy testSet
postResample(pred_brand_rf3,testSet$brand) #accuracy 0.62, kappa 0, indicating overfitting of training dataset.

####### how to handle overfitting, going back to model, try less folds

#5 fold cross validation, repeat = 1
fitControl <- trainControl(method = "repeatedcv", 
                           number = 5, 
                           repeats = 1)

#Train model using C5.0, with independent variables age and salary to predict brand, tunelength = 2
dt_model_2 <- train(brand~age+salary,
                    data = trainSet,
                    method = "C5.0",
                    tunelength = 2)
#results
dt_model_2 #accuracy 0.91, kappa 0.81

#predict on new data, model: dt C5.0, predictors: age and salary, accuracy 0.91, kappa 0.81
pred_brand_dt <- predict(dt_model_2, newdata = SurveyIncomplete) 

#postresample, comparing accuracy testSet
postResample(pred_brand_dt,testSet$brand)
# accuracy 0.521, kappa -0.00834, so model with less folds is still overfitting