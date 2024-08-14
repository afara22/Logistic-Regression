#load packages
library(tidyverse)
library(caret)
library(tidymodels)

#read in csv data file
lifeInsurance_raw_data <- read_csv("insurance.csv")

view(lifeInsurance_raw_data)

#prep data
lifeInsurance_data <- lifeInsurance_raw_data %>% 
  mutate(smokerInt = as.integer(smoker == "yes"))

view(lifeInsurance_data)

#visualize
ggplot(lifeInsurance_data, aes(x = expenses, y = smokerInt)) +
  geom_jitter(height = .05,
              alpha = .1)


#split prepped data into training and testing sets
set.seed(2)

split <- initial_split(lifeInsurance_data, prop = .80, strata = "smokerInt")
insurance_train <- training(split)
insurance_test <- testing(split)

view(insurance_train)
view(insurance_test)

#visualize data
ggplot(insurance_train, aes(x = expenses, 
                            y = smokerInt)) +
  geom_jitter(height = .05, 
              alpha = .1) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"),
              se = FALSE) +
  theme_minimal() #changes background from gray to white
  
#build that model
model <- glm(smokerInt ~ expenses,
             data = insurance_train, 
             family = "binomial")

summary(model)

#evaluate model on testing set
#first we'll add new columns to testing data
insurance_test <- insurance_test %>% 
  mutate(smoker_prob = predict(model,
                               insurance_test,
                               type = "response"),     #type makes sure it 
         smoker_pred = ifelse(smoker_prob > .5, 1, 0)) #returns p not logit(p)

view(insurance_test)

#confusion matrix
t <- table(insurance_test$smoker, 
           insurance_test$smoker_pred)

accuracy <- sum(diag(t))/sum(t)
accuracy # 89.925% accurate

TPR <- t[2,2] / sum(t[2,]) #positive rate given it's positive 
                            #(sensitivity, True Positive Rate)

FNR <- t[2,1] / sum(t[2,]) #negative rate given it's positive 
                            #(False Negative Rate)

TNR <- t[1,1] / sum(t[1,]) #negative rate given it's negative
                            #(specificity, True Negative Rate)

FPR <- t[1,2] / sum(t[1,]) #positive rate given it's negative
                            #(False Positive Rate)

#display these results
results <- c(accuracy = accuracy, TPR = TPR, FNR = FNR, TNR = TNR, FPR = FPR)
results
