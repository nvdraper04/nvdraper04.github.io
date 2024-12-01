library(rpart)
library(rpart.plot)

#################################################
# Credit tree
#################################################
library(ISLR2)

myCredit = Credit

set.seed(34)
training = sample(1:400, size = 400*.80)
credit_training = myCredit[training, ]
credit_testing = myCredit[-training, ]

dim(credit_training)
dim(credit_testing)

# Model 1: Base model 

base_credit_model = rpart(Own ~., method = "class", data = credit_training, 
                     control = rpart.control( cp = 0))

prp(base_credit_model)

rpart.plot(base_credit_model)

# base accuracy
credit_testing$predict1 = predict(base_credit_model, 
                                 credit_testing, type= "class" )

base_accuracy = mean(credit_testing$predict1 == credit_testing$Own)     
base_accuracy

credit_testing$Own
credit_testing$predict1

# pre pruning

credit_model_preprun = rpart(Own ~., method = "class", data = credit_training, 
                             control = rpart.control(cp = 0, maxdepth = 3, 
                                                     minsplit = 25))

prp(credit_model_preprun)
rpart.plot(credit_model_preprun)

# Accuracy of pre pruned tree

credit_testing$predict2 = predict(credit_model_preprun, credit_testing, 
                                  type = "class")

preprun_accuracy = mean(credit_testing$predict2 == credit_testing$Own)
preprun_accuracy

# Post pruning

printcp(base_credit_model)
base_credit_model$cptable[which.min(base_credit_model$cptable[,"xerror"]),"CP"]

pruned_credit = prune(base_credit_model, cp = 0.02922078)
prp(pruned_credit)
rpart.plot(pruned_credit)


credit_testing$prediction3 = predict(pruned_credit, credit_testing, 
                                     type = "class")

postpruned_credit_accuracy = mean(credit_testing$prediction3 == 
                                    credit_testing$Own)

# Compare Accuracy 

data.frame(base_accuracy, preprun_accuracy, postpruned_credit_accuracy)







