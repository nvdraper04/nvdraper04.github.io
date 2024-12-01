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
# 0 = yes no = 1

set.seed(45)
sort(sample(0:100, 10))

set.seed(91)
sample(rep(0:1, 4))

set.seed(78)
sample(rep(0:1, 4))

set.seed(64)
sample(rep(0:1, 4))

mean(c(10, 36))
mean(c(36, 38))
mean(c(38, 45))
mean(c(45, 57))
mean(c(57, 63))
mean(c(63, 74))
mean(c(74, 82))
mean(c(82, 90))
mean(c(90, 95))
# Likes Cake Impurity 

1 - (1/3)^2 - (2/3)^2 # Left node: 0.44444

1 - (5/7)^2 - (2/7)^2 #Right node: 0.375

(3/10)*0.44444 + (7/10)*0.4081633 # Total Impurity

# Student Impurity

1 - (3/5)^2 - (2/5)^2 # Left node: 

1 - (3/5)^2 - (2/5)^2 # Right node:

(5/10)*0.48 + (5/10)*0.48

mean(c(10, 36))
mean(c(36, 38))
mean(c(38, 45))
mean(c(45, 57))
mean(c(57, 63))
mean(c(63, 74))
mean(c(74, 82))
mean(c(82, 90))
mean(c(90, 95))


# Age < 23 

# left Node: 0

1 - (1/1)^2 - (0/1)^2

1 - (5/9)^2 - (4/9)^2 

(1/10)*0 + (9/10)*0.4938272# Total Impurity


# Age < 37

1 - (2/2)^2 - (0/2)^2

1 - (4/8)^2 - (4/8)^2
 
(2/10)*0 + (8/10)*0.5

# Age < 41.5

1 - (2/3)^2 - (1/3)^2

1 - (4/7)^2 - (3/7)^2

(3/10)*0.4444444 + (7/10)*.4897959

# Age < 51 

1 - (2/4)^2 - (2/4)^2

1 - (4/6)^2 - (2/6)^2

(4/10)*.5 + (6/10)*.4444444

# Age < 60

1 - (2/5)^2 - (3/5)^2 

1 - (4/5)^2 - (1/5)^2

(5/10)*.48 + (5/10)*.32

# Age < 68.5

1 - (3/6)^2 - (3/6)^2

1 - (3/4)^2 - (1/4)^2

(6/10)*.5 + (4/10)*.375

# Age < 78

1 - (4/7)^2 - (3/7)^2

1 - (2/3)^2 - (1/3)^2

(7/10)*.4897959 + (3/10)*.444444

# Age < 86

1 - (5/8)^2 - (3/8)^2

1 - (1/2)^2 - (1/2)^2

(8/10)*.46875 + (2/10)*.5

# Age < 92.5

1 - (5/9)^2 - (4/9)^2

1 - (1/1)^2 - (0/1)^2

(9/10)*.4938272 + (1/10)*0

# Doesn't like cake and is or not a student : .3428571

1 - (3/5)^2 - (2/5)^2 # Is a student

1 - (2/2)^2 - (0/2)^2 # Is not a student

(5/7)*.48 + (2/7)*0

# New age means
mean(c(57, 63))
mean(c(63, 74))
mean(c(74, 90))
mean(c(90, 95))


# Age < 60 : 0.3 - Lowest Gini Impurity

1 - (0/1)^2 - (1/1)^2

1 - (3/4)^2 - (1/4)^2

(1/5)*0 + (4/5)*.375

# Age < 68.5 : 0.4666667

1 - (1/2)^2 - (1/2)^2

1 - (2/3)^2 - (1/3)^2

(2/5)*0.5 + (3/5)*.4444444

# Age < 82 : .4666664

1 - (2/3)^2 - (1/3)^2

1 - (1/2)^2 - (1/2)^2

(3/5)*.444444 + (2/5)*.5

# Age < 92.5 : .4

1 - (2/4)^2 - (2/4)^2

1 - (1/1)^2 - (0/1)^2

(4/5)*.5 + (1/5)*0


(1/3)^2

(2/3)^2






