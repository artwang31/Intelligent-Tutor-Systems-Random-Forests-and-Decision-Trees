library(rpart)
library(party)
library(rpart.plot)
library(randomForest)
library(caTools)
library(zoo)
library(tidyverse)

D1 <- intelligent_tutor

# [1] "id"                    "prior_prob_count"      "prior_percent_correct" "score"                
# [5] "hints"                 "hint.y"                "complete"              "action"
# nrow(D1) = 378 rows

# predict for action, along hint.y and complete
tree <- rpart(action ~ hint.y + complete, method="class", data=D1) 
printcp(tree)
prp(tree)

# Data Pre-Processing
max(D1$hints, na.rm = TRUE) # 95 hints
min(D1$hints, na.rm = TRUE) # 0 hints
mean(D1$hints, na.rm = TRUE) # 5.65 hints
D1$advice <- ifelse(D1$action == 3, 'no action', ifelse(D1$action == 2, 'monitor', 'intervene'))

# Train test split
set.seed(101)
sampleD1 <- sample.split(D1$action, SplitRatio = 0.7) # nrow(college) = 378
train <- subset(D1, sampleD1 == TRUE) # 70% training set, 265 rows
test <- subset(D1, sampleD1 == FALSE) # 30% test set, 113 rows
traintree <- ctree(tree, data=train)
plot(traintree)

score_ctree <- rpart(advice ~ prior_prob_count + prior_percent_correct + hints, 
                     method = 'class', data = D1)
prp(score_ctree)
summary(score_ctree)
predict(score_ctree)
# The results indicate that the predict advice was similar to the actual advice that was given. 
#      CP nsplit rel error xerror       xstd
# 1 0.052      0     1.000  1.080 0.03513240
# 2 0.022      1     0.948  1.076 0.03522926
# 3 0.020      4     0.880  1.044 0.03595235
# 4 0.012      5     0.860  0.992 0.03694125
# 5 0.010      9     0.812  0.996 0.03687301
