library(readxl)
education <- read_excel("C:/Users/54208/Google Drive/School/Graduate School/Masters/Fall 2021/DataMining/Continuing Education.xlsx")


library(caret)

library(gains)

library(rpart)

library(rpart.plot)

library(pROC)

education$ContinueEdu <- as.factor(education$ContinueEdu)

set.seed(1)

smp_size <- floor(.7 * nrow(education))

train_ind <- sample(seq_len(nrow(education)), size = smp_size)

trainSet <- education[train_ind,]
testSet <- education[-train_ind,]

default_tree <- rpart(ContinueEdu ~ . , data=trainSet, method="class")
summary(default_tree)

prp(default_tree, type=1, extra=1, under = TRUE)

full_tree <- rpart(ContinueEdu ~ ., data= trainSet, method="class", cp=0, minsplit=2, minbucket=1)

prp(full_tree, type=1, extra=1, under = TRUE)

printcp(full_tree)

pruned_tree <- prune(full_tree, cp=0.0098684)

prp(pruned_tree, type=1, extra=1, under = TRUE)

predicted_class <- predict(pruned_tree, testSet, type="class")
confusionMatrix(predicted_class, testSet$ContinueEdu, positive="1")
