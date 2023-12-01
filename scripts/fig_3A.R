#Classification Tree for Metabolite Mice Baseline between Persist:NonPersist
#n=12
#
load(file = "../data/ClassificationTree-Jules.RData")
library(rpart)
library(dplyr)

#Step 1. Created a data.frame that includes variable of interest (responder) and covariables
#Persist and Nonpersist is a binary variable
#Bacterial taxa at the family level
#Cohort
#Treatment and Day will be identical across all mice

#Data file
colnames(classtree_persist_test)

#Step 2: Classification tree with a minsplit of 2
classificationv2 <- rpart(Responder ~ ., data = classtree_persist_test, control =rpart.control(minsplit =2,minbucket=1, cp=0))
plot(classificationv2, uniform = T, margin=0.1)
text(classificationv2, use.n = T, all = T)

#Step 3: Evaluate where respond/nonrespond are located in tree, missclassification of any mice?
#Root node has n = 12 (5=Respond)
#8 mice have Erysip > 0.009 
table(classtree_persist_test$Erysipelotrichaceae, classtree_persist_test$Responder)
table(classtree_persist$Erysipelotrichaceae, classtree_persist$Subject)
table(classtree_persist$Responder, classtree_persist$Subject)
#lm3 (responder) is Erysip >0.009 and is actually 0.142 relative abundance (the highest rel ab of Erysip of all the mice)

#Step 4: Is Clostridiales vadin group < 0.016 false for the other 4 responders as well? Or merely a good marker for lm3 versus the non-responders?
table(classtree_persist$`Clostridiales vadingroup`, classtree_persist$Subject)
#Not a marker that holds true for all responder mice
#Clostridiales vadingroup: lj3 =  0.009666667; ma1 = 0.009333333; ma5 = 0.012; lm1 =  0.011666667
#costridiales vadingroup is likely a case of oversplitting the tree
#Conclusion: At baseline, having below 1% Erysipelotrichaceae is a marker for predicting whether a mouse would be a responder/non-responder.
#HOWEVER, there is one case where a mouse with high Erysipelo was categorized as a responder. 
