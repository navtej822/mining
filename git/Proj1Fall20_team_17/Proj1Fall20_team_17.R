#Navtejinder Singh Brar 1001874259

#Project 1 Fall 2020


#rpart library is for implementing decision tree
#rattle is for displaying the decision tree
#we used libraries caret and e1071 to implement naive bayes as given in the description
library(rpart)
library(rattle)
library(caret)
library(e1071)
#loading dataset
setwd("/Users/ntb/Desktop/data_mining")
bank.additional.full<-read.csv(file ="bank-additional-full.csv",header=TRUE,sep=";")
#here we remove the rows with the unknown values basically we clean the data
processed<-bank.additional.full[!(bank.additional.full$age=="unknown"|bank.additional.full$job=="unknown"
                            |bank.additional.full$marital=="unknown"|bank.additional.full$education=="unknown"
                            |bank.additional.full$default=="unknown"|bank.additional.full$housing=="unknown"
                            |bank.additional.full$loan=="unknown"|bank.additional.full$contact=="unknown"
                            |bank.additional.full$month=="unknown"|bank.additional.full$day_of_week=="unknown"
                            |bank.additional.full$duration=="unknown"),]
#Here we remove the attributes mentioned in the project description and after removing those attributes we get the processed data set.
processed$marital <- NULL
processed$default <- NULL
processed$housing <- NULL
processed$loan <- NULL
processed$contact <- NULL
#Here we will analyze the model by deleting different attributes
#processed$duration<-NULL
#processed$day_of_week<-NULL
#processed$euribor3m<-NULL
#processed$age<-NULL
View(processed)
#seed is set to 170 as given in the project description for group 17
set.seed(170)
sampledata <- processed[sample(nrow(processed), 10000),]
View(sampledata)
#we do the split here whether it's 80-20 or 50-50
samplesize <- floor(0.8 * nrow(sampledata))

trainingsize <- sample(nrow(sampledata), samplesize)

trainingdata <- sampledata[trainingsize, ]
testdata <- sampledata[-trainingsize, ]

View(trainingdata)
View(testdata)
#control here is for controlling tree growth we can also use minsplit(minimum mentioned(number) objects are required to do a split) here but we are using cp for controlling tree growth and xval for 10 cross validations
#we can also remove cp if we do not want to prune the decision tree i.e. if we want to show the default decision tree we generate
ginidecisiontree <- rpart(y~., data = trainingdata, method = 'class', control = rpart.control(cp = 0, xval = 10))
#here cp is printed and the lowest corresponding cp to the xerror is taken and added to the rpart decision tree algo to get the pruned decison tree
printcp(ginidecisiontree)
plotcp(ginidecisiontree, lty = 3, col = 2, upper = "splits")
#here cp is taken
ginil_errorcp <- ginidecisiontree$cptable[which.min(ginidecisiontree$cptable[,"xerror"]),"CP"]
#cp is added here to get the pruned tree
prunedginidecisiontree<-rpart(y~., data = trainingdata, method = 'class', control = rpart.control(cp = ginil_errorcp, xval = 10))
#rattle library function fancyRpart which has many palettes is used to plot the pruned decision tree
fancyRpartPlot(prunedginidecisiontree, palettes = c("Greens", "Reds"), sub = "")
#here we get the importance of the attributes with respect to the tree
ginivariableimpo <- as.data.frame(prunedginidecisiontree$variable.importance)

print(paste('gini DT variable importance in index metric '))

print(ginivariableimpo)
#here we predict the outcome of 2000 or 5000 objects based on the split we are using
giniprediction <- predict(prunedginidecisiontree,testdata,type="class")


#information gain
#same is done for the information gain as it is done for gini just the syntax for creating the decision tree is different from that of gini
igdecisiontree <- rpart(y~., data = trainingdata, method = 'class', parms = list(split = 'information'), control = rpart.control(cp = 0, xval = 10))
#print the cp values
printcp(igdecisiontree)
#plot the values
plotcp(igdecisiontree, lty = 3, col = 2, upper = "splits")
#find the best cp
igl_errorcp <- igdecisiontree$cptable[which.min(igdecisiontree$cptable[,"xerror"]),"CP"]
#prune the tree using the cp we found
prunedigdecisiontree<-rpart(y~., data = trainingdata, method = 'class', control = rpart.control(cp = igl_errorcp, xval = 10))
#plot the pruned tree
fancyRpartPlot(prunedigdecisiontree, palettes = c("Greys", "Oranges"), sub = "")
#find the importance of the variables or attributes
igvariableimpo <- as.data.frame(prunedigdecisiontree$variable.importance)
#print the impoortance
print(paste('information gain variable importance in index metric '))
print(igvariableimpo)
#predict the test data  
igprediction <- predict(prunedigdecisiontree,testdata,type="class")



#naive bayes
#we are creating the model here
naiveclassy <- naiveBayes(as.factor(y)~., trainingdata, )
naiveclassy
#here we are predicting
naiveclassyprediction<-predict(naiveclassy,testdata,prob=TRUE)

#here we are creating the confusion matrix
cm_ig <- table(testdata$y, igprediction)
cm_gini <- table(testdata$y, giniprediction)
cm_nb <- table(testdata$y, naiveclassyprediction)



rownames(cm_gini) <- paste("Actual", rownames(cm_gini), sep = ":")
colnames(cm_gini) <- paste("Predicted", colnames(cm_gini), sep = ":")
rownames(cm_ig) <- paste("Actual", rownames(cm_ig), sep = ":")
colnames(cm_ig) <- paste("Predicted", colnames(cm_ig), sep = ":")
rownames(cm_nb) <- paste("Actual", rownames(cm_nb), sep = ":")
colnames(cm_nb) <- paste("Predicted", colnames(cm_nb), sep = ":")
#printing the confusion matrix for gini,IG and naive bayes
print(cm_gini)
print(cm_ig)
print(cm_nb)

#get the accuracy,precision, recall and f1 score using the formulas and values we obtained from the confusion matrix for gini
giniaccuracy <- sum(diag(cm_gini)) / sum(cm_gini)
giniprecision <- cm_gini[2, "Predicted:yes"] / sum(cm_gini[1, "Predicted:yes"], cm_gini[2, "Predicted:yes"])
ginirecall <- cm_gini[2, "Predicted:yes"] / sum(cm_gini[2, "Predicted:yes"], cm_gini[2, "Predicted:no"])
ginif1score <- (2*(ginirecall * giniprecision)) / (ginirecall + giniprecision)
#get the accuracy,precision, recall and f1 score using the formulas and values we obtained from the confusion matrix for IG
igaccuracy <- sum(diag(cm_ig)) / sum(cm_ig)
igprecision <- cm_ig[2, "Predicted:yes"] / sum(cm_ig[1, "Predicted:yes"], cm_ig[2, "Predicted:yes"])
igrecall <- cm_ig[2, "Predicted:yes"] / sum(cm_ig[2, "Predicted:yes"], cm_ig[2, "Predicted:no"])
igf1score <- (2*(igrecall * igprecision)) / (igrecall + igprecision)
#get the accuracy,precision, recall and f1 score using the formulas and values we obtained from the confusion matrix for naive bayes
nbaccuracy <- sum(diag(cm_nb)) / sum(cm_nb)
nbprecision <- cm_nb[2, "Predicted:yes"] / sum(cm_nb[1, "Predicted:yes"], cm_nb[2, "Predicted:yes"])
nbrecall <- cm_nb[2, "Predicted:yes"] / sum(cm_nb[2, "Predicted:yes"], cm_nb[2, "Predicted:no"])
nbf1score <- (2*(nbrecall * nbprecision)) / (nbrecall + nbprecision)
#print the accuracy,precision, recall and f1 for gini
print(paste('accuracy for gini is', giniaccuracy*100, '%'))
print(paste('precision for gini is', giniprecision))
print(paste('recall for gini is', ginirecall))
print(paste('F1 score for gini is', ginif1score))
#print the accuracy,precision, recall and f1 for IG 
print(paste('Accuracy for ig model is', igaccuracy*100, '%'))
print(paste('Precision for ig model is', igprecision))
print(paste('Recall for ig model is', igrecall))
print(paste('F1 score for ig model is', igf1score))
#print the accuracy,precision, recall and f1 for naive bayes
print(paste('Accuracy for naives bayes model is', nbaccuracy*100, '%'))
print(paste('Precision for naives bayes model is', nbprecision))
print(paste('Recall for naives bayes model is', nbrecall))
print(paste('F1 score for naives bayes model is', nbf1score))

