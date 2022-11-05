library(caret)
library(ggplot2)

if(!file.exists("./raw_data")){
  dir.create("./raw_data")
}

url1<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url2<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

download.file(url1, "./raw_data/training.csv")
download.file(url2, "./raw_data/testing.csv")
training<-read.csv("./raw_data/training.csv",na.strings="")
testing<-read.csv("./raw_data/testing.csv",na.strings="")
str(training)



#your goal will be to use data from accelerometers on the belt, 
#forearm, arm, and dumbell of 6 participants.
#replace "NA" with NA
training[training=="NA"]<-NA

#analyze NA
na_count<-table(colSums(is.na(training)),colnames(training))
#100 columns has 19216 NA while 60 columns has 0 NA
sum(colSums(is.na(training))==19216)
sum(colSums(is.na(training))==0)
#remove columns with 19216 NA
training_v1<-training[, colSums(is.na(training)) == 0]
#remove column 1-7
training_v1<-training_v1[,8:60]

#do the same to the testing data
testing[testing=="NA"]<-NA
testing_v1<-testing[, colSums(is.na(testing)) == 0]
testing_v1<-testing_v1[,8:60]

#training with decision tree model
#use k-fold cross validation
set.seed(100)
# value of K equal to 10
train_control <- trainControl(method = "cv", number = 10)
tree_model<-train(classe ~ ., method="rpart", data=training_v1, 
                  trControl = train_control)
print(tree_model) #average accuracy=0.506, poor
#predict testing
predict_tree<-predict(tree_model,newdata = testing_v1)
predict_tree
#C A C A A C C A A A C C C A C A A A A C

#random forest model, no need to do CV
rf_model <- train(classe ~ ., data = training_v1, method = "rf") #this will take some time
print(rf_model)
pred_rf <- predict(rf_model, testing_v1)
print(pred_rf)





