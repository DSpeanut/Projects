getwd()
setwd("C:/Users/Eunji/Desktop/Data Science/Durham/Modules/Machine Learning/Assignment")
data = read.csv("Dry_Bean_Dataset.csv", header=TRUE)

#Load all the packages into R
library(skimr)
library(tree)
library(caret)
library(randomForest)
library(gbm)
library(dplyr)
library(ggplot2)
library(GGally)

set.seed(1)

#Chaing the class variable as factor
data$Class <- as.factor(data$Class)

#Checking data summary
skim(data)
summary(data)

#Separating the data for pariplot considering large size(4variable vs Class)
sample_visualization = data[sample(nrow(data),500),]
sample_visualization1 = sample_visualization[,c(1:4,17)]
sample_visualization2 = sample_visualization[, c(5:8,17)]
sample_visualization3 = sample_visualization[, c(9:12,17)]
sample_visualization4 = sample_visualization[, c(13:17)]

#Plotting each pairplot of the variables for EDA
ggpairs(sample_visualization1, aes(colour = Class), upper = list(continuous = wrap("cor", size = 2.5)))
ggpairs(sample_visualization2, aes(colour = Class), upper = list(continuous = wrap("cor", size = 2.5)))
ggpairs(sample_visualization3, aes(colour = Class), upper = list(continuous = wrap("cor", size = 2.5)))
ggpairs(sample_visualization4, aes(colour = Class), upper = list(continuous = wrap("cor", size = 2.5)))


#Dependent variable analysis
class_percent <- data %>% 
  group_by(Class) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

#Plotting dependent variable in pie chart
p <- ggplot(class_percent, aes(x = "", y = perc, fill = Class)) +
  geom_col() +
  geom_label(aes(label = labels),
             position = position_stack(vjust = 0.7),
             show.legend = FALSE) +
  coord_polar(theta = "y")+
  ggtitle("Dry Beans Class Ratio")


p+theme(plot.title = element_text(size=20, hjust=0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

#Checking correlation coefficient
correlation <- cor(data[1:16])
correlation


#splitting data into 80/20 
set.seed(1)
training_obs = sample (1: 13611, 10889) #dividing the dataset to 80/20

train_data = data[training_obs,]
test_data = data[-training_obs,]

#Fitting general decision tree
normal_tree = tree(Class~., train_data)
summary(normal_tree)

#Plotting the general decision tree
plot(normal_tree); text(normal_tree, pretty = 0)

#Checking train/test accuracy
normal_train_pred = predict(normal_tree, train_data, type = 'class')
confusionMatrix(normal_train_pred, train_data$Class)
normal_test_pred = predict(normal_tree, test_data, type = 'class')
confusionMatrix(normal_test_pred, test_data$Class)

#Tree pruning process
pruned_tree = cv.tree(normal_tree, FUN=prune.misclass)
summary(pruned_tree)

#Plotting pruned tree
plot(pruned_tree,xlim=c(1,10))
abline(v=7, col="red")

#Based on pruning process fit the optimal model and plot the pruned tree
fixed_pruned_tree = prune.misclass(normal_tree, best=7)
plot(fixed_pruned_tree); text(fixed_pruned_tree, pretty=0)
summary(fixed_pruned_tree)

#Checking train/test accuracy for pruned tree
prunedtree_train_pred = predict(fixed_pruned_tree, train_data, type= 'class')
confusionMatrix(prunedtree_train_pred, train_data$Class)
prunedtree_pred = predict(fixed_pruned_tree, test_data, type= 'class')
confusionMatrix(prunedtree_pred, test_data$Class)

#Comparison between general decision tree and pruned tree
summary(normal_tree)
summary(fixed_pruned_tree)

#Fitting random forest model
set.seed(1)
RandomForest_tree = randomForest(Class ~., data=train_data, mtry = 16, importance=TRUE, type='class')

#Feature importance
importance(RandomForest_tree)
varImpPlot(RandomForest_tree)

#Checking train/test accuracy
randomall_train_pred = predict(RandomForest_tree, train_data, type='class')
confusionMatrix(randomall_train_pred, train_data$Class)

randomall_test_pred = predict(RandomForest_tree, test_data, type='class')
confusionMatrix(randomall_test_pred, test_data$Class)
summary(RandomForest_tree)

#Checking test accuracy by the number of subset variable selections for each split
accuracy_variable =c(1:16)*0
for(mtry_t in 1:16){
  set.seed(1)
  fit=randomForest(Class~.,data=train_data,mtry=mtry_t,ntree=100)
  pred=predict(fit,test_data)
  tab <- table(test_data$Class, pred)
  accuracy_variable[mtry_t] <- sum(diag(tab)) / sum(tab)
}

#Plot the above acccuracy to identify the optimal value for subset variable selections
plot(accuracy_variable, xlab="Number of variables randomly sampled", ylab="Accuracy")
abline(v=9, col='red')
which.max(accuracy_variable)

#finding the best tree numbers for randomforest model
accuracy_treenumber =c(1:100)*0
num_tree = c(1:100)
for(num in num_tree){
  fit=randomForest(Class~.,data=train_data,mtry=9,ntree=num)
  pred=predict(fit,test_data)
  tab <- table(test_data$Class, pred)
  accuracy_treenumber[num] <- sum(diag(tab)) / sum(tab)
}
plot(accuracy_treenumber, xlab = "Number of trees", ylab='Accuracy')
which.max(accuracy_treenumber)
abline(v=54,col='red')


#Based on the result of testing subset variable selection and the number of trees, fitting the optimal randomforest model
set.seed(1)
random_tree = randomForest(Class ~., data=train_data, mtry = 9, importance=TRUE, type='class', ntree=54)

#Checking train/test accuracy
random_train_pred = predict(random_tree, train_data, type='class')
confusionMatrix(random_train_pred, train_data$Class)

random_test_pred = predict(random_tree, test_data, type='class')
confusionMatrix(random_test_pred, test_data$Class)
summary(random_tree)

#Building boosting model with parameter tuning (shriankage/interaction depth)
boost_bean_0.01 <- gbm(as.character(Class) ~ .  
                  , data = train_data
                  , distribution="multinomial"   
                  , n.trees= 1000
                  , shrinkage=0.01
                  , cv.folds=10
                  , verbose=TRUE,
                  interaction.depth =2)

boost_bean_0.001<- gbm(as.character(Class) ~ .  
                  , data = train_data
                  , distribution="multinomial"   
                  , n.trees= 1000
                  , shrinkage=0.001
                  , cv.folds=10
                  , verbose=TRUE,
                  interaction.depth =2)


#finding the optimal tree numbers for boosting models(shrinkage 0.01)
accuracy_treenumber_boost =c(1:1000)*0
n.trees = c(1:1000)
level = c("BARBUNYA", "BOMBAY", "CALI", "DERMASON", "HOROZ", "SEKER","SIRA")
for(num in n.trees){
  set.seed(1)
  predmatrix<-predict(boost_bean_0.01,test_data,n.trees = num, type='response')
  predmatrix <- apply(predmatrix, 1, which.max)
  predmatrix <-factor(predmatrix, levels = c(1:7), labels = level)
  tab <- table(test_data$Class, predmatrix)
  accuracy_treenumber_boost[num] <- sum(diag(tab)) / sum(tab)
}

#plotting the accuracy by number of trees.
plot(accuracy_treenumber_boost, xlab = "Number of trees", ylab='Accuracy')
which.max(accuracy_treenumber_boost)
abline(v=775, col='red')

#Comaprison between shrinkage
summary(boost_bean_0.01)
summary(boost_bean_0.001)

#Checking accuracy on boosting model with number of tree 800 to check difference between shrinkage value
boost_pred_0.01_train = predict.gbm(boost_bean_0.01,train_data, n.trees = 800, type='response')
boost_pred_0.001_train = predict.gbm(boost_bean_0.001,train_data, n.trees = 800, type='response')
boost_pred_class_0.01_train <- apply(boost_pred_0.01_train, 1, which.max)
boost_pred_class_0.001_train <- apply(boost_pred_0.001_train, 1, which.max)

boost_pred_0.01_test = predict.gbm(boost_bean_0.01,test_data, n.trees = 800, type='response')
boost_pred_0.001_test = predict.gbm(boost_bean_0.001,test_data, n.trees = 800, type='response')
boost_pred_class_0.01_test <- apply(boost_pred_0.01_test, 1, which.max)
boost_pred_class_0.001_test <- apply(boost_pred_0.001_test, 1, which.max)

level = c("BARBUNYA", "BOMBAY", "CALI", "DERMASON", "HOROZ", "SEKER","SIRA")
boost_pred_class_0.01_train <- factor(boost_pred_class_0.01_train, levels = c(1:7), labels = level)
boost_pred_class_0.001_train <- factor(boost_pred_class_0.001_train, levels = c(1:7), labels = level)
boost_pred_class_0.01_test <- factor(boost_pred_class_0.01_test, levels = c(1:7), labels = level)
boost_pred_class_0.001_test <- factor(boost_pred_class_0.001_test, levels = c(1:7), labels = level)

confusionMatrix(boost_pred_class_0.01_train, train_data$Class)
confusionMatrix(boost_pred_class_0.001_train, train_data$Class)

confusionMatrix(boost_pred_class_0.01_test, test_data$Class)
confusionMatrix(boost_pred_class_0.001_test, test_data$Class)


#Finally fitting optimal boosting model with number of trees(800), shrinkage(0.01), interaction depth(2)
boost_bean_full <- gbm(as.character(Class) ~ .  
                  , data = data
                  , distribution="multinomial"   
                  , n.trees= 800
                  , shrinkage=0.01
                  , cv.folds=10
                  , verbose=TRUE
                  , interaction.depth = 2)

#Checking accuracy for optimal boosting model
boost_full_pred = predict.gbm(boost_bean_full,data, n.trees = 1000, type='response')
boost_full_class <- apply(boost_full_pred, 1, which.max)
level = c("BARBUNYA", "BOMBAY", "CALI", "DERMASON", "HOROZ", "SEKER","SIRA")
boost_full_class <- factor(boost_full_class, levels = c(1:7), labels = level)
confusionMatrix(boost_full_class, data$Class)

