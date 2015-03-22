Practical Machine Learning - Project
======================================
load libraries for analysis


```r
library(caret)
```

```
## Warning: package 'caret' was built under R version 3.1.3
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
library("randomForest")
```

```
## Warning: package 'randomForest' was built under R version 3.1.3
```

```
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```
Loading the train data set

```r
train_data<-read.csv('C:/Users/tc592u/Documents/Training - Machine Learning/pml-training.csv')
```

Examining the variables in the data set in order to understand which ones are relevant for our prediction


```r
summary(train_data)
```

Preprocessing - eliminating variables: 
1.Near zero variance variables
2.variables which contain mostly (=over 95%) NA
3.Variables which represent timestamps
4.First variable with row indecies


```r
nzv<-nearZeroVar(train_data[,-160],saveMetrics = TRUE)
train_new<-train_data[,nzv[,4]==FALSE]

sums<-matrix(nrow = 1,ncol = ncol(train_new)-1)
for (i in 1:ncol(train_new)-1)  sums[i]<-sum(is.na(train_new[,i]))
train_no_na<-train_new[,(sums < round(95/100*(dim(train_new)[1]),0))]

train_no_time<-train_no_na[,names(train_no_na)[!grepl("time",names(train_no_na))]]

train_no_index<-train_no_time[,-1]
```

How many variables were we left with after preprocessing? 


```r
dim(train_no_index)[2]
```

```
## [1] 55
```

Splitting data to a train and test set, using Create data partition, with the class variable as the parting variable


```r
part_index<-createDataPartition(train_no_index$class, p = 0.50,list=FALSE)
train_no_index_1 = train_no_index[part_index,]
train_no_index_2 = train_no_index[-part_index,]
```

Fitting a random forest model on the train data set using randomForest library and printing a confusion matrix of predicted calss values as opposed to the observed class vaiable in the train dataset


```r
rf1<-randomForest(formula = classe~., data = train_no_index_1)
print(rf1)
```

```
## 
## Call:
##  randomForest(formula = classe ~ ., data = train_no_index_1) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 7
## 
##         OOB estimate of  error rate: 0.58%
## Confusion matrix:
##      A    B    C    D    E class.error
## A 2790    0    0    0    0 0.000000000
## B    8 1887    4    0    0 0.006319115
## C    0   12 1698    1    0 0.007597896
## D    0    0   23 1584    1 0.014925373
## E    0    0    0    8 1796 0.004434590
```

Using the fitted random forest model on the second split of the train data set, in order to check performance


```r
pred<-predict(rf1, newdata=train_no_index_2[-55])
predRight<-pred==train_no_index_2$class
table(pred,train_no_index_2$class)
```

```
##     
## pred    A    B    C    D    E
##    A 2790    7    0    0    0
##    B    0 1890    8    0    0
##    C    0    1 1697    9    0
##    D    0    0    6 1599    3
##    E    0    0    0    0 1800
```

```r
accuracy_of_rf1<-sum(predRight)/length(predRight)
out_of_sample_error_of_rf1<-(1-accuracy_of_rf1)
print(accuracy_of_rf1)
```

```
## [1] 0.9965341
```

```r
print(out_of_sample_error_of_rf1)
```

```
## [1] 0.003465851
```

After the model showed a high accuracy metric and a low out of sample error (regarding the split test data), I now upload the data set for submission in order to compute predictions

First, loading the data, then doing the preprocessing steps as I did on the training data set. 


```r
submission_data<-read.csv('C:/Users/tc592u/Documents/Training - Machine Learning/pml-testing.csv')


submission_nzv<-nearZeroVar(submission_data[,-160],saveMetrics = TRUE)
submission_new<-submission_data[,submission_nzv[,4]==FALSE]

submission_sums<-matrix(nrow = 1,ncol = ncol(submission_new)-1)
for (i in 1:ncol(submission_new)-1)  submission_sums[i]<-sum(is.na(submission_new[,i]))
submission_no_na<-submission_new[,(submission_sums < round(95/100*(dim(submission_new)[1]),0))]

submission_no_time<-submission_no_na[,names(submission_no_na)[!grepl("time",names(submission_no_na))]]

submission_no_index<-submission_no_time[,-1]
```

How many variables were we left with after preprocessing the submission data? 


```r
dim(submission_no_index)[2]
```

```
## [1] 55
```

Predicting the class outcome based on out randomForest model, on the submission ('test') data set


```r
submission_pred<-predict(rf1, newdata=submission_no_index)
```

Writing 20 files with one prediction answer in each using the function "pml_write_files"


```r
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(submission_pred)
```

