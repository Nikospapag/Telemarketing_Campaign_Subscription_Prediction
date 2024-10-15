#########ΒΑΖΟΥΜΕ ΤΙΣ LIBRARY ΠΟΥ ΘΕΛΟΥΜΕ:
library(readxl)
file_path <- "C:/Users/NIKOS/Downloads/project 2 2023-2024/project 2 2023-2024.xls"
data <- read_excel(file_path)
library(MASS)
library(caret)
library(pROC)
library(e1071)
data<-as.data.frame(data)
library(penalizedLDA)
library(class)

####MERIKA TESTS
head(data)
summary(data)
count_999 <- sum(data$pdays == 999)
print(count_999)
poutcome_nonex<-sum(data$poutcome=="nonexistent")
print(poutcome_nonex)




###########################Χωριζουμε τα data ωστε μετα να ελεξγουμε τα μοντελα
set.seed(121)
random <- sample(1:39883, 31000)
selected_data <- data[random, ]
remaining <- setdiff(1:39883, random)
remaining_data <- data[remaining, ]

####################Με αυτην θα λειτουργήσουμε
df2<-selected_data[,-c(6,13,16)]

###numeric μεταβλητές για το correlation matrix
dfcor<-data[,-c(2:10,15)]
dfcor$SUBSCRIBED <-(ifelse(dfcor$SUBSCRIBED == "no", 1, 2))  ###Μετατρεπουμε την SUBSCRIBED
correlation_matrix<-cor(dfcor) ###Βλεπουμε ποιες εχουν στενες συσχετισεις μεταξυ τους και ταυτοχρονα ποιες εχουν ισχυρη συσχετιση με τη μεταβλητη SUBSCRIBED
print(correlation_matrix)


###########################################################LDA:
set.seed(121)
df2<-selected_data[,-c(6,13,16)] ### Μετα απο δοκιμες παρατηρησα οτι η μεταβλητή 6 πρέπει να αφαιρεθεί για να μην εχουμε collinear warning
###Πρωτη απλη εφαρμογη lda μεθοδου:
e1<-lda(SUBSCRIBED~., data=df2)  
e2<-predict(e1)
plot(e1)
t1<-table(df2[,18],e2$class)
conf_matrix <- table(df2[, 18], e2$class)
conf_matrix

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
specificity <- conf_matrix[1, 1] / sum(conf_matrix[1, ])
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
f1_score <- 2 * (precision * sensitivity) / (precision + sensitivity)
cat("Accuracy:", accuracy, "\n") 
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")
cat("Precision:", precision, "\n") 
cat("F1-Score:", f1_score, "\n")



random2 <- sample(1:31000, 600)
selected_data2 <- selected_data[random2, ]
sub_data<-selected_data2[,-c(6,13,16)]	  ###Μικρό σύνολο δεδομένων κυριως για γραφηματα
e10<-lda(SUBSCRIBED~., data=sub_data)
e11<-predict(e10)
colors <- ifelse(sub_data[,18] == "yes", "green", "red")
plot(e11$x,col=colors, cex=1.5)




#####ROC
predicted_probabilities <- predict(e1, type = "response")$posterior[, "yes"]
roc_curve <- roc(df2$SUBSCRIBED, predicted_probabilities)
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2, legacy.axes = TRUE)


new_predictions <- ifelse(predicted_probabilities >= 0.30, "yes", "no")
new_conf_matrix <- table(df2$SUBSCRIBED, new_predictions)
new_conf_matrix


# Υπολογισμός νέων στατιστικών απόδοσης
new_accuracy <- sum(diag(new_conf_matrix)) / sum(new_conf_matrix)
new_sensitivity <- new_conf_matrix[2, 2] / sum(new_conf_matrix[2, ])
new_specificity <- new_conf_matrix[1, 1] / sum(new_conf_matrix[1, ])
new_precision <- new_conf_matrix[2, 2] / sum(new_conf_matrix[, 2])
new_f1_score <- 2 * (new_precision * new_sensitivity) / (new_precision + new_sensitivity)

# Εκτύπωση των νέων στατιστικών
cat("New Accuracy:", new_accuracy, "\n")
cat("New Sensitivity:", new_sensitivity, "\n")
cat("New Specificity:", new_specificity, "\n")
cat("New Precision:", new_precision, "\n")
cat("New F1 Score:", new_f1_score, "\n")

df_without<-df2[,-c(1,3,15)]   ###Αφαιρουμε μεταβλητες με χαμηλη συσχετιση με την SUBSCRIBED
e1<-lda(SUBSCRIBED~., data=df_without)
predicted_probabilities <- predict(e1, type = "response")$posterior[, "yes"]
new_predictions <- ifelse(predicted_probabilities >= 0.30, "yes", "no")
conf_matrix <- table(df_without[, 15], new_predictions)
conf_matrix
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
specificity <- conf_matrix[1, 1] / sum(conf_matrix[1, ])
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
f1_score <- 2 * (precision * sensitivity) / (precision + sensitivity)
cat("Accuracy:", accuracy, "\n") 
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")
cat("Precision:", precision, "\n") 
cat("F1-Score:", f1_score, "\n")



#########################################################LDA TRAIN-TEST:
set.seed(121)
ind<-createDataPartition(df2$SUBSCRIBED,p=0.8,list=FALSE)
train<- df2[ind,]
test<-df2[-ind,]

e4<-lda(SUBSCRIBED~.,data=train)
plot(e4)
predicted_probabilities <- predict(e4, newdata = test, type = "response")$posterior[, "yes"]
new_predictions <- ifelse(predicted_probabilities >= 0.30, "yes", "no")

# Υπολογισμός confusion matrix
conf_matrix2 <- table(test$SUBSCRIBED, new_predictions)

# Υπολογισμός ακρίβειας (accuracy)
accuracy2 <- sum(diag(conf_matrix2)) / sum(conf_matrix2)


# Υπολογισμός ευαισθησίας (sensitivity)
sensitivity2 <- conf_matrix2[2, 2] / sum(conf_matrix2[2, ])

# Υπολογισμός ειδικότητας (specificity)
specificity2 <- conf_matrix2[1, 1] / sum(conf_matrix2[1, ])

# Υπολογισμός precision
precision2 <- conf_matrix2[2, 2] / sum(conf_matrix2[, 2])

# Υπολογισμός F1-score
f1_score2 <- 2 * (precision2 * sensitivity2) / (precision2 + sensitivity2)

# Εκτύπωση των μετρικών
cat("Accuracy:", accuracy2, "\n")
cat("Sensitivity:", sensitivity2, "\n")
cat("Specificity:", specificity2, "\n")
cat("Precision:", precision2, "\n")
cat("F1 Score:", f1_score2, "\n")




colors <- ifelse(new_predictions == "no", "green", "red")
plot(predicted_probabilities, col = colors, main = "LDA Train-test", xlab = "Observation", ylab = "Probability of 'no'")



###Αφαιρουμε τις μεταβλητες με χαμηλη συσχετιση με την SUBSCRIBED
set.seed(121)
ind<-createDataPartition(df_without$SUBSCRIBED,p=0.8,list=FALSE)
train<- df_without[ind,]
test<-df_without[-ind,]
e4<-lda(SUBSCRIBED~.,data=train)
predicted_probabilities <- predict(e4, newdata = test, type = "response")$posterior[, "yes"]
new_predictions <- ifelse(predicted_probabilities >= 0.30, "yes", "no")
####Παρατηρουμε αλλαγες
conf_matrix2 <- table(test$SUBSCRIBED, new_predictions)
accuracy2 <- sum(diag(conf_matrix2)) / sum(conf_matrix2)
sensitivity2 <- conf_matrix2[2, 2] / sum(conf_matrix2[2, ])
specificity2 <- conf_matrix2[1, 1] / sum(conf_matrix2[1, ])
precision2 <- conf_matrix2[2, 2] / sum(conf_matrix2[, 2])
f1_score2 <- 2 * (precision2 * sensitivity2) / (precision2 + sensitivity2)

cat("Accuracy:", accuracy2, "\n")
cat("Sensitivity:", sensitivity2, "\n")
cat("Specificity:", specificity2, "\n")
cat("Precision:", precision2, "\n")
cat("F1 Score:", f1_score2, "\n")



#######################################################LDA CROSS-VALIDATION:
set.seed(121)

e3<-lda(SUBSCRIBED~., data=df2, CV=TRUE)
predicted_probabilities <- e3$posterior[, "yes"]

new_predictions <- ifelse(predicted_probabilities >= 0.30, "yes", "no")



conf_matrix_cv <- table(df2$SUBSCRIBED, new_predictions)

# Υπολογισμός ακρίβειας (accuracy)
accuracy_cv <- sum(diag(conf_matrix_cv)) / sum(conf_matrix_cv)

# Υπολογισμός ευαισθησίας (sensitivity)
sensitivity_cv <- conf_matrix_cv[2, 2] / sum(conf_matrix_cv[2, ])

# Υπολογισμός ειδικότητας (specificity)
specificity_cv <- conf_matrix_cv[1, 1] / sum(conf_matrix_cv[1, ])

# Υπολογισμός precision
precision_cv <- conf_matrix_cv[2, 2] / sum(conf_matrix_cv[, 2])

# Υπολογισμός F1-score
f1_score_cv <- 2 * (precision_cv * sensitivity_cv) / (precision_cv + sensitivity_cv)

# Εκτύπωση των μετρικών
cat("Accuracy (Cross-Validation):", accuracy_cv, "\n")
cat("Sensitivity (Cross-Validation):", sensitivity_cv, "\n")
cat("Specificity (Cross-Validation):", specificity_cv, "\n")
cat("Precision (Cross-Validation):", precision_cv, "\n")
cat("F1 Score (Cross-Validation):", f1_score_cv, "\n")



colors <- ifelse(new_predictions == "no", "green", "red")
plot(predicted_probabilities, col = colors, main = "LDA Cross-Validation", xlab = "Observation", ylab = "Probability of 'no'")



###Αφαιρουμε τις μεταβλητες με χαμηλη συσχετιση με την SUBSCRIBED
e3<-lda(SUBSCRIBED~., data=df_without, CV=TRUE)
predicted_probabilities <- e3$posterior[, "yes"]
new_predictions <- ifelse(predicted_probabilities >= 0.30, "yes", "no")
conf_matrix_cv <- table(df_without$SUBSCRIBED, new_predictions)
accuracy_cv <- sum(diag(conf_matrix_cv)) / sum(conf_matrix_cv)
sensitivity_cv <- conf_matrix_cv[2, 2] / sum(conf_matrix_cv[2, ])
specificity_cv <- conf_matrix_cv[1, 1] / sum(conf_matrix_cv[1, ])
precision_cv <- conf_matrix_cv[2, 2] / sum(conf_matrix_cv[, 2])
f1_score_cv <- 2 * (precision_cv * sensitivity_cv) / (precision_cv + sensitivity_cv)

cat("Accuracy (Cross-Validation):", accuracy_cv, "\n")
cat("Sensitivity (Cross-Validation):", sensitivity_cv, "\n")
cat("Specificity (Cross-Validation):", specificity_cv, "\n")
cat("Precision (Cross-Validation):", precision_cv, "\n")
cat("F1 Score (Cross-Validation):", f1_score_cv, "\n")










##############################################################Penalized LDA

df1<-df2[,-c(2:9,13)]###μονο οι numeric kai subscribed



library(penalizedLDA)
df1$SUBSCRIBED <- ifelse(df1$SUBSCRIBED == "yes", 1, 2)
pen<-PenalizedLDA(df1[,-9],df1[,9],lambda=0.1,xte=df1[,-9],K=1)
pen$discrim ### Δειχνει πως και ποσο τα χαρακτηριστικα επηρεαζουν την διαδικασια και τον τροπο με τον οποιο γινεται η διακριση των διαφορετικων κλασεων yes or no
set.seed(121)
index<-createDataPartition(df1$SUBSCRIBED,p=0.8,list=FALSE)
train<- df1[index,]
test<- df1[-index,]
score<-NULL
possiblelambda<- seq(0.05,0.25,by=0.01) 
for (lam in possiblelambda) {
  pen<-PenalizedLDA(train[,-9],train[,9],lambda=lam,
                    xte=test[,-9],K=1)
  pen$discrim
  pen$ypred
  t<-table(test[,9],pen$ypred[,1])
  score<-c(score,sum(diag(t))/sum(t))
}
cbind(possiblelambda,score)    ###Βλεπουμε το βελτιστο και τιμες για ολα τα lambda και μετα κανουμε δοκιμες

###Βλεπουμε και ποιες μεταβλητες μηδενιζονται οσο αυξανεται η τιμη του lambda(πιθανες για να διωξουμε)
pen<-PenalizedLDA(df1[,-9],df1[,9],lambda=0.16,xte=df1[,-9],K=1)  
pen$discrim

pen<-PenalizedLDA(df1[,-9],df1[,9],lambda=0.18,xte=df1[,-9],K=1)
pen$discrim

pen<-PenalizedLDA(df1[,-9],df1[,9],lambda=0.25,xte=df1[,-9],K=1)
pen$discrim

pen<-PenalizedLDA.cv(df1[,-9],df1[,9],lambdas=c(0.16,0.18,0.25),K=1,nfold = 10)  ###Cross validation με 10 folds για τρεις διαφορετικες τιμες lambda 
print(pen)


#######################################################################################SVM
set.seed(121)

data.SVM<-df2
data.SVM$SUBSCRIBED <- as.factor(ifelse(data.SVM$SUBSCRIBED == "no", 0, 1))
set.seed(121)
param_grid <- expand.grid(C = c(0.5, 1, 3), gamma = c(0.7, 1.5, 2))

svm_tune <- tune(svm, SUBSCRIBED ~ ., data = data.SVM, kernel = "linear",
                 ranges = list(C = c(0.5, 1, 3), gamma = c(0.7, 1.5, 2)),
                 tunecontrol = tune.control(sampling = "cross", cross = 10))

print(svm_tune)


best_svm <- svm(SUBSCRIBED ~ ., data = data.SVM, kernel = "linear", C = 0.5, gamma = 0.7)
predictions <- predict(best_svm, newdata = data.SVM)

accuracy <- mean(predictions == data.SVM$SUBSCRIBED)
print(paste("Accuracy:", accuracy))









#######################################################################################RandomForest:
set.seed(121)

library(randomForest)
data_rf<-df2
οοb<-NULL
for (ntree in c(50, 100, 200, 300, 400, 500)) {
  for (mtry in c(3, 4, 5, 6)) {
    myRF <- randomForest(as.factor(SUBSCRIBED) ~ ., data = data_rf, ntree = ntree, mtry = mtry)
    oob_error <- myRF$err.rate[ntree, 1]
    cat("ntree:", ntree, ", mtry:", mtry, ", OOB Error:", oob_error, "\n")
    
  }
}

####vriskoume to xamhlotero px 6,400




myRF<- randomForest(as.factor(SUBSCRIBED) ~ ., data=data_rf, ntree=400,
                    mtry=6, importance=TRUE)
myRF
plot(myRF)
myRF$predicted
myRF$importance
myRF$err.rate
myRF$votes
predict(myRF,data_rf)

################################ΣΥΓΚΡΙΝΟΥΜΕ:
set.seed(121)

df_without2<-df2[,-c(5,6,7,12)]
myRF<- randomForest(as.factor(SUBSCRIBED) ~ ., data=df_without2, ntree=400,mtry=6, importance=TRUE)
myRF
myRF$importance




######################################################################################KNN:
set.seed(121)
library(class)
data_knn<-df2[,-c(2:9,13)]
data_knn2<- cbind(data_knn[,9],scale(data_knn[,-9]))
### knn with k=3, initial data
km3<-knn(data_knn[,-9],data_knn[,-9], cl=data_knn[,9],k=3)
### knn with k=3, standardize data
km3scaled<-knn(data_knn2[,-1],data_knn2[,-1], cl=data_knn2[,1],k=3)
table(data_knn2[,1],km3scaled)
### knn with k=5
km5<-knn(data_knn[,-9],data_knn[,-9], cl=data_knn[,9],k=5)
km5scaled<-knn(data_knn2[,-1],data_knn2[,-1], cl=data_knn2[,1],k=5)
table(data_knn2[,1],km5scaled)

result_cv <- knn.cv(data_knn2[, -1], data_knn2[, 1], k = 3)
true_labels <- data_knn2[, 1]
error_rate <- sum(result_cv != true_labels) / length(true_labels)
cat("Error Rate:", error_rate, "\n")
accuracy <- sum(result_cv == true_labels) / length(true_labels)
accuracy


result_cv <- knn.cv(data_knn2[, -1], data_knn2[, 1], k = 5)
true_labels <- data_knn2[, 1]
error_rate <- sum(result_cv != true_labels) / length(true_labels)
cat("Error Rate:", error_rate, "\n")
accuracy <- sum(result_cv == true_labels) / length(true_labels)
accuracy



####Μπορουμε μεσω της caret να εντοπισουμε τη καλυτερη τιμη του k
ctrl <- trainControl(method = "cv", number = 10)
grid <- expand.grid(k = 1:10)
knn_model <- train(data_knn2[, -1], data_knn2[, 1], method = "knn", trControl = ctrl, tuneGrid = grid)
print(knn_model)
####Αρα το k=9 
result_cv <- knn.cv(data_knn2[, -1], data_knn2[, 1], k = 9)
true_labels <- data_knn2[, 1]
error_rate <- sum(result_cv != true_labels) / length(true_labels)
cat("Error Rate:", error_rate, "\n")
accuracy <- sum(result_cv == true_labels) / length(true_labels)
accuracy



##################################################################
ΣΥΓΚΡΙΣΕΙΣ ΜΕΘΟΔΩΝ:
  ###################Τωρα θα χρησιμοποιησουμε τα remaining_data
  
  dc<-remaining_data[,-c(6,13,16)]
################################################################LDA C/V:
set.seed(121)

ec<-lda(SUBSCRIBED~., data=dc, CV=TRUE)
predicted_probabilities <- ec$posterior[, "yes"]
new_predictions <- ifelse(predicted_probabilities >= 0.30, "yes", "no")
conf_matrix_cv <- table(dc$SUBSCRIBED, new_predictions)
conf_matrix_cv


# Υπολογισμός ακρίβειας (accuracy)
accuracy_cv <- sum(diag(conf_matrix_cv)) / sum(conf_matrix_cv)

# Υπολογισμός ευαισθησίας (sensitivity)
sensitivity_cv <- conf_matrix_cv[2, 2] / sum(conf_matrix_cv[2, ])

# Υπολογισμός ειδικότητας (specificity)
specificity_cv <- conf_matrix_cv[1, 1] / sum(conf_matrix_cv[1, ])

# Υπολογισμός precision
precision_cv <- conf_matrix_cv[2, 2] / sum(conf_matrix_cv[, 2])

# Υπολογισμός F1-score
f1_score_cv <- 2 * (precision_cv * sensitivity_cv) / (precision_cv + sensitivity_cv)

# Εκτύπωση των μετρικών
cat("Accuracy (Cross-Validation):", accuracy_cv, "\n")
cat("Sensitivity (Cross-Validation):", sensitivity_cv, "\n")
cat("Specificity (Cross-Validation):", specificity_cv, "\n")
cat("Precision (Cross-Validation):", precision_cv, "\n")
cat("F1 Score (Cross-Validation):", f1_score_cv, "\n")


colors <- ifelse(new_predictions == "no", "green", "red")
plot(predicted_probabilities, col = colors, main = "LDA Cross-Validation", xlab = "Observation", ylab = "Probability of 'no'")


####################################################################### Αφαίρεση age campaign
dc2<-dc[,-c(1,11)]### Αφαίρεση age campaign
ec<-lda(SUBSCRIBED~., data=dc2, CV=TRUE)
predicted_probabilities <- ec$posterior[, "yes"]
new_predictions <- ifelse(predicted_probabilities >= 0.30, "yes", "no")
conf_matrix_cv <- table(dc$SUBSCRIBED, new_predictions)
conf_matrix_cv

# Υπολογισμός ακρίβειας (accuracy)
accuracy_cv <- sum(diag(conf_matrix_cv)) / sum(conf_matrix_cv)

# Υπολογισμός ευαισθησίας (sensitivity)
sensitivity_cv <- conf_matrix_cv[2, 2] / sum(conf_matrix_cv[2, ])

# Υπολογισμός ειδικότητας (specificity)
specificity_cv <- conf_matrix_cv[1, 1] / sum(conf_matrix_cv[1, ])

# Υπολογισμός precision
precision_cv <- conf_matrix_cv[2, 2] / sum(conf_matrix_cv[, 2])

# Υπολογισμός F1-score
f1_score_cv <- 2 * (precision_cv * sensitivity_cv) / (precision_cv + sensitivity_cv)

# Εκτύπωση των μετρικών
cat("Accuracy (Cross-Validation):", accuracy_cv, "\n")
cat("Sensitivity (Cross-Validation):", sensitivity_cv, "\n")
cat("Specificity (Cross-Validation):", specificity_cv, "\n")
cat("Precision (Cross-Validation):", precision_cv, "\n")
cat("F1 Score (Cross-Validation):", f1_score_cv, "\n")




###################################################################Penalized
df1<-dc
df1<-df1[,-c(2:9,13)]
df1$SUBSCRIBED <- ifelse(df1$SUBSCRIBED == "yes", 1, 2)
set.seed(121)
index<-createDataPartition(df1$SUBSCRIBED,p=0.8,list=FALSE)
train<- df1[index,]
test<- df1[-index,]
score<-NULL
possiblelambda<- seq(0.05,0.25,by=0.01) 
for (lam in possiblelambda) {
  pen<-PenalizedLDA(train[,-9],train[,9],lambda=lam,
                    xte=test[,-9],K=1)
  pen$discrim
  pen$ypred
  t<-table(test[,9],pen$ypred[,1])
  score<-c(score,sum(diag(t))/sum(t))
}
cbind(possiblelambda,score)    ###Βλεπουμε το βελτιστο και τιμες για ολα τα lambda και μετα κανουμε δοκιμες

pen<-PenalizedLDA.cv(df1[,-9],df1[,9],lambdas=c(0.16,0.18,0.25),K=1,nfold = 10)  ###Cross validation με 10 folds για τρεις διαφορετικες τιμες lambda 
print(pen)
###############################################################RandomForest
set.seed(121)

library(randomForest)
data_rf<-dc
οοb<-NULL
for (ntree in c(50, 100, 200, 300, 400, 500)) {
  for (mtry in c(3, 4, 5, 6)) {
    myRF <- randomForest(as.factor(SUBSCRIBED) ~ ., data = data_rf, ntree = ntree, mtry = mtry)
    oob_error <- myRF$err.rate[ntree, 1]
    cat("ntree:", ntree, ", mtry:", mtry, ", OOB Error:", oob_error, "\n")
    
  }
}


####vriskoume to xamhlotero px 5,300


myRF<- randomForest(as.factor(SUBSCRIBED) ~ ., data=data_rf, ntree=300,
                    mtry=5, importance=TRUE)
myRF
plot(myRF)
myRF$predicted
myRF$importance
myRF$err.rate
myRF$votes
predict(myRF,data_rf)



#######Αφαιρουμε με τις αδύναμες μεταβλητές 
data_rf2<-data_rf[,-c(5,6,7,12)]
myRF<- randomForest(as.factor(SUBSCRIBED) ~ ., data=data_rf2, ntree=300,
                    mtry=5, importance=TRUE)
myRF
plot(myRF)
myRF$predicted
myRF$importance
myRF$err.rate
myRF$votes
predict(myRF,data_rf)




###############################################################################KNN
set.seed(121)


dr<-dc[,-c(2:9,13)]
dr2<- cbind(dr[,9],scale(dr[,-9]))

####Μπορουμε μεσω της caret να εντοπισουμε τη καλυτερη τιμη του k
ctrl <- trainControl(method = "cv", number = 10)
grid <- expand.grid(k = 1:10)
knn_model <- train(dr2[, -1], dr2[, 1], method = "knn", trControl = ctrl, tuneGrid = grid)
print(knn_model)


result_cv <- knn.cv(dr2[, -1], dr2[, 1], k = 9)
true_labels <- dr2[, 1]
error_rate <- sum(result_cv != true_labels) / length(true_labels)
cat("Error Rate:", error_rate, "\n")
accuracy <- sum(result_cv == true_labels) / length(true_labels)
accuracy




################################################################################SVM:
set.seed(121)

data.SVM<-dc
data.SVM$SUBSCRIBED <- as.factor(ifelse(data.SVM$SUBSCRIBED == "no", 0, 1))
set.seed(121)
param_grid <- expand.grid(C = c(0.5, 1, 3), gamma = c(0.7, 1.5, 2))
svm_tune <- tune(svm, SUBSCRIBED~ ., data = data.SVM, kernel = "linear",
                 ranges = list(C = c(0.5, 1, 3), gamma = c(0.7, 1.5, 2)),
                 tunecontrol = tune.control(sampling = "cross", cross = 10))
svm_tune



best_svm <- svm(SUBSCRIBED ~ ., data = data.SVM, kernel = "linear", C = 0.5, gamma = 0.7)
predictions <- predict(best_svm, newdata = data.SVM)
accuracy <- mean(predictions == data.SVM$SUBSCRIBED)
print(paste("Accuracy:", accuracy))
