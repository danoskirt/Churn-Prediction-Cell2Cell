

#Loading libraries
#library(class)
#install.packages("dplyr")
#library(dplyr)
#install.packages("tidyr")
#library(tidyr)
#install.packages("caret")
#library(caret)
#install.packages("e1071")
#library(e1071)
#install.packages("Hmisc")
#library(Hmisc)
#install.packages("lattice")
#library(lattice)
#install.packages("ggplot2")
#library(ggplot2)
#install.packages("ggcorrplot")
#library(ggcorrplot)
#install.packages("mlbench")
#library(mlbench)
#install.packages("randomForest")
#library(randomForest)
#install.packages("varImp")
#library(varImp)
#install.packages("C50")
#library(C50)
#install.packages("caTools")
#library(caTools)
#install.packages("edfReader")
#library(edfReader)
#install.packages("eegkit")
#library(eegkit)
#install.packages("rlist")
#library(rlist)
#install.packages("ipred")
#library(ipred)
install.packages("rlang")
package_list <- c("class","dplyr","tidyr","caret","e1071","Hmisc",
                  "lattice","ggplot2","ggcorrplot","mlbench","randomForest",
                  "varImp","C50","caTools","edfReader","eegkit","rlist","ipred", "rlang")
lapply(package_list, require, character.only = TRUE)

input_file_path <-  "ml/cell2cell.csv"
output_file_path <- "ml/model_output.txt"

#DATA COLLECTION
#Loading the data on R
cell1 <- read.csv(file = input_file_path, header = T, stringsAsFactors = FALSE)


#Reorganizing the columns with target field as the last column
cell2 <- cell1 %>% select(CustomerID,MonthlyRevenue, MonthlyMinutes, TotalRecurringCharge, DirectorAssistedCalls, OverageMinutes, RoamingCalls,PercChangeMinutes, PercChangeRevenues, DroppedCalls, BlockedCalls, UnansweredCalls, CustomerCareCalls, ThreewayCalls, ReceivedCalls,OutboundCalls, InboundCalls, PeakCallsInOut, OffPeakCallsInOut, DroppedBlockedCalls, CallForwardingCalls, CallWaitingCalls, MonthsInService,UniqueSubs, ActiveSubs, ServiceArea, Handsets, HandsetModels, CurrentEquipmentDays, AgeHH1, AgeHH2, ChildrenInHH, HandsetRefurbished,HandsetWebCapable, TruckOwner, RVOwner, Homeownership, BuysViaMailOrder, RespondsToMailOffers, OptOutMailings, NonUSTravel, OwnsComputer,HasCreditCard, RetentionCalls, RetentionOffersAccepted, NewCellphoneUser, NotNewCellphoneUser, ReferralsMadeBySubscriber, IncomeGroup,OwnsMotorcycle, AdjustmentsToCreditRating, HandsetPrice, MadeCallToRetentionTeam, CreditRating, PrizmCode,Occupation, MaritalStatus,Churn )
#view the dataset
View(cell2)



#DATA PREPARATION
#summary statistics
Data <- cell2 #Naming the data frame Data
summary(Data) #summarize values in the vectors
describe(Data) #contingency table to understand the data
#To check the distribution of target variable
percentage <- prop.table(table(Data$Churn)) * 100
percentage
cbind(freq=table(Data$Churn), percentage= percentage)

#DATA PRE-PROCESSING

#Deleting Marital status and Customer Id
Data_Marital<- Data%>%
  mutate(MaritalStatus= na_if(MaritalStatus,"Unknown")) #converting the "unknown" values to "na"
Data %>% group_by(MaritalStatus) %>% count() #Check the count of each unique values present in the vector
Data$MaritalStatus <- NULL #delete the vector- "MaritalStatus" as it has 19201 "na"
Data$CustomerID <- NULL #delete the vector- "CustomerID" as it does not have impact on the analysis
View(Data) #view the data frame after deleting two vectors

#To remove missing values
Data <- na.omit(Data) #omitting "na" from the data frame, 2000 rows in this case
View(Data) #viewing the dataframe to confirm there are no "na"


#Converting vectors from categorical to Numeric format
for (i in 1:(ncol(Data)-1)) {
  if (is.character(Data[, i])==TRUE){
    for(j in 1:nrow(Data)) {
      ascis <- as.numeric(charToRaw(Data[j, i]))
      Data[ j, i] <- sum(ascis)
    }
  }
  Data[,i] <- as.numeric(Data[,i])
}
Data[,ncol(Data)] <- as.factor(Data[,ncol(Data)])
#create variable x- attach to it the input attributes of the dataset
x  <- Data[,1:ncol(Data)-1]
#create variable y- attach to it the output attribute of the dataset
y <- Data[,ncol(Data)]


#EXPLORATORY DATA ANALYSIS
#Bar graph for target variable- Churn
Churn <- table(Data$Churn)
barplot(Churn, main = "Plot of Target Field", xlab = "Churn", ylab = "Churn Count")
#scatterplot for monthly revenue vs monthly minutes with target field (churn)
ggplot(Data, aes(x = MonthlyRevenue, y = MonthlyMinutes, color = y)) +
  geom_point() + ggtitle("Scatterplot MonthlyRevenue vs MonthlyMinutes")
#boxplot for first 6 variables
par(mfrow=c(1,6))
for(i in 1:6) {
  boxplot(x[,i], main=names(Data)[i])
}
#scatterplot with dropped calls and blocked calls
ggplot(Data, aes(x = DroppedCalls, y = BlockedCalls)) +
  geom_point()+ ggtitle("Scatterplot DroppedCalls vs BlockedCalls")
#using lineplot for outbound calls and inbound calls without churn
ggplot(Data, aes(x = OutboundCalls, y = InboundCalls)) +
  geom_line( colour = "blue", size = 1)+ ggtitle("Scatterplot OutboundCalls vs  InboundCalls")
#scatter plot for PeakCallsInOut vs OverageMinutes for different Churn categories
ggplot(data = Data,
       mapping = aes(x = PeakCallsInOut,
                     y = OverageMinutes,
                     color = Churn)) +
  geom_point(alpha = .7,
             size = 3) +
  geom_smooth(method = "lm",
              se = FALSE,
              size = 1.5, formula= y ~ x, color= "black") + ggtitle("Scatterplot PeakCallsInOut vs  OverageMinutes")



#MODELING

#Splitting the data into test and train
set.seed(150)
split <- sample.split(Data, SplitRatio = 0.7)
t <- 0.70
train_cl <- subset(Data, split == "TRUE")
test_cl <- subset(Data, split == "FALSE")   


#C5 Model using all variables
#cat function for printing #sep = specified separator

tree_model_55 <- C5.0(Churn~., data = train_cl) #applying tree-based model on the training data

message ("-----------------------")
message (" Training Performance")
message ("-----------------------")
print(tree_model_55) #viewing the model
summary(tree_model_55) #printing the output of the model
plot(tree_model_55) #graphical presentation of the model




Predictions <- predict(tree_model_55, newdata = test_cl) #testing the outcome of training data on the testing data
cm_C5_55 <- confusionMatrix(test_cl$Churn, Predictions) #creating confusion matrix for Positives and negatives
message ("-----------------------")
message (" Testing Performance")
message ("-----------------------")
print(cm_C5_55) #printing the confusion matrix

cat("\n\n","C5 USING ALL VARIABLES WITHOUT OPTIMIZATION","\n\n",
    "*****************************************************************************************************************************","\n","train_percent", "\t", "test_percent", "\t ",
    "Accuracy", "\t ", "Kappa", "\t\t ", "Sensitivity", "\t ", "Specificity", "\t ", "Precision", "\t ", "Recall","\n",
    "*****************************************************************************************************************************","\n",
    file = output_file_path, sep = " ", append = TRUE)


# write evaluation metrics to txt file
cat( t*100, "\t\t ", (1-t)*100, "\t\t ",
     format(round(cm_C5_55[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_C5_55[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_C5_55[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_C5_55[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_C5_55[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_C5_55[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n" ,"\n",
     file = output_file_path, sep = " ", append = TRUE)


#Random Forest using all variables
set.seed(123)
model_RF55 <- randomForest(Churn ~ ., data = train_cl, ntree = 500, mtry = 5)
message ("-----------------------")
message (" Training Performance")
message ("-----------------------")
model_RF55
plot(model_RF55)


prediction_RF55 <- predict(model_RF55, newdata = test_cl) # The next step is to validate our model using the test data

cm_RF55 <-confusionMatrix(test_cl$Churn,prediction_RF55)

message ("-----------------------")
message (" Testing Performance")
message ("-----------------------")

print(cm_RF55)

cat("\n\n","RANDOM FOREST USING ALL VARIABLES WITHOUT OPTIMIZATION","\n\n",
    "*****************************************************************************************************************************","\n","train_percent", "\t", "test_percent", "\t ",
    "Accuracy", "\t ", "Kappa", "\t\t ", "Sensitivity", "\t ", "Specificity", "\t ", "Precision", "\t ", "Recall","\n",
    "*****************************************************************************************************************************","\n",
    file = output_file_path, sep = " ", append = TRUE)


# write evaluation metrics to txt file
cat( t*100, "\t\t ", (1-t)*100, "\t\t ",
     format(round(cm_RF55[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_RF55[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_RF55[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_RF55[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_RF55[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_RF55[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n" ,"\n",
     file = output_file_path, sep = " ", append = TRUE)




#Naive Bayes-all variables

# Fitting Naive Bayes Model to training dataset
set.seed(120)  # Setting Seed
classifier_cl <- naiveBayes(Churn ~ ., data = train_cl)

message ("-----------------------")
message (" Training Performance")
message ("-----------------------")

classifier_cl

# Predicting on test data'
y_pred <- predict(classifier_cl, newdata = test_cl)
# Confusion Matrix
cm_nb_55 <- confusionMatrix(test_cl$Churn, y_pred)

message ("-----------------------")
message (" Testing Performance")
message ("-----------------------")
print(cm_nb_55)


cat("\n\n","NAIVE BAYES USING ALL VARIABLES WITHOUT OPTIMIZATION","\n\n",
    "*****************************************************************************************************************************","\n","train_percent", "\t", "test_percent", "\t ",
    "Accuracy", "\t ", "Kappa", "\t\t ", "Sensitivity", "\t ", "Specificity", "\t ", "Precision", "\t ", "Recall","\n",
    "*****************************************************************************************************************************","\n",
    file = output_file_path, sep = " ", append = TRUE)


# write evaluation metrics to txt file
cat( t*100, "\t\t ", (1-t)*100, "\t\t ",
     format(round(cm_nb_55[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_nb_55[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_nb_55[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_nb_55[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_nb_55[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_nb_55[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n" ,"\n",
     file = output_file_path, sep = " ", append = TRUE)


##KNN- all random variables
#create list that runs 10-fold cross-validation.
control_knn <- trainControl(method="repeatedcv", repeats = 10)

#create metric_knn to store metric to be used for model evaluation
metric_knn <- "Accuracy"

#convert train set traget to factor
train_cl[,ncol(train_cl)] <- as.factor(train_cl[,ncol(train_cl)])

set.seed(10)
#Build the knn model
library(class)
fit.knn <- knn(train_cl[,1:ncol(test_cl)-1], test_cl[,1:ncol(test_cl)-1],
               train_cl$Churn, k=3, prob=TRUE)

message ("-----------------------")
message (" Training Performance")
message ("-----------------------")
print(fit.knn)



# get confusion matrix to see accuracy and other parameter values
cm_55_knn<- confusionMatrix(fit.knn, test_cl$Churn)
message ("-----------------------")
message (" Testing Performance")
message ("-----------------------")

print(cm_55_knn)

cat("\n\n","KNN USING ALL VARIABLES WITHOUT OPTIMIZATION","\n\n",
    "*****************************************************************************************************************************","\n","train_percent", "\t", "test_percent", "\t ",
    "Accuracy", "\t ", "Kappa", "\t\t ", "Sensitivity", "\t ", "Specificity", "\t ", "Precision", "\t ", "Recall","\n",
    "*****************************************************************************************************************************","\n",
    file = output_file_path, sep = " ", append = TRUE)


# write evaluation metrics to txt file
cat( t*100, "\t\t ", (1-t)*100, "\t\t ",
     format(round(cm_55_knn[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_55_knn[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_55_knn[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_55_knn[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_55_knn[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_55_knn[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n" ,"\n",
     file = output_file_path, sep = " ", append = TRUE)




#Feature Selection

#Correlation matrix for with all features
correlation_matrix<- cor(x[,1:55]) #to obtain a correlation matrix for all the input attributes without the target field
correlation_matrix
# ensure the results are repeatable
set.seed(7)
# find attributes that are highly corrected
highlyCorrelated <- findCorrelation(correlation_matrix, cutoff=0.75)
# print names of highly correlated attributes
names(Data[,highlyCorrelated])


#Feature importance using Random forest feature selection
set.seed(2018)
# Compute variable importance with Random Forest Algorithm

variable_imp_func <- function(data_name="Data"){
  quick_RF <- randomForest(x=data_name[,-ncol(data_name)],
                           y=data_name$Churn,
                           ntree=100,importance=TRUE)
  imp_RF <- importance(quick_RF)
  imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
  imp_DF <<- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]
}

# call variable importance function on entire data
variable_imp_func(Data)
top_n_features <- imp_DF$Variables[1:nrow(imp_DF)]

# Plot variable importance
ggplot(imp_DF[1:length(top_n_features),], aes(x=reorder(Variables, MSE),
                                              y=MSE, fill=MSE)) +
  geom_bar(stat = 'identity', fill = "Blue") +
  labs(x = 'Variables',
       y= '% increase in MSE if variable is randomly permuted',
       title = "Feature importance using RF") +
  theme(plot.title =
          element_text(size = 10, face = "bold", hjust = 0.5)) +
  coord_flip() + theme(legend.position="none")



#Running models with different combinations of features and different train-test split (80-20, 70-30, 60-40, 50-50,
# 40-60, 30-70, 20-80)

# Naive Bayes
# initialize performance frame counter
pfc_nb <- 0
features_used <- c(25,35,47,55)
#loop through different number of features
for (f in features_used)
{
  message ("*********************************************************************")
  message ("NAIVE BAYES USING ", f ," FEATURES " )
  message ("*********************************************************************")
  
  #write into txt file
  
  cat("\n\n","NAIVE BAYES USING", f, "FEATURES","\n\n",
      "*****************************************************************************************************************************","\n","train_percent", "\t", "test_percent", "\t ",
      "Accuracy", "\t ", "Kappa", "\t\t ", "Sensitivity", "\t ", "Specificity", "\t ", "Precision", "\t ", "Recall","\n",
      "*****************************************************************************************************************************","\n",
      file = output_file_path, sep = " ", append = TRUE)
  
  #create naive bayes performance frame
  pf_nb <- data.frame(matrix(
    vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50",
                                    "TR-60", "TR-70", "TR-80", "TR-90"),
                                  c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),
    stringsAsFactors=F)
  
  # Train percentage sequence
  training_data_percentages <- seq(0.8 , 0.2,  -0.1 )
  
  for (t in training_data_percentages){
    message ("*********************************************************************")
    message ("Training Data: ", t*100, " | Testing Data : ", (1-t)*100 )
    message ("*********************************************************************")
    
    #increment performance counter
    pfc_nb <- pfc_nb+1
    
    control <- trainControl (method="repeatedcv", repeats = 10)
    metric <- "Accuracy"
    
    # split data into training_data and testing_data
    validation <- createDataPartition(Data$Churn, p=t, list=FALSE)
    
    training_data <- Data[validation,]
    testing_data <- Data[-validation,]
    
    # Run variable importance on training data only
    variable_imp_func(training_data)
    
    
    # select top n features
    top_n_features <- imp_DF$Variables[1:f]
    selected_features <- Data[validation,top_n_features]
    
    # Add target column to selected features
    selected_features$Churn <-  Data[validation,"Churn"]
    training_data_w_features <- selected_features
    
    
    # Convert target column to factor
    training_data_w_features[,ncol(training_data_w_features) ] <- as.factor (training_data_w_features[,ncol(training_data_w_features) ])
    set.seed (15)
    
    # Train naive bayes model
    
    set.seed(120)
    TrainedClassifier_nb <- naiveBayes(Churn ~ ., data = training_data_w_features, laplace=0)
    Predicted_outcomes_nb <- predict(TrainedClassifier_nb, newdata = testing_data[,1:ncol(testing_data)-1])
    cm_nb <- confusionMatrix(testing_data$Churn, Predicted_outcomes_nb)
    
    message ("-----------------------")
    message (" Training Performance")
    message ("-----------------------")
    
    print(TrainedClassifier_nb)
    
    message ("-----------------------")
    message (" Testing Performance")
    message ("-----------------------")
    print (cm_nb)
    
    # write evaluation metrics to txt file
    cat( t*100, "\t\t ", (1-t)*100, "\t\t ",
         format(round(cm_nb[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
         format(round(cm_nb[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
         format(round(cm_nb[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
         format(round(cm_nb[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
         format(round(cm_nb[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
         format(round(cm_nb[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n" ,"\n",
         file = output_file_path, sep = " ", append = TRUE)
    
    #pf[pfc_nb,"Accuracy"] <- format(round(cm_nb[["overall"]][["Accuracy"]]*100, 2), nsmall = 2)
    #pf[pfc_nb,"Kappa"] <- format(round(cm_nb[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
    #pf[pfc_nb,"Sensitivity"] <- format(round(cm_nb[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2)
    #pf[pfc_nb,"Specificity"] <- format(round(cm_nb[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
    #pf[pfc_nb,"Precision"] <- format(round(cm_nb[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
    #pf[pfc_nb,"Recall"] <- format(round(cm_nb[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
    
    
    
    
  }
  
  #Write performance frame into file
  #cat("Performance Frame with ",f," Variables ", "\n\n\n",
  #      file = output_file_path, sep = " ", append = TRUE)
  
  
  #write.table(pf, file = output_file_path, append = TRUE, sep = " ", dec = ".", row.names = TRUE, col.names = TRUE)
  
  
}


#C5
# initialize performance frame counter
pfc_c5 <- 0
features_used <- c(25,35,47,55)


#loop through different number of features
for (f in features_used)
{
  message ("*********************************************************************")
  message ("C5 USING ", f ," FEATURES " )
  message ("*********************************************************************")
  
  #write into txt file
  
  cat("\n\n","C5 USING", f, "FEATURES","\n\n",
      "*****************************************************************************************************************************","\n","train_percent", "\t", "test_percent", "\t ",
      "Accuracy", "\t ", "Kappa", "\t\t ", "Sensitivity", "\t ", "Specificity", "\t ", "Precision", "\t ", "Recall","\n",
      "*****************************************************************************************************************************","\n",
      file = output_file_path, sep = " ", append = TRUE)
  
  #create c5 performance frame
  pf_c5 <- data.frame(matrix(
    vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50",
                                    "TR-60", "TR-70", "TR-80", "TR-90"),
                                  c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),
    stringsAsFactors=F)
  
  # Train percentage sequence
  training_data_percentages <- seq(0.8 , 0.2,  -0.1 )
  
  for (t in training_data_percentages){
    message ("*********************************************************************")
    message ("Training Data: ", t*100, " | Testing Data : ", (1-t)*100 )
    message ("*********************************************************************")
    
    #increment performance counter
    pfc_c5 <- pfc_c5+1
    
    control <- trainControl (method="repeatedcv", repeats = 10)
    metric <- "Accuracy"
    
    # split data into training_data and testing_data
    validation <- createDataPartition(Data$Churn, p=t, list=FALSE)
    
    training_data <- Data[validation,]
    testing_data <- Data[-validation,]
    
    # Run variable importance on training data only
    variable_imp_func(training_data)
    
    
    # select top n features
    top_n_features <- imp_DF$Variables[1:f]
    selected_features <- Data[validation,top_n_features]
    
    # Add target column to selected features
    selected_features$Churn <-  Data[validation,"Churn"]
    training_data_w_features <- selected_features
    
    # Convert target column to factor
    training_data_w_features[,ncol(training_data_w_features) ] <- as.factor (training_data_w_features[,ncol(training_data_w_features) ])
    set.seed (15)
    
    tree_model <- C5.0(Churn~., data = training_data)
    message ("-----------------------")
    message (" Training Performance")
    message ("-----------------------")
    
    print(tree_model)
    summary(tree_model)
    plot(tree_model)
    
    Predictions <- predict(tree_model, newdata = testing_data)
    cm_c5 <- confusionMatrix(testing_data$Churn, Predictions)
    
    message ("-----------------------")
    message (" Testing Performance")
    message ("-----------------------")
    
    print(cm_c5)
    
    
    
    # write evaluation metrics to text file
    cat( t*100, "\t\t ", (1-t)*100, "\t\t ",
         format(round(cm_c5[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
         format(round(cm_c5[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
         format(round(cm_c5[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
         format(round(cm_c5[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
         format(round(cm_c5[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
         format(round(cm_c5[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n" ,"\n",
         file = output_file_path, sep = " ", append = TRUE)
    
    #pf[pfc_c5,"Accuracy"] <- format(round(cm_c5[["overall"]][["Accuracy"]]*100, 2), nsmall = 2)
    #pf[pfc_c5,"Kappa"] <- format(round(cm_c5[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
    #pf[pfc_c5,"Sensitivity"] <- format(round(cm_c5[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2)
    #pf[pfc_c5,"Specificity"] <- format(round(cm_c5[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
    #pf[pfc_c5,"Precision"] <- format(round(cm_c5[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
    #pf[pfc_c5,"Recall"] <- format(round(cm_c5[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
    
    
    
    
  }
  
  #Write performance frame into file
  #cat("Performance Frame with ",f," Variables ", "\n\n\n",
  #      file = output_file_path, sep = " ", append = TRUE)
  
  
  #write.table(pf, file = output_file_path, append = TRUE, sep = " ", dec = ".", row.names = TRUE, col.names = TRUE)
  
  
}


#Random Forest
# initialize performance frame counter
pfc_rf <- 0
features_used <- c(25,35,47,55)
#loop through different number of features
for (f in features_used)
{
  message ("*********************************************************************")
  message ("RANDOM FOREST USING ", f ," FEATURES " )
  message ("*********************************************************************")
  
  #write into txt file
  
  cat("\n\n","RANDOM FOREST USING", f, "FEATURES","\n\n",
      "*****************************************************************************************************************************","\n","train_percent", "\t", "test_percent", "\t ",
      "Accuracy", "\t ", "Kappa", "\t\t ", "Sensitivity", "\t ", "Specificity", "\t ", "Precision", "\t ", "Recall","\n",
      "*****************************************************************************************************************************","\n",
      file = output_file_path, sep = " ", append = TRUE)
  
  #create random forest performance frame
  pf_rf <- data.frame(matrix(
    vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50",
                                    "TR-60", "TR-70", "TR-80", "TR-90"),
                                  c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),
    stringsAsFactors=F)
  
  # Train percentage sequence
  training_data_percentages <- seq(0.8 , 0.2,  -0.1 )
  
  for (t in training_data_percentages){
    message ("*********************************************************************")
    message ("Training Data: ", t*100, " | Testing Data : ", (1-t)*100 )
    message ("*********************************************************************")
    
    #increment performance counter
    pfc_rf <- pfc_rf+1
    
    control <- trainControl (method="repeatedcv", repeats = 10)
    metric <- "Accuracy"
    
    # split data into training_data and testing_data
    validation <- createDataPartition(Data$Churn, p=t, list=FALSE)
    
    training_data <- Data[validation,]
    testing_data <- Data[-validation,]
    
    # Run variable importance on training data only
    variable_imp_func(training_data)
    
    # select top n features
    top_n_features <- imp_DF$Variables[1:f]
    selected_features <- Data[validation,top_n_features]
    
    # Add target column to selected features
    selected_features$Churn <-  Data[validation,"Churn"]
    training_data_w_features <- selected_features
    
    # Convert target column to factor
    #training_data_w_features[,ncol(training_data_w_features) ] <- as.factor (training_data_w_features[,ncol(training_data_w_features) ])
    set.seed (123)
    
    # Train Random Forest model model
    rf <-randomForest(Churn ~., data = training_data_w_features,ntree = 500, mtry=5)
    message ("-----------------------")
    message (" Training Performance")
    message ("-----------------------")
    print(rf)
    
    Predicted_outcomes_rf <- predict(rf, newdata = testing_data)
    
    cm_rf <- confusionMatrix(testing_data$Churn, Predicted_outcomes_rf)
    
    message ("-----------------------")
    message (" Testing Performance")
    message ("-----------------------")
    print (cm_rf)
    
    # write evaluation metrics to text file
    cat( t*100, "\t\t ", (1-t)*100, "\t\t ",
         format(round(cm_rf[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
         format(round(cm_rf[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
         format(round(cm_rf[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
         format(round(cm_rf[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
         format(round(cm_rf[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
         format(round(cm_rf[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n" ,"\n",
         file = output_file_path, sep = " ", append = TRUE)
    
    
    
    #pf[pfc_rf,"Accuracy"] <- format(round(cm_rf[["overall"]][["Accuracy"]]*100, 2), nsmall = 2)
    #pf[pfc_rf,"Kappa"] <- format(round(cm_rf[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
    #pf[pfc_rf,"Sensitivity"] <- format(round(cm_rf[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2)
    #pf[pfc_rf,"Specificity"] <- format(round(cm_rf[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
    #pf[pfc_rf,"Precision"] <- format(round(cm_rf[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
    #pf[pfc_rf,"Recall"] <- format(round(cm_rf[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
    
    
    
    
  }
  
  #Write performance frame into file
  #cat("Performance Frame with ",f," Variables ", "\n\n\n",
  #      file = output_file_path, sep = " ", append = TRUE)
  
  
  #write.table(pf, file = output_file_path, append = TRUE, sep = " ", dec = ".", row.names = TRUE, col.names = TRUE)
}


# OPTIMIZATION OF C5 BY BOOSTING

set.seed(7)

t<- 0.70
control <- trainControl (method="repeatedcv", repeats = 10)
metric <- "Accuracy"

# split data into training_data and testing_data
validation <- createDataPartition(Data$Churn, p=t, list=FALSE)

training_data <- Data[validation,]
testing_data <- Data[-validation,]


# Convert target column to factor
training_data[,ncol(training_data) ] <- as.factor (training_data[,ncol(training_data) ])
set.seed (15)


tree_model <- C5.0(Churn~., data = training_data, trials =  5)
message ("-----------------------")
message (" Training Performance")
message ("-----------------------")

print(tree_model)

Predictions <- predict(tree_model, newdata = testing_data)
cm_c5 <- confusionMatrix(testing_data$Churn, Predictions)


message ("-----------------------")
message (" Testing Performance")
message ("-----------------------")

print(cm_c5)


cat("\n\n","C5 AFTER BOOSTING","\n\n",
    "*****************************************************************************************************************************","\n","train_percent", "\t", "test_percent", "\t ",
    "Accuracy", "\t ", "Kappa", "\t\t ", "Sensitivity", "\t ", "Specificity", "\t ", "Precision", "\t ", "Recall","\n",
    "*****************************************************************************************************************************","\n",
    file = output_file_path, sep = " ", append = TRUE)


# write evaluation metrics to txt file
cat( t*100, "\t\t ", (1-t)*100, "\t\t ",
     format(round(cm_c5[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_c5[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_c5[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_c5[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_c5[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_c5[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n" ,"\n",
     file = output_file_path, sep = " ", append = TRUE)



set.seed(123)
# Split data into train and test sets
validation <- createDataPartition(Data$Churn, p=0.70, list=FALSE)
t <- 0.70

testing_split <- Data[-validation,]
training_split <- Data[validation,]

# Principal Component Analysis for train set
principal_components <- prcomp(training_split[,1:ncol(Data)-1], center = TRUE, scale. =TRUE)

training_data <- cbind(Churn = training_split[, "Churn"], principal_components$x) %>%
  as.data.frame()

# Pick 30 Principal Components plus Target
training_data <- training_data[,1:31]

# Convert target to factor
training_data[,"Churn"] <- as.factor(training_data[,"Churn"])

#Run model on train
trained_classifier <- C5.0(Churn~., data=training_data )

message ("-----------------------")
message (" Training Performance")
message ("-----------------------")

print(trained_classifier)
summary(trained_classifier)
plot(trained_classifier)



#transform test set into PCA
test_p <- predict(principal_components, newdata= testing_split[,1:ncol(Data)-1])

test_p <- as.data.frame(test_p)

# pick 30 PCs
testing_data <- test_p[,1:30]

# convert test target to factor
testing_split[,"Churn"] <- as.factor(testing_split[,"Churn"])
levels(testing_split[,"Churn"]) <- c("1","2")


testing_data <- cbind(Churn = testing_split[, "Churn"], test_p[, 1:30]) %>% as.data.frame()


#make preciction on testing data
prediction <- predict(trained_classifier, testing_data)

cm_c5 <- confusionMatrix( prediction,testing_data$Churn)
message ("-----------------------")
message (" Testing Performance")
message ("-----------------------")

print(cm_c5)

cat("\n\n","C5 OPTIMIZED WITH  PRINCIPAL COMPONENT ANALYSIS","\n\n",
    "*****************************************************************************************************************************","\n","train_percent", "\t", "test_percent", "\t ",
    "Accuracy", "\t ", "Kappa", "\t\t ", "Sensitivity", "\t ", "Specificity", "\t ", "Precision", "\t ", "Recall","\n",
    "*****************************************************************************************************************************","\n",
    file = output_file_path, sep = " ", append = TRUE)


# write evaluation metrics to txt file
cat( t*100, "\t\t ", (1-t)*100, "\t\t ",
     format(round(cm_c5[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_c5[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_c5[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_c5[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_c5[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_c5[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n" ,"\n",
     file = output_file_path, sep = " ", append = TRUE)


# Normalization
Data_new <- Data

Data_new <- Data_new[,1:ncol(training_data)-1]

norm_prep <- preProcess(as.data.frame(Data_new), method=c("range"))

data_norm <- predict(norm_prep, as.data.frame(Data_new))


data_norm <-cbind(data_norm,Churn = Data$Churn)

#Compare MonthlyRevenue Vs Normalized MonthlyRevenue
par(mfrow=c(1,2))
boxplot(data_norm[,1], main=names(data_norm)[1])
boxplot(Data[,1], main=names(Data)[1])

# C5 model using normalized data
set.seed(7)

validation <- createDataPartition(data_norm$Churn, p=0.70, list=FALSE)
t <- 0.70
training_data <- data_norm[validation,]
testing_data <- data_norm[-validation,]



# Train C5 model with normalized data
model_c5_norm <- C5.0(Churn~., data = training_data)
message ("-----------------------")
message (" Training Performance of  C5 model using normalized data")
message ("-----------------------")
print(model_c5_norm)
prediction_c5norm <- predict(model_c5_norm, newdata = testing_data) # Validate model using the test data

cm_c5_norm <-confusionMatrix(testing_data$Churn,prediction_c5norm)

message ("-----------------------")
message (" Testing Performance of  C5 model using normalized data")
message ("-----------------------")
print(cm_c5_norm)

cat("\n\n","C5 RESULT USING NORMALIZED DATA","\n\n",
    "*****************************************************************************************************************************","\n","train_percent", "\t", "test_percent", "\t ",
    "Accuracy", "\t ", "Kappa", "\t\t ", "Sensitivity", "\t ", "Specificity", "\t ", "Precision", "\t ", "Recall","\n",
    "*****************************************************************************************************************************","\n",
    file = output_file_path, sep = " ", append = TRUE)


# write evaluation metrics to txt file
cat( t*100, "\t\t ", (1-t)*100, "\t\t ",
     format(round(cm_c5_norm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_c5_norm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_c5_norm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_c5_norm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_c5_norm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_c5_norm[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n" ,"\n",
     file = output_file_path, sep = " ", append = TRUE)


# Random Forest Algorithm with Normalized Data and Performance Tuning
set.seed(7)

validation <- createDataPartition(data_norm$Churn, p=0.70, list=FALSE)
t<- 0.70
training_data <- data_norm[validation,]
testing_data <- data_norm[-validation,]


# Train RF model to be tuned
model_RF55 <- randomForest(Churn~., data = training_data, ntree = 300, mtry = 5)
message ("-----------------------")
message (" Training Performance of  RF model using normalized data")
message ("-----------------------")
print(model_RF55)
prediction_RF55 <- predict(model_RF55, newdata = testing_data) # Validate model using the test data

cm_rf_norm <-confusionMatrix(testing_data$Churn,prediction_RF55)

message ("-----------------------")
message (" Testing Performance of  RF model using normalized data")
message ("-----------------------")
print(cm_rf_norm)

cat("\n\n","RANDOM FOREST RESULT USING NORMALIZED DATA","\n\n",
    "*****************************************************************************************************************************","\n","train_percent", "\t", "test_percent", "\t ",
    "Accuracy", "\t ", "Kappa", "\t\t ", "Sensitivity", "\t ", "Specificity", "\t ", "Precision", "\t ", "Recall","\n",
    "*****************************************************************************************************************************","\n",
    file = output_file_path, sep = " ", append = TRUE)


# write evaluation metrics to txt file
cat( t*100, "\t\t ", (1-t)*100, "\t\t ",
     format(round(cm_rf_norm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_rf_norm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_rf_norm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_rf_norm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_rf_norm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_rf_norm[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n" ,"\n",
     file = output_file_path, sep = " ", append = TRUE)


#plot initial RF model to determine tuning parameters for  a better algorithm performance
plot(model_RF55)

tune_plot <- tuneRF(training_data[,-ncol(training_data)], training_data[,ncol(training_data)],
                    stepFactor = 0.5,
                    plot = TRUE,
                    ntreeTry = 100,
                    trace = TRUE,
                    improve = 0.05)

model_Tuned55 <- randomForest(Churn ~ ., data = training_data, ntree = 200, mtry = 3, trace = TRUE)

message ("-----------------------")
message (" Training Performance of tuned RF model")
message ("-----------------------")

print(model_Tuned55)

prediction_Tuned55 <- predict(model_Tuned55, newdata = testing_data)

CM_Tuned55 <-confusionMatrix(testing_data$Churn,prediction_Tuned55)

message ("-----------------------")
message (" Testing Performance of tuned RF model")
message ("-----------------------")
CM_Tuned55

cat("\n\n","RANDOM FOREST RESULT AFTER TUNING AND USING NORMALIZED DATA","\n\n",
    "*****************************************************************************************************************************","\n","train_percent", "\t", "test_percent", "\t ",
    "Accuracy", "\t ", "Kappa", "\t\t ", "Sensitivity", "\t ", "Specificity", "\t ", "Precision", "\t ", "Recall","\n",
    "*****************************************************************************************************************************","\n",
    file = output_file_path, sep = " ", append = TRUE)


# write evaluation metrics to txt file
cat( t*100, "\t\t ", (1-t)*100, "\t\t ",
     format(round(CM_Tuned55[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(CM_Tuned55[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(CM_Tuned55[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(CM_Tuned55[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(CM_Tuned55[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(CM_Tuned55[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n" ,"\n",
     file = output_file_path, sep = " ", append = TRUE)




# Random Forest Algorithm with UnNormalized Data and Performance Tuning

set.seed(7)
validation <- createDataPartition(Data$Churn, p=0.70, list=FALSE)
t <- 0.70
training_data <- Data[validation,]
testing_data <- Data[-validation,]

# Train RF model to be tuned
model_RF55 <- randomForest(Churn ~ ., data = training_data, ntree = 300, mtry = 5)
prediction_RF55 <- predict(model_RF55, newdata = testing_data) # Validate model using the test data

CM_RF55 <-confusionMatrix(testing_data$Churn,prediction_RF55)

#plot initial RF model to determine tuning parameters for  a better algorithm performance

plot(model_RF55)

tune_plot <- tuneRF(training_data[,-ncol(training_data)], training_data[,ncol(training_data)],
                    stepFactor = 0.5,
                    plot = TRUE,
                    ntreeTry = 100,
                    trace = TRUE,
                    improve = 0.05)

model_Tuned55 <- randomForest(Churn ~ ., data = training_data, ntree = 200, mtry = 3, trace = TRUE)
message ("-----------------------")
message (" Training Performance of Tuned RF with unnormalized data")
message ("-----------------------")

print(model_Tuned55)



prediction_Tuned55 <- predict(model_Tuned55, newdata = testing_data)

cm_rf_tuned_unorm <-confusionMatrix(testing_data$Churn,prediction_Tuned55)

message ("-----------------------")
message (" Testing Performance")
message ("-----------------------")
print(cm_rf_tuned_unorm)

cat("\n\n","RANDOM FOREST RESULT AFTER TUNING ","\n\n",
    "*****************************************************************************************************************************","\n","train_percent", "\t", "test_percent", "\t ",
    "Accuracy", "\t ", "Kappa", "\t\t ", "Sensitivity", "\t ", "Specificity", "\t ", "Precision", "\t ", "Recall","\n",
    "*****************************************************************************************************************************","\n",
    file = output_file_path, sep = " ", append = TRUE)


# write evaluation metrics to txt file
cat( t*100, "\t\t ", (1-t)*100, "\t\t ",
     format(round(cm_rf_tuned_unorm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_rf_tuned_unorm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_rf_tuned_unorm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_rf_tuned_unorm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_rf_tuned_unorm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
     format(round(cm_rf_tuned_unorm[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n" ,"\n",
     file = output_file_path, sep = " ", append = TRUE)



message("PROAGRAM RAN SUCCESSFULLY")
