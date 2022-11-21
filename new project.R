#install.packages("coinmarketcapr")
#install.packages("devtools")

#library(coinmarketcapr)
#install.packages('devtools')
#library(tidyverse)
#library(devtools)
#install.packages('forecast')
#library(forecast)
#install.packages("timetk")
#library(ggplot2)
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
#library(writexl)
#install.packages("rvest")
#library(rlang)
#install.packages("ltm")
library(ltm)

getwd()

setwd("C:/Users/Daniel/OneDrive - University of Surrey/Desktop")

package_list <- c("class","dplyr","tidyr","CaTool","caret","e1071","Hmisc","readxl",
                  "lattice","ggplot2","ggcorrplot","mlbench","randomForest",
                  "varImp","C50","caTools","edfReader","eegkit","rlist","ipred","forecast","devtools","coinmarketcapr", "writexl", "rvest", "randomForest")
lapply(package_list, require, character.only = TRUE)

output_file_path <-  "C:/Users/Daniel/OneDrive - University of Surrey/Desktop/crypto/model_output.txt"

devtools::install_github("amrrs/coinmarketcapr") #installing github coin-marketcap.

#Api_key <- "6c90fe1d-e25e-4494-8e27-4b565349ae15" #registering the API KEY

#devtools::install_github("tidyverse/tidyverse")

#coinmarketcapr::setup(Api_key) #Setting up API KEY

#crypto<-get_crypto_listings(limit= 5000) # getting the data from coin-market cap

library(writexl)

#write_xlsx(crypto,"C:/Users/Daniel/OneDrive - University of Surrey/Desktop/PROJECT.xlsx")



crypto <- read_xlsx("C:/Users/Daniel/OneDrive - University of Surrey/Desktop/PROJECt.xlsx")

crypto <- as.data.frame(crypto) #converting crypto to data frame



#Reorganizing the columns with target field as the last column

names(crypto) # viewing the headers

library(dplyr)

glimpse(crypto)

count(crypto)



summary(crypto) #descriptive analysis of the raw data

#DATA PRE-PROCESSING

#Checking missing variables in the dataset.

is.na(crypto)

sum(is.na(crypto)) # checking missing value for each variable.


#Re-coding missing value with the mean.

crypto$max_supply[is.na(crypto$max_supply)] <- mean(crypto$max_supply, na.rm = TRUE)
crypto$self_reported_circulating_supply[is.na(crypto$self_reported_circulating_supply)] <- mean(crypto$self_reported_circulating_supply, na.rm = TRUE)
crypto$self_reported_market_cap[is.na(crypto$self_reported_market_cap)] <- mean(crypto$self_reported_market_cap, na.rm = TRUE)
crypto$tvl_ratio[is.na(crypto$tvl_ratio)] <- mean(crypto$tvl_ratio, na.rm = TRUE)
crypto$platform_id[is.na(crypto$platform_id)] <- mean(crypto$platform_id, na.rm = TRUE)
crypto$USD_tvl[is.na(crypto$USD_tvl)] <- mean(crypto$USD_tvl, na.rm = TRUE)

#Deleting column not needed

crypto$id <- NULL #delete the vector- "id" as it has 19201 "na"
crypto$slug <- NULL #delete the vector- "slug" as it does not have impact on the analysis
crypto$tags<- NULL
crypto$last_updated<-NULL
crypto$platform_id<-NULL
crypto$platform_name<-NULL
crypto$platform_symbol<-NULL
crypto$platform_slug<-NULL
crypto$platform_token_address<-NULL
crypto$USD_last_updated<-NULL
crypto$date_added<-NULL
crypto$symbol<-NULL

describe(crypto)

View(crypto) #view the data frame after deleting vectors


#Converting vectors with characters to ASCII format

for (i in 1:(ncol(crypto))) {
  if (is.character(crypto[, i])==TRUE){
    for(j in 1:nrow(crypto)) {
      ascis <- as.numeric(charToRaw(crypto[j, i]))
      crypto[ j, i] <- sum(ascis)
    }
  }
  crypto[,i] <- as.numeric(crypto[,i])
}

#calculate Cronbach's Alpha

cronbach.alpha(crypto)

View(crypto)

# custom function to implement min max scaling
minMax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}


#normalise data using custom function
normalisedMydata <- as.data.frame(lapply(crypto, minMax))

#re-arrange target column to end

normalisedMydata <- normalisedMydata[ ,c(1,2,3,4,5,6,7,8,9,11,
           12,13,14,15,16,17,18,19,20,21,22,10)]

names(normalisedMydata)

describe(normalisedMydata)

summary(normalisedMydata)

sapply(normalisedMydata[,1:22], sd) #standard deviation of columns

#Skewness of the data
library(moments)

skewness_crypto  <- rnorm(normalisedMydata) 
skewness(skewness_crypto)

hist(skewness_crypto)

hist(kurtosis(skewness_crypto))

hist(skewness_crypto, prob = TRUE)                   # Draw histogram with density
lines(density(skewness_crypto), col = 2, lwd = 3)

normalisedMydata_visualization<-normalisedMydata

#create variable x- attach to it the input attributes of the dataset
x  <- normalisedMydata[,1:ncol(normalisedMydata)-1]
#create variable y- attach to it the output attribute of the dataset
y <- normalisedMydata[,ncol(normalisedMydata)]

head(normalisedMydata)
summary(normalisedMydata)

View(normalisedMydata)

# Test for normality using shapiro

shapiro.test(normalisedMydata$USD_price)

hist(normalisedMydata$USD_price, 
     main="USD_price", 
     xlab="USD_price", 
     border="light blue", 
     col="blue", 
     las=1, 
     breaks=5)

#process <- preProcess(as.data.frame(crypto), method=c("range"))

#crypto_scale <- predict(process, as.data.frame(crypto))

#summary(crypto_scale)
#View(crypto_scale)

#library(writexl)

#write_xlsx(crypto,"C:/Users/Daniel/OneDrive - University of Surrey/Desktop/PROJECT.xlsx")

# Handling the large decimal places in the different columns

#crypto2 <- crypto %>% 
  #mutate_if(is.numeric, round, digits = 2)


#Visualization

# correlation plot to view the different realtions between the diffferent variables

library(corrplot)

#install.packages("GGally")

library(GGally)


k = cor(normalisedMydata_visualization)

corrplot(k, method = "number")

corrplot(k, tl.col = "red", bg = "White", tl.srt = 35, 
         title = "\n\n Correlation Plot Of crypto data \n",
         addCoef.col = "black", type = "full")

corrplot(k, method = "number")



cor(k)

library(corrplot)


# Install and load reshape2 package
install.packages("reshape2")
library(reshape2)

# creating correlation matrix
corr_mat <- round(cor(normalisedMydata_visualization),2)

# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat)
head(melted_corr_mat)

# plotting the correlation heatmap
library(ggplot2)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value),
            color = "black", size = 4)


normalisedMydata1 <- table(normalisedMydata_visualization$USD_price )

barplot(normalisedMydata1,
        col="dodgerblue3",
        width=c(1000000))

ggplot(normalisedMydata_visualization, aes(x=USD_percent_change_1h, y=USD_price, width=USD_percent_change_1h/10)) + 
  geom_bar(aes(fill=USD_percent_change_1h), stat="identity", position="identity")

ggplot(normalisedMydata_visualization, aes(x=name, y=USD_price, width=name/10)) + 
  geom_bar(aes(fill=name), stat="identity", position="identity")

#scatterplot for USD_price vs USD_percent_change_1h with target field (USD price)

ggplot(normalisedMydata_visualization, aes(x = USD_percent_change_1h, y = USD_price, color = USD_price, width = 10)) +
  geom_point() + ggtitle("USD_price vs USD_percent_change_1h")


ggplot(normalisedMydata_visualization, aes(x = name, y = USD_price, color = name)) +
  geom_point() + ggtitle("USD_price vs name")

ggplot(normalisedMydata_visualization, aes(x = total_supply, y = USD_price, color = total_supply)) +
  geom_point() + ggtitle("USD_price vs total_supply")

ggplot() +
  geom_point(aes(x = normalisedMydata_visualization$USD_market_cap, y = normalisedMydata_visualization$USD_price),
             colour = 'red')

#Boxplot
par(mfrow=c(1, 8))
for(i in 1:9) {
  boxplot(x[,i], main=names(normalisedMydata_visualization) [i])
}

#histogram showing distribution of USD price of the various cryprocurrency.

hist(normalisedMydata_visualization$USD_price)

#scatter plot of the whole variables.

scatterplot3d::scatterplot3d(normalisedMydata_visualization)


ggplot(data = normalisedMydata_visualization,
       mapping = aes(x = name,
                     y = USD_price,
                     color = max_supply)) +
  geom_point(alpha = .7,
             size = 3) +
  geom_smooth(method = "lm",
              se = FALSE,
              linewidth = 0.5, formula= y ~ x, color= "black") + ggtitle("Coin_name vs  USD_ price")



boxplot(normalisedMydata_visualization$USD_price, ylab = "USD_price ($)")

boxplot(normalisedMydata_visualization)

hist(normalisedMydata_visualization)

data(normalisedMydata_visualization, package="mosaicData")

# select numeric variables
df <- dplyr::select_if(normalisedMydata_visualization, is.numeric)

# calulate the correlations
r <- cor(df, use="complete.obs")

round(r,2)

ggcorrplot(r)

{plot(seq(1,10,1))
  abline(a=0,b=1)}



qqnorm(normalisedMydata_visualization$USD_price, pch = 1, frame = FALSE)


#MODELING

#Splitting the data into test and train
set.seed(150)
#install.packages('caTools')
library(caTools)

split <- sample.split(normalisedMydata, SplitRatio = 0.7)
t <- 0.70
train_cl <- subset(normalisedMydata, split == "TRUE")
test_cl <- subset(normalisedMydata, split == "FALSE") 


#Random Forest using all variables
set.seed(123)


model_RF22 <- randomForest(USD_price ~ ., data = train_cl, ntree = 500, mtry = 5)

# Look at variable importance:
importance(model_RF22)

plot(model_RF22)

varImpPlot(model_RF22)


prediction_RF22 <- predict(model_RF22, newdata = test_cl) # The next step is to validate our model using the test data

result_train <- data.frame(test_cl,
                          prediction_RF22)

result_train

data.frame(RMSE = RMSE(prediction_RF22, test_cl$USD_price),
           R2 = R2(prediction_RF22, test_cl$USD_price))

hist(prediction_RF22)

#RMSE        R2
#1 0.02335415 0.1052408

#Linear Regression using all variables

model_LR22 <- lm(USD_price ~ ., data = train_cl)

plot(model_LR22)


hist(model_LR22$residuals)

summary(model_LR22)

summary(model_LR22)$coefficient

shapiro.test(x = model_LR22$residuals[3:5000]) #Shapiromilk test


#Shapiro-Wilk normality test

#data:  model_LR22$residuals[3:5000]
#W = 0.036706, p-value < 2.2e-16

library(car)

vif(model_LR22)

prediction_LR22 <- predict(model_LR22, newdata = test_cl) # The next step is to validate our model using the test data


result_train_LR22 <- data.frame(test_cl,
                           prediction_LR22)

# plot predicted values and actual values

plot(predict(model_LR22), train_cl$USD_price,
     xlab = "Predicted Values",
     ylab = "Observed Values")
abline(a = 0, b = 1, lwd=2,
       col = "green")

#install.packages("performance")

library(performance)

performance_accuracy(model_LR22) #Validating the accuracy of the model

data.frame(RMSE = RMSE(prediction_LR22, test_cl$USD_price),
           R2 = R2(prediction_LR22, test_cl$USD_price))

hist(prediction_LR22)

#RMSE           R2
#1 0.0279878 2.689596e-06

#Feature Selection

#Correlation matrix for with all features
correlation_matrix<- cor(x[,1:21]) #to obtain a correlation matrix for all the input attributes without the target field
correlation_matrix
# ensure the results are repeatable
set.seed(7)
#install.packages("caret")
library(caret)
# find attributes that are highly corrected
highlyCorrelated <- findCorrelation(correlation_matrix, cutoff=0.50)
# print names of highly correlated attributes
names(normalisedMydata[,highlyCorrelated])



#Feature importance using Random forest feature selection
set.seed(2018)

# Compute variable importance with Random Forest Algorithm

variable_imp_func <- function(data_name="x"){
  quick_RF <- randomForest(x=data_name[,-ncol(data_name)],
                           y=data_name$USD_price,
                           ntree=100,importance=TRUE)
  imp_RF <- importance(quick_RF)
  imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
  imp_DF <<- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]
}

# call variable importance function on entire data
variable_imp_func(x)
top_n_features <- imp_DF$Variables[1:nrow(imp_DF)] #Rank the variable based on importance
top_n_features # view the variables
top_imp_variables <-top_n_features[1:16] #grouping the best 15 variables into a dataframe


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

# Modelling using the top variables with importance

View(top_imp_variables)

# selecting the top important variables from the dataset to use for the model.



top_imp_variable_16 <- select(normalisedMydata,total_supply,USD_market_cap,circulating_supply,self_reported_market_cap,USD_market_cap_dominance,tvl_ratio,name,cmc_rank,USD_percent_change_24h,USD_volume_24h,USD_fully_diluted_market_cap,USD_percent_change_7d,num_market_pairs,USD_percent_change_60d,self_reported_circulating_supply,USD_price)

cronbach.alpha(top_imp_variable_16)


split2 <- sample.split(top_imp_variable_16, SplitRatio = 0.7)
t <- 0.70
train_cl_top <- subset(top_imp_variable_16, split2 == "TRUE")
test_cl_top <- subset(top_imp_variable_16, split2 == "FALSE") 


#Random Forest using all variables
set.seed(123)
model_RF16 <- randomForest(USD_price ~ ., data = train_cl_top, ntree = 500, mtry = 5)

# Look at variable importance:
importance(model_RF16)

plot(model_RF16)


prediction_RF16 <- predict(model_RF16, newdata = test_cl_top) # The next step is to validate our model using the test data

result_train_16 <- data.frame( test_cl_top,
                           prediction_RF16)

result_train_16

data.frame(RMSE = RMSE(prediction_RF16, test_cl_top$USD_price),
           R2 = R2(prediction_RF16, test_cl_top$USD_price))

hist(prediction_RF16)

#RMSE        R2
#1 0.02166621 0.1383044


#Linear Regression using all variables

model_LR16 <- lm(USD_price ~ ., data = train_cl_top)

model_LR16

summary(model_LR16)

summary(model_LR22)$coefficient


prediction_LR16 <- predict(model_LR16, newdata = test_cl_top) # The next step is to validate our model using the test data


result_train_LR16 <- data.frame(crypto = test_cl_top, 
                                price = test_cl_top$USD_price,
                                predictions = prediction_LR16)

# Model performance
data.frame(RMSE = RMSE(prediction_LR16, test_cl_top$USD_price),
           R2 = R2(prediction_LR16, test_cl_top$USD_price))

#RMSE           R2
#1 0.02324036 0.0003929851

#The RMSE value tells us that the average deviation between the predicted house price made by the model and the actual house price is $0.02324036.

#install.packages("performance")

performance_accuracy(model_LR16) #Validating the accuracy of the model

hist(prediction_LR16)

#Binomial Logistic model

set.seed(18)

glmModel_16 <- glm(USD_price~ . , data=train_cl_top, family=binomial)

summary(glmModel_16)

pred.glmModel_16 <- predict(glmModel_16, newdata=test_cl_top, type="response")

library(pROC)

roc.glmModel <- pROC::roc(test_cl_top$USD_price, pred.glmModel_16)

auc.glmModel <- pROC::auc(roc.glmModel) # checking if prediction is correct using AUC

#install.packages('gbm') #install gradient boost

model_gbm = gbm(train_cl$USD_price ~.,
                data = train_cl,
                distribution = "gaussian",
                cv.folds = 10,
                shrinkage = .01,
                n.minobsinnode = 10,
                n.trees = 500)

summary(model_gbm)

pred_y = predict.gbm(model_gbm, test_cl)
pred_y

plot(pred_y)




