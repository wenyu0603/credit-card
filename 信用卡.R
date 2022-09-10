BankChurners_ <- read.csv("/Users/yangwenyu/Documents/110-1/多變量分析/BankChurners_.csv", header=TRUE)
data = as.data.frame(BankChurners_)

# summary statistics
str(data)
summary(data)
options(digits=5) #計算到小數點後5位

#PCA
#選擇數值型變數
library(dplyr)
select <- select(data,c(Customer_Age, Months_on_book, Avg_Open_To_Buy, Credit_Limit, Total_Trans_Amt, Total_Trans_Ct, 
                        Total_Revolving_Bal, Avg_Utilization_Ratio, Dependent_count, Total_Relationship_Count, 
                        Months_Inactive_12_mon, Contacts_Count_12_mon,Total_Amt_Chng_Q4_Q1,  Total_Ct_Chng_Q4_Q1))
#數值型變數的相關係數
a = cor(select)
a

#選擇降維的變數
select1 <- select(data,c(Customer_Age, Months_on_book, Avg_Open_To_Buy, Credit_Limit, Total_Trans_Amt, Total_Trans_Ct, 
                        Total_Revolving_Bal, Avg_Utilization_Ratio))
pca <- prcomp(select1, scale = TRUE)                          
pca
#Standard deviations：特徵值開根號
#Rotation：特徵向量，也就是各個主成份，所對應的線性組合(linear combination)的係數

#陡坡圖(Scree plot)
plot(pca,         
     type="line", # 用直線連結每個點
     main="Scree Plot") # 主標題
# 用藍線標示出特徵值=1的地方
abline(h=1, col="blue")

#求出每個主成份的特徵值(也就是variance = std^2)
vars <- (pca$sdev)^2  
vars

#計算每個主成props <- vars / sum(vars)    
props <- vars / sum(vars)    
props

#累加每個主成份的解釋比例(aggregated effects)
cumulative.props <- cumsum(props) 
cumulative.props

#把累積解釋比例畫成圖
#當我們取前四個主成份，可以解釋 92.42% 的變異
cumulative.props[4]

# 累積解釋比例圖
plot(cumulative.props)

# pca$rotation 
top4_pca.data <- pca$x[, 1:4]
top4_pca.data 

# 特徵向量(原變數的線性組合)
pca$rotation

#取前四個主成份的特徵向量
top4.pca.eigenvector <- pca$rotation[, 1:4]
top4.pca.eigenvector


#繪製主成份負荷圖，觀察原變數和主成份之間的關係
first.pca <- top4.pca.eigenvector[, 1]   #  第一主成份
second.pca <- top4.pca.eigenvector[, 2]  #  第二主成份
third.pca <- top4.pca.eigenvector[, 3]   #  第三主成份
fourth.pca <- top4.pca.eigenvector[, 4]   #  第三主成份

# 第一主成份：由小到大排序原變數的係數
first.pca[order(first.pca, decreasing=FALSE)]  
# 使用dotchart，繪製主成份負荷圖
dotchart(first.pca[order(first.pca, decreasing=FALSE)] ,   # 排序後的係數
         main="Loading Plot for PC1",                      # 主標題
         xlab="Variable Loadings",                         # x軸的標題
         col="red")                                        # 顏色

# 第二主成份：由小到大排序原變數的係數
second.pca[order(second.pca, decreasing=FALSE)]  
dotchart(second.pca[order(second.pca, decreasing=FALSE)] ,  # 排序後的係數
         main="Loading Plot for PC2",                       # 主標題
         xlab="Variable Loadings",                          # x軸的標題
         col="blue")                                        # 顏色

# 第三主成份：由小到大排序原變數的係數
third.pca[order(third.pca, decreasing=FALSE)]  
# 使用dotchart，繪製主成份負荷圖
dotchart(third.pca[order(third.pca, decreasing=FALSE)] ,   # 排序後的係數
         main="Loading Plot for PC3",                      # 主標題
         xlab="Variable Loadings",                         # x軸的標題
         col="purple")                                     # 顏色

# 第四主成份：由小到大排序原變數的係數
fourth.pca[order(fourth.pca, decreasing=FALSE)]  
# 使用dotchart，繪製主成份負荷圖
dotchart(fourth.pca[order(fourth.pca, decreasing=FALSE)] ,   # 排序後的係數
         main="Loading Plot for PC4",                      # 主標題
         xlab="Variable Loadings",                         # x軸的標題
         col="green")                                     # 顏色

#logistic regression
#選擇未降維的資料欄位
select2 <- select(data,c(Attrition_Flag,Gender,Dependent_count,Education_Level,
                         Marital_Status,Income_Category,Card_Category,Months_Inactive_12_mon,
                         Contacts_Count_12_mon,Total_Amt_Chng_Q4_Q1,Total_Ct_Chng_Q4_Q1))
#將資料合併為新的資料
newdata <- cbind(select2,top4_pca.data)
newdata

library(tibble)
library(ggplot2)
library(gridExtra)

newdata = as.data.frame(newdata)
# summary statistics
str(newdata)
summary(newdata)

# check missing data
table(is.na(newdata))

#將類別型資料轉為因子
newdata$Attrition_Flag=factor(newdata$Attrition_Flag)
newdata$Gender=factor(newdata$Gender)
newdata$Education_Level=factor(newdata$Education_Level)
newdata$Marital_Status=factor(newdata$Marital_Status)
newdata$Income_Category=factor(newdata$Income_Category)
newdata$Card_Category=factor(newdata$Card_Category)

# split newdata in to training (80%) and testing (20%) data set
set.seed(42)
train.index = sample(x=1:nrow(newdata), size=ceiling(0.8*nrow(newdata) ))
train = newdata[train.index, ]
test = newdata[-train.index, ]

# set response variable into binary
# No=0, Yes=1
train$Attrition_Flag = as.numeric(train$Attrition_Flag)-1
test$Attrition_Flag = as.numeric(test$Attrition_Flag) -1


#=====================
# model fitting
#=====================

model_glm = glm(Attrition_Flag ~ .,data = train, family = "binomial")
summary(model_glm)

# prediction the log odd ratio for each sample
head(predict(model_glm))
head(predict(model_glm, type = "link"))

# predict Attrition_Flag prob of each sample
head(predict(model_glm, type='response'))

# make confusion table by setting default pro>=0.5 as 1

result = predict(model_glm, type='response')
result = ifelse(result > 0.5,1,0)
head(result)

trn_tab = table(predicted = result, actual = train$Attrition_Flag)
trn_tab


#=====================
# Prediction
#=====================

# Predicting Test Data
result = predict(model_glm,newdata=test,type='response')
result = ifelse(result > 0.5,1,0)

# Confusion matrix and statistics of the test data
library(caret)
confusionMatrix(data=as.factor(result), reference=as.factor(test$Attrition_Flag))

#=====================
# Model evaluation
#=====================


# Make ROC curve
library("pROC")
test_prob=predict(model_glm, newdata = test, type = "response")
par(pty="s") #指定畫布繪製成正方形的大小
test_roc=roc(test$Attrition_Flag ~ test_prob, plot = TRUE, print.auc = TRUE, legacy.axes=TRUE) 
#legacy.axes=TRUE 將 x軸改成 1 - Specificity
