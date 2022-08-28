library(ggplot2)
library(dplyr)
library(lubridate)#將char轉成時間格式
library(rpart)
library(rpart.plot)
library(rattle)
library(stringr)
library(dummies)
library(missRanger)
library(e1071)
library(randomForest)

PatientInfo = read.csv("D:\\科目\\資料探勘\\期中\\dataset\\PatientInfo.csv")

str(PatientInfo)


#將空白轉成na，並查看遺失值
PatientInfo[PatientInfo[1:nrow(PatientInfo),]== ""]=NA
# missing.value = colSums(is.na(PatientInfo))
# missing.value =as.data.frame(missing.value)
# miss = data.frame(column_name = rownames(missing.value),missing_number = missing.value$missing.value)

#處理資料
##stringr replace 把age欄位裡面的s替代成空字串然後轉成numeric
PatientInfo$age = str_replace_all(PatientInfo$age, "s", "")
PatientInfo$age = as.numeric(PatientInfo$age)

##做資料清理
###去除前五多遺失值的欄位
PatientInfo = PatientInfo[,-c(1,8,9,10,11,12,13)]
PatientInfo = missRanger(PatientInfo,.-state~.-state, pmm.k = 3, num.trees = 100)

#將類別欄位轉成factor
PatientInfo = lapply(PatientInfo, function(x){
    if(is.character(x))
        return(as.factor(x))
    else
        return(x)
}) %>% as.data.frame()
str(PatientInfo)

#做dummy
n = nrow(PatientInfo)#獲得多少筆樣本
PatientInfo2 = fastDummies::dummy_cols(PatientInfo,select_columns=colnames(PatientInfo)[-7],
                                       remove_first_dummy = T,remove_selected_columns=T)


set.seed(666)
#將數據順序重新排列
newPatientInfo = PatientInfo2[sample(n),]
#取出樣本數的idx
set.seed(666)
t_idx = sample(seq_len(n), size = round(0.7 * n))
#分出70建模，30測試
traindata = newPatientInfo[t_idx,]
testdata = newPatientInfo[-t_idx,]

#建立svm
SVMmodel = svm(formula = state ~ .,  
            data = traindata)
pred_svm = predict(SVMmodel, newdata = testdata, type="class")
    confus.matrix_svm = table(real = testdata$state, predict = pred_svm)
# 計算預測準確率 = 對角線的數量/總數量
accuracy_svm = sum(diag(confus.matrix_svm))/sum(confus.matrix_svm) # 對角線的數量/總數量


#建立randomforest
a = janitor::clean_names(traindata)
b = janitor::clean_names(testdata)
colnames(traindata) = colnames(a)
colnames(testdata) = colnames(b)
rf_model = randomForest(state~.,
                        data = traindata,
                        ntree = 150)
# rf_model = ranger(state~.,
#                         data = traindata,
#                   num.trees = 150)

pred_rf = predict(rf_model, newdata = testdata, type="class")
confus.matrix_rf = table(real = testdata$state, predict = pred_rf)
# 計算預測準確率 = 對角線的數量/總數量
accuracy_rf = sum(diag(confus.matrix_rf))/sum(confus.matrix_rf) # 對角線的數量/總數量

a = janitor::clean_names(PatientInfo2)
colnames(PatientInfo2) = colnames(a)
churners_model = ranger(state ~ . ,
                        data = PatientInfo,  
                        seed = 1,
                        importance = 'impurity', #impurity不純度 gini(?)
                        mtry = NULL,
                        verbose = TRUE,
                        num.trees = 50,
                        write.forest=TRUE)

var_impor = churners_model$variable.importance %>% as.data.frame()
var_impor=data.frame(var_name = row.names(var_impor) , value=var_impor$.)
var_impor
#畫出重要性變數
bar_plot <- ggplot(var_impor, mapping =  aes(x = reorder(var_name,value), y = value , fill = var_name)) +
    labs(x ="變數名稱" , y = "重要性" )+
    geom_bar(stat = "identity") + coord_flip()

bar_plot


