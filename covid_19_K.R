library(ggplot2)
library(dplyr)
library(lubridate)#將char轉成時間格式
library(rpart)
library(rpart.plot)
library(rattle)
library(stringr)
library(dummies)
library(missRanger)
library(DMwR2)
library(class)


PatientInfo = read.csv("D:\\科目\\資料探勘\\期中\\dataset\\PatientInfo.csv")
str(PatientInfo)

#將空白轉成na，並查看遺失值
PatientInfo[PatientInfo[1:nrow(PatientInfo),]== ""]=NA
missing.value = colSums(is.na(PatientInfo))
missing.value =as.data.frame(missing.value)
miss = data.frame(column_name = rownames(missing.value),missing_number = missing.value$missing.value)

#處理資料
##stringr replace 把age欄位裡面的s替代成空字串然後轉成numeric
PatientInfo$age = str_replace_all(PatientInfo$age, "s", "")
PatientInfo$age = as.numeric(PatientInfo$age)
str(PatientInfo)

#畫圖
miss %>% ggplot(aes(x=reorder(column_name,missing_number),y = missing_number,fill= column_name))+
    geom_bar(stat = "identity")+
    labs(x = "欄位名稱", y = "欄位遺失數",title = 'Number of missing values' )+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

#性別和康復狀態
a = PatientInfo %>% group_by(sex,state) %>% summarise(c=n())
a = a[-7:-9,] 
ggplot(data = a, aes(x = factor(sex),y = a$c,fill=state)) + 
    geom_bar(position="dodge",stat = "identity")+
    labs(x ="性別" , y = "人數", title = "性別和感染狀況")

#年齡和康復狀態
b = PatientInfo %>% group_by(age,state) %>% summarise(c=n())
b = b[-29:-31,]


ggplot(data = b, aes(x = age,y = b$c,fill=state)) + 
    geom_bar(position="dodge",stat = "identity")+
    labs(x ="性別" , y = "人數", title = "性別和感染狀況")


##做資料清理
###去除前五多遺失值的欄位
PatientInfo2 = PatientInfo[,-c(1,8,9,10,11,12,13)]
# #用randomforest補
PatientInfo2 = missRanger(PatientInfo2,.-state~.-state, pmm.k = 3, num.trees = 100)
# save(PatientInfo2, file="D:\\科目\\資料探勘\\PatientInfo2.rda")
# write.csv(PatientInfo2,file="D:\\科目\\資料探勘\\PatientInfo2.csv")
# #用直接把有遺失值的資料刪掉
# PatientInfo2 = na.omit(PatientInfo2)

#用knn補值
# missing.value = colSums(is.na(PatientInfo2))
# set.seed(666)
# n = nrow(PatientInfo2)
# newknn = PatientInfo2[sample(n),]
# t_knnidx <- sample(seq_len(n), size = round(0.7 * n))
# #分出70建模，30測試
# knntraindata = newknn[t_knnidx,]
# knntestdata = newknn[-t_knnidx,]
# 
# sex.knn <- knn(
#     knntraindata[,c(3,4,7)], 
#     knntestdata[,c(3,4,7)], 
#     knntraindata[,1],
#     k = 3, l = 0, prob = TRUE)
# 
# 
# 
# train_data1=   PatientInfo2[!is.na(PatientInfo2$sex), ]
# train_data1[,c(1,3,4,7)] %>% str()
# train_data1[,c(3,4,7)]=lapply(train_data1[,c(3,4,7)], function(x){
#     return(as.numeric(x))
# })
# test_data1= PatientInfo2[is.na(PatientInfo2$sex), ]
# 
# test_data1[,c(3,4,7)]=lapply(test_data1[,c(3,4,7)], function(x){
#     return(as.numeric(x))
# })
# sex.knn <- knn(
#     train_data1[,c(3,4,7)], 
#     test_data1[,c(3,4,7)], 
#     train_data1[,1],
#     k = 61,l = 0, prob = FALSE,use.all = F)
# 
# colSums(is.na(PatientInfo2))


#***

PatientInfo2=lapply(PatientInfo2, function(x){
    if(is.character(x))
        return(as.factor(x))
    else
        return(x)
}) %>% as.data.frame() 

##轉成dummy
# PatientInfo3 <- dummy.data.frame(PatientInfo2)
# 
# PatientInfo3 %>% str()
# 
# PatientInfo2 %>% str()
n = nrow(PatientInfo2)#獲得多少筆樣本
PatientInfo3 = fastDummies::dummy_cols(PatientInfo2,select_columns=colnames(PatientInfo2)[-7],
                                       remove_first_dummy = T,remove_selected_columns=T)


set.seed(666)
#將數據順序重新排列
newPatientInfo <- PatientInfo3[sample(n),]

#取出樣本數的idx
set.seed(666)
t_idx <- sample(seq_len(n), size = round(0.7 * n))

#分出70建模，30測試
traindata = newPatientInfo[t_idx,]
testdata = newPatientInfo[-t_idx,]

#建立決策樹模型(cart)

# traindata$age %>% str()
# traindata$age[is.na(traindata$age)]=mean(traindata$age,na.rm=T)
# traindata1=lapply(traindata,function(x){
#     if(is.character(x))
#         return(as.factor(x))
#     else
#         return(x)
# }) %>% as.data.frame()
# traindata1 %>% str()

cart.model<- rpart(state ~. , data = traindata)
prp(cart.model,         # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray")  # 最下面的節點塗上陰影  


pred = predict(cart.model, newdata=traindata, type="class")
confus.matrix = table(real= traindata$state, predict=pred)
# 計算預測準確率 = 對角線的數量/總數量
accuracy = sum(diag(confus.matrix))/sum(confus.matrix) # 對角線的數量/總數量

# traindata %>% dim()
# testdata %>% dim()


pred2 = predict(cart.model, newdata=testdata, type="class")
confus.matrix2 = table(real=testdata$state, predict=pred2)
# 計算預測準確率 = 對角線的數量/總數量
accuracy2 = sum(diag(confus.matrix2))/sum(confus.matrix2) # 對角線的數量/總數量

#調參
printcp(cart.model) # 先觀察未修剪的樹，CP欄位代表樹的成本複雜度參數
##交叉验证的估计误差（“xerror”列），以及标准误差(“xstd”列)，平均相对误差=xerror±xstd
    

# 利用能使決策樹具有最小誤差的CP來修剪樹
prunetree_cart.model = prune(cart.model, 
                             cp = cart.model$cptable[which.min(cart.model$cptable[,"xerror"]),"CP"]) 

