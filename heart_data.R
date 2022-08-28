library(factoextra)
library(fastDummies)
library(psych)
library(GPArotation)
library(datasets)
library(readr)
library(data.table)
library(dplyr)
library(cluster)
library(purrr)
library(ranger)
library(gridExtra)
library(ggplot2)
require(RColorBrewer)
library(tidyr)
mycolors <- colorRampPalette(brewer.pal(8, "Set3"))(20)

data_raw <- read.csv("D:\\科目\\資料探勘\\期末\\heart_duplicates.csv" , header=TRUE)
data_raw = data_raw[-1]
data <- data_raw[,c(-1,-14)]
scale_data <- scale(data)
data3 = cbind(scale_data,target = data_raw$target)

E.dist <- dist(scale_data, method="euclidean") # 歐式距離
fviz_nbclust(scale_data, FUN = hcut, method = "wss")#組內間距 ->分四群
fviz_nbclust(x = scale_data,FUNcluster = hcut, method = "silhouette")#輪廓係數 ->分2群

h.E.cluster <- hclust(E.dist)
h.cluster <- hclust(E.dist, method="ward.D2") # 歐式距離配上華德法

plot(hclust(E.dist, method="ward.D2"), xlab = "華德法: Ward's Method")
abline(h=19, col="red") # ->分四群

#################### 分 4 群   ####################
cut.h.cluster <- cutree(h.cluster, k=4)  # 分成4群
a = cut.h.cluster %>% as.data.frame()    # 分群結果

rect.hclust(tree =h.cluster, k = 4, border = "blue")

names(a)[1] <- "cluster"
cluster_result <- cbind(data_raw, cluster = a)
cluster_result$cluster %>% table()
hc_pc = prcomp(scale_data)
pca_plot = fviz_pca_ind(hc_pc, habillage = cut.h.cluster)
pca_plot

#################### 分 4 群  ####################

#################### 分 2 群  ####################
cut.h.cluster_3 <- cutree(h.cluster, k=2)  # 分成2群
b = cut.h.cluster_3 %>% as.data.frame()    # 分群結果

rect.hclust(tree =h.cluster, k = 2, border = "blue")

names(b)[1] <- "cluster"
cluster_result_3 <- cbind(data_raw, cluster = b)
cluster_result_3$cluster %>% table

hc_pc = prcomp(scale_data)
pca_plot_3 = fviz_pca_ind(hc_pc, habillage = cut.h.cluster_3)
pca_plot_3

####################  分 2 群  ####################

continuous_df = cluster_result_3 %>% group_by(cluster) %>% summarise(年齡=mean(age),血壓=mean(trestbps),
                                                                   膽固醇指數=mean(chol),心跳計數 = mean(thalach),
                                                                   運動引起的ST = mean(oldpeak)*100)


continuous_df1=continuous_df %>% gather(key = "key",value = "value",-cluster)
continuous_df1

#連續數值
continuous_df1 %>% ggplot( aes(x=as.factor(cluster),y=value,fill=key )) + 
    geom_bar(position="dodge",stat = "identity")+
    scale_fill_manual(values=mycolors[c(1,14,7,8,12)])+theme(text = element_text(family='STHeitiTC-Light'))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

#類別數值
# cluster_result %>% group_by(cluster) %>% summarise(性別 = n(sex),胸痛 = n(cp),
#                                                      血糖= n(fbs),心電圖疼痛程度 = n(restecg),
#                                                      運動誘發心絞痛 = n(exang),運動的st狀況 = n(slope),
#                                                      熒光檢查著色的血管數 = n(ca),鉈測試 = n(thal),
#                                                      心臟衰竭可能性 = n(target)
#                                                      )

continuous_df2 = cluster_result %>% group_by(cluster) %>% summarise(sex = sum(sex)/n(),cp = sum(cp)/n(),
                                                   fbs= sum(fbs)/n(),restecg = sum(restecg)/n(),
                                                   exang = sum(exang)/n(),slope = sum(slope)/n(),
                                                   ca = sum(ca)/n(),thal = sum(thal)/n(),
                                                   target = sum(target)/n())


bbb = cluster_result[cluster_result$cluster ==1,]
nrow(bbb[bbb$sex==1,])

continuous_df2=continuous_df2 %>% gather(key = "key",value = "value",-cluster)
continuous_df2

continuous_df2 %>% ggplot( aes(x=as.factor(cluster),y=value,fill=key )) + 
    geom_bar(position="dodge",stat = "identity")


be_factor = function(x){
    x = as.factor(x)
    return(x)
}


cluster_result$sex = as.factor(cluster_result$sex)
cluster_result$cp = as.factor(cluster_result$cp)
cluster_result$fbs = as.factor(cluster_result$fbs)
cluster_result$restecg = as.factor(cluster_result$restecg)
cluster_result$exang = as.factor(cluster_result$exang)
cluster_result$slope = as.factor(cluster_result$slope)
cluster_result$ca = as.factor(cluster_result$ca)
cluster_result$target = as.factor(cluster_result$target)



##cluster 1
cluster_1 = cluster_result[cluster_result$cluster==1,]
#sex


sum(cluster_1$sex==0)#0
sum(cluster_1$sex==1)#108


#cp
sum(cluster_1$cp ==0)#32
sum(cluster_1$cp ==1)#26
sum(cluster_1$cp ==2)#36
sum(cluster_1$cp ==3)#14

#fbs
sum(cluster_1$fbs==0)#108
sum(cluster_1$fbs==1)#0

#restecg
sum(cluster_1$restecg==0)#48
sum(cluster_1$restecg==1)#60
sum(cluster_1$restecg==2)#0

#exang
sum(cluster_1$exang==0)#92
sum(cluster_1$exang==1)#16

#slope
sum(cluster_1$slope==0)#4
sum(cluster_1$slope==1)#29
sum(cluster_1$slope==2)#75

#ca
sum(cluster_1$ca==0)#73
sum(cluster_1$ca==1)#20
sum(cluster_1$ca==2)#8
sum(cluster_1$ca==3)#4
sum(cluster_1$ca==4)#3

#target
sum(cluster_1$target==0)#42
sum(cluster_1$target==1)#66

x = c(rep(1, each = 23))
y = c("性別0","性別1","cp0","cp1","cp2","cp3","fbs0","fbs1","restecg0","restecg1","restecg2","exang0","exang1",
      "slope0","slope1","slope2","ca0","ca1","ca2","ca3","ca4","target0","target1")
z = c(0,108,32,26,36,14,108,0,48,60,0,92,16,4,29,75,73,20,8,4,3,42,66)

category_df = data.frame(cluster = x , 類別 = y , value = z)



##cluster 2
cluster_2 = cluster_result[cluster_result$cluster==2,]
#sex
sum(cluster_2$sex==0)#32
sum(cluster_2$sex==1)#73


#cp
sum(cluster_2$cp ==0)#91
sum(cluster_2$cp ==1)#4
sum(cluster_2$cp ==2)#8
sum(cluster_2$cp ==3)#2

#fbs
sum(cluster_2$fbs==0)#92
sum(cluster_2$fbs==1)#13

#restecg
sum(cluster_2$restecg==0)#59
sum(cluster_2$restecg==1)#42
sum(cluster_2$restecg==2)#4

#exang
sum(cluster_2$exang==0)#29
sum(cluster_2$exang==1)#76

#slope
sum(cluster_2$slope==0)#14
sum(cluster_2$slope==1)#80
sum(cluster_2$slope==2)#11

#ca
sum(cluster_2$ca==0)#42
sum(cluster_2$ca==1)#29
sum(cluster_2$ca==2)#21
sum(cluster_2$ca==3)#13
sum(cluster_2$ca==4)#0

#target
sum(cluster_2$target==0)#85
sum(cluster_2$target==1)#20

x2 = c(rep(2, each = 23))
y2 = c("性別0","性別1","cp0","cp1","cp2","cp3","fbs0","fbs1","restecg0","restecg1","restecg2","exang0","exang1",
      "slope0","slope1","slope2","ca0","ca1","ca2","ca3","ca4","target0","target1")
z2 = c(32,73,91,4,8,2,92,13,59,42,4,29,76,14,80,11,42,29,21,13,0,85,20)



category_df2 = data.frame(cluster = x2 , 類別 = y2 , value = z2)

category_df = rbind(category_df,category_df2)



##cluster 3
cluster_3 = cluster_result[cluster_result$cluster==3,]
#sex
sum(cluster_3$sex==0)#58
sum(cluster_3$sex==1)#0


#cp
sum(cluster_3$cp ==0)#14
sum(cluster_3$cp ==1)#15
sum(cluster_3$cp ==2)#27
sum(cluster_3$cp ==3)#2

#fbs
sum(cluster_3$fbs==0)#57
sum(cluster_3$fbs==1)#1

#restecg
sum(cluster_3$restecg==0)#23
sum(cluster_3$restecg==1)#35
sum(cluster_3$restecg==2)#0

#exang
sum(cluster_3$exang==0)#58
sum(cluster_3$exang==1)#0

#slope
sum(cluster_3$slope==0)#0
sum(cluster_3$slope==1)#22
sum(cluster_3$slope==2)#36

#ca
sum(cluster_3$ca==0)#45
sum(cluster_3$ca==1)#9
sum(cluster_3$ca==2)#4
sum(cluster_3$ca==3)#0
sum(cluster_3$ca==4)#0

#target
sum(cluster_3$target==0)#2
sum(cluster_3$target==1)#56

x3 = c(rep(3, each = 23))
y3 = c("性別0","性別1","cp0","cp1","cp2","cp3","fbs0","fbs1","restecg0","restecg1","restecg2","exang0","exang1",
       "slope0","slope1","slope2","ca0","ca1","ca2","ca3","ca4","target0","target1")
z3 = c(58,0,14,15,27,2,57,1,23,35,0,58,0,0,22,36,45,9,4,0,0,2,56)



category_df3 = data.frame(cluster = x3 , 類別 = y3 , value = z3)

category_df = rbind(category_df,category_df3)

percent = as.numeric(category_df$value)/302

category_df %>% mutate(percent = percent)

#類別數值

category_df %>% ggplot( aes(x=as.factor(cluster),y=percent,fill=類別 )) + 
    geom_bar(stat = "identity")



