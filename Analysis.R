library(tidyverse)
data = read.csv("尼玛，终于清洗完了.csv")
DATA <-data %>% select(id,trueage,a1,f41,
                       cognition_1,cognition_2,cognition_3,cognition_4,cognition_5,
                       )
#描述性统计分析
library(stargazer)
stargazer(DATA, type = "text",digits = 2)
library(Hmisc)
res<-rcorr(as.matrix(DATA))
res
##################################model1##############
library(lavaan)
library(haven)
library(semPlot)
model1_1<-" 
  截距 =~cognition_1 + cognition_2 + cognition_3 + cognition_4 + cognition_5
  斜率 =~0*cognition_1+3*cognition_2+6*cognition_3+9*cognition_4+13*cognition_5
  斜率+截距~trueage
"
fit1_1 <- growth(model1_1,DATA,se="bootstrap",bootstrap=5000)
summary(fit1_1,standardized=TRUE)
fitmeasures(fit1_1,c("chisq","df","pvalue","rmsea","srmr","cfi","gfi"))
p1_1 = semPaths(fit1_1)

model1_2<-" 
  截距 =~cognition_1 + cognition_2 + cognition_3 + cognition_4 + cognition_5
  斜率 =~0*cognition_1+3*cognition_2+6*cognition_3+9*cognition_4+13*cognition_5
  曲率 =~0*cognition_1+9*cognition_2+36*cognition_3+81*cognition_4+169*cognition_5
"
fit1_2 <- growth(model1_2,DATA,se="bootstrap",bootstrap=5000)
summary(fit1_2,standardized=TRUE)
fitmeasures(fit1_2,c("chisq","df","pvalue","rmsea","srmr","cfi","gfi"))
p1_2 = semPaths(fit1_2)
#模型比较
anova(fit1_1, fit1_2)
##################################model2############
model2<-" 
  截距 =~cognition_1 + cognition_2 + cognition_3 + cognition_4 + cognition_5
  斜率 =~0*cognition_1+3*cognition_2+6*cognition_3+9*cognition_4+13*cognition_5
  截距+斜率 ~ trueage + a1 + f41
"
fit2 <- growth(model2,DATA,se="bootstrap",bootstrap=5000)
summary(fit2,standardized=TRUE)
fitmeasures(fit2,c("chisq","df","pvalue","rmsea","srmr","cfi"))
p2 = semPaths(fit2)

model3<-" 
  截距 =~cognition_1 + cognition_2 + cognition_3 + cognition_4 + cognition_5
  斜率 =~0*cognition_1+3*cognition_2+6*cognition_3+9*cognition_4+13*cognition_5
  曲率 =~0*cognition_1+9*cognition_2+36*cognition_3+81*cognition_4+169*cognition_5
  截距+斜率+曲率 ~ trueage + a1 + f41
"
fit3 <- growth(model3,DATA,se="bootstrap",bootstrap=5000)
summary(fit3,standardized=TRUE)
fitmeasures(fit3,c("chisq","df","pvalue","rmsea","srmr","cfi"))
p3 = semPaths(fit3)


