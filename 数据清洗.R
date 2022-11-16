##############################################选择变量#############################
library(tidyverse)
data_origin <- read.csv("05_18.csv")
select_variables<-data_origin %>% select(id,residenc,trueage,a1,a2,a51,a530,f2,f1,monthin,year_8,month_8,yearin_11,monthin_11,yearin_14,monthin_14,yearin_18,monthin_18,
                                         b11,b12, 
                                         d71,d81,d91,d11d,d11f,d11g,d11h,
                                         #depression
                                         b21,b23,b24,b26,b27,
                                         #cognition
                                         c11,c12,c13,c14,c15,c16,c21a,c21b,c21c,c22,c31a,c31b,c31c,c31d,c31e,c32,c41a,c41b,c41c,c51a,c51b,c52,c53a,c53b,c53c,
                                         #marriage
                                         f41,f42,f43a4,f43b4,f43c4,f43d4,
                                         b11_8,b12_8, 
                                         d71_8,d81_8,d91_8,d11d_8,d11f_8,d11g_8,d11h_8,
                                         b21_8,b23_8,b24_8,b26_8,b27_8,
                                         c11_8,c12_8,c13_8,c14_8,c15_8,c16_8,c21a_8,c21b_8,c21c_8,c22_8,c31a_8,c31b_8,c31c_8,c31d_8,c31e_8,c32_8,c41a_8,c41b_8,c41c_8,c51a_8,c51b_8,c52_8,c53a_8,c53b_8,c53c_8,
                                         f41_8,f42_8,f43a4_8,f43b4_8,f43c4_8,f43d4_8,
                                         b11_11,b12_11, 
                                         d71_11,d81_11,d91_11,d11d_11,d11f_11,d11g_11,d11h_11,
                                         b21_11,b23_11,b24_11,b26_11,b27_11,
                                         c11_11,c12_11,c13_11,c14_11,c15_11,c16_11,c21a_11,c21b_11,c21c_11,c22_11,c31a_11,c31b_11,c31c_11,c31d_11,c31e_11,c32_11,c41a_11,c41b_11,c41c_11,c51a_11,c51b_11,c52_11,c53a_11,c53b_11,c53c_11,
                                         f41_11,f42_11,f43a4_11,f43b4_11,f43c4_11,f43d4_11,
                                         b11_14,b12_14, 
                                         d71_14,d81_14,d91_14,d11d_14,d11f_14,d11g_14,d11h_14,
                                         b21_14,b23_14,b24_14,b26_14,b27_14,
                                         c11_14,c12_14,c13_14,c14_14,c15_14,c16_14,c21a_14,c21b_14,c21c_14,c22_14,c31a_14,c31b_14,c31c_14,c31d_14,c31e_14,c32_14,c41a_14,c41b_14,c41c_14,c51a_14,c51b_14,c52_14,c53a_14,c53b_14,c53c_14,
                                         f41_14,f42_14,f43a4_14,f43b4_14,f43c4_14,f43d4_14,
                                         b11_18,b12_18, 
                                         d71_18,d81_18,d91_18,d11d_18,d11f_18,d11g_18,d11h_18,
                                         b21_18,b23_18,b24_18,b26_18,b27_18,
                                         c11_18,c12_18,c13_18,c14_18,c15_18,c16_18,c21a_18,c21b_18,c21c_18,c22_18,c31a_18,c31b_18,c31c_18,c31d_18,c31e_18,c32_18,c41a_18,c41b_18,c41c_18,c51a_18,c51b_18,c52_18,c53a_18,c53b_18,c53c_18,
                                         f41_18,f42_18,f43a4_18,f43b4_18,f43c4_18,f43d4_18)
######################################变量清洗#######################################
#删除年龄不满60岁的(选择年龄大运59岁)和年龄缺失值
datatrueage <- subset(select_variables,select_variables$trueage>59)
data_trueage <- datatrueage %>% drop_na(trueage)
#居住地类型
data_residenc <- data_trueage %>% drop_na(residenc)
#a1年龄
data_a1 <- data_trueage %>% drop_na(a1)
#a2民族
dataa2 <- data_a1 %>% drop_na(a2)
data_a2 <- subset(dataa2,dataa2$a2<8)
#a51居住类型（与人共同生活）
data_a51 <- subset(data_a2,a51<9)
#f1受教育年限
data_f1 <- subset(data_a51,f1<87)
#f2职业
data_f2 <- subset(data_f1,f2<8)
#a530房屋类型
#data_a530 <- subset(data_f2,a530<9)
###############################WAVE1#######################################
#b11自我汇报的生活质量
data_b11 <- subset(data_f2,b11<8)
#b12自我汇报的身体健康
data_b12 <- subset(data_b11,b12<8)
#抑郁
data_b2 <- subset(data_b12,b21<8 & b23<8 & b24<8 & b23<8 & b24<8 & b26<8 & b27<8)
#认知
data_c1 <- subset(data_b2,c11!=9 & c12!=9 & c13!=9 & c14!=9 & c15!=9 & c16!=99 & c21a!=9 & c21b!=9 
                  & c21c!=9 & c22!=9 & c31a!=9 & c31b!=9 & c31c!=9 & c31d!=9 & c31e!=9 & c32!=9 & c32!=9
                  & c32!=9 & c41a!=9 & c41b!=9 & c41c!=9 & c51a!=9 & c51b!=9 & c52!=9 & c53a!=9 & c53b!=9
                  & c53c!=9)
data_c1$c11[which(data_c1$c11 == "8")] <- '0'
data_c1$c12[which(data_c1$c12 == "8")] <- '0'
data_c1$c13[which(data_c1$c13 == "8")] <- '0'
data_c1$c14[which(data_c1$c14 == "8")] <- '0'
data_c1$c15[which(data_c1$c15 == "8")] <- '0'
data_c1$c16[which(data_c1$c16 == "88")] <- "0"
data_c1$c21a[which(data_c1$c21a == "8")] <- '0'
data_c1$c21b[which(data_c1$c21b == "8")] <- '0'
data_c1$c22[which(data_c1$c22 == "7")] <- '0'
data_c1$c22[which(data_c1$c22 == "8")] <- '0'
data_c1$c21c[which(data_c1$c21c == "8")] <- '0'
data_c1$c31a[which(data_c1$c31a == "8")] <- '0'
data_c1$c31b[which(data_c1$c31b == "8")] <- '0'
data_c1$c31c[which(data_c1$c31c == "8")] <- '0'
data_c1$c31d[which(data_c1$c31d == "8")] <- '0'
data_c1$c31e[which(data_c1$c31e == "8")] <- '0'
data_c1$c32[which(data_c1$c32 == "8")] <- '0'
data_c1$c41a[which(data_c1$c41a == "8")] <- '0'
data_c1$c41b[which(data_c1$c41b == "8")] <- '0'
data_c1$c41c[which(data_c1$c41c == "8")] <- '0'
data_c1$c51b[which(data_c1$c51b == "8")] <- '0'
data_c1$c52[which(data_c1$c52 == "8")] <- '0'
data_c1$c53a[which(data_c1$c53a == "8")] <- '0'
data_c1$c53b[which(data_c1$c53b == "8")] <- '0'
data_c1$c53c[which(data_c1$c53c == "8")] <- '0'
write.csv(data_c1,file = "delWave1.csv")

###############################WAVE2#######################################
delWave1 <- read.csv("delWave1.csv")
#b11自我汇报的生活质量
data_b11_8 <- subset(delWave1,b11_8<8)
#b12自我汇报的身体健康
data_b12_8 <- subset(data_b11_8,b12_8<8)
#抑郁
data_b2_8 <- subset(data_b12_8,b21_8<8 & b23_8<8 & b24_8<8 & b23_8<8 & b24_8<8 & b26_8<8 & b27_8<8)
#认知
data_c1_8 <- subset(data_b2_8,c11_8!=9 & c12_8!=9 & c13_8!=9 & c14_8!=9 & c15_8!=9 & c16_8!=99 & c21a_8!=9 & c21b_8!=9 
                  & c21c_8!=9 & c22_8!=9 & c31a_8!=9 & c31b_8!=9 & c31c_8!=9 & c31d_8!=9 & c31e_8!=9 & c32_8!=9 & c32_8!=9
                  & c32_8!=9 & c41a_8!=9 & c41b_8!=9 & c41c_8!=9 & c51a_8!=9 & c51b_8!=9 & c52_8!=9 & c53a_8!=9 & c53b_8!=9
                  & c53c_8!=9)
data_c1_8$c11_8[which(data_c1_8$c11_8 == "8")] <- '0'
data_c1_8$c12_8[which(data_c1_8$c12_8 == "8")] <- '0'
data_c1_8$c13_8[which(data_c1_8$c13_8 == "8")] <- '0'
data_c1_8$c14_8[which(data_c1_8$c14_8 == "8")] <- '0'
data_c1_8$c15_8[which(data_c1_8$c15_8 == "8")] <- '0'
data_c1_8$c16_8[which(data_c1_8$c16_8 == "88")] <- '0'
data_c1_8$c21a_8[which(data_c1_8$c21a_8 == "8")] <- '0'
data_c1_8$c21b_8[which(data_c1_8$c21b_8 == "8")] <- '0'
data_c1_8$c22_8[which(data_c1_8$c22_8 == "7")] <- '0'
data_c1_8$c22_8[which(data_c1_8$c22_8 == "8")] <- '0'
data_c1_8$c21c_8[which(data_c1_8$c21c_8 == "8")] <- '0'
data_c1_8$c31a_8[which(data_c1_8$c31a_8 == "8")] <- '0'
data_c1_8$c31b_8[which(data_c1_8$c31b_8 == "8")] <- '0'
data_c1_8$c31c_8[which(data_c1_8$c31c_8 == "8")] <- '0'
data_c1_8$c31d_8[which(data_c1_8$c31d_8 == "8")] <- '0'
data_c1_8$c31e_8[which(data_c1_8$c31e_8 == "8")] <- '0'
data_c1_8$c32_8[which(data_c1_8$c32_8 == "8")] <- '0'
data_c1_8$c41a_8[which(data_c1_8$c41a_8 == "8")] <- '0'
data_c1_8$c41b_8[which(data_c1_8$c41b_8 == "8")] <- '0'
data_c1_8$c41c_8[which(data_c1_8$c41c_8 == "8")] <- '0'
data_c1_8$c51b_8[which(data_c1_8$c51b_8 == "8")] <- '0'
data_c1_8$c52_8[which(data_c1_8$c52_8 == "8")] <- '0'
data_c1_8$c53a_8[which(data_c1_8$c53a_8 == "8")] <- '0'
data_c1_8$c53b_8[which(data_c1_8$c53b_8 == "8")] <- '0'
data_c1_8$c53c_8[which(data_c1_8$c53c_8 == "8")] <- '0'

write.csv(data_c1_8,file = "delWave2.csv")

###############################WAVE3#######################################
delWave2 <- read.csv("delWave2.csv")
#b11自我汇报的生活质量
data_b11_11 <- subset(delWave2,b11_11<8)
#b12自我汇报的身体健康
data_b12_11 <- subset(data_b11_11,b12_11<8)
#抑郁
data_b2_11 <- subset(data_b12_11,b21_11<8 & b23_11<8 & b24_11<8 & b23_11<8 & b24_11<8 & b26_11<8 & b27_11<8)
#认知
data_c1_11 <- subset(data_b2_11,c11_11!=9 & c12_11!=9 & c13_11!=9 & c14_11!=9 & c15_11!=9 & c16_11!=99 & c21a_11!=9 & c21b_11!=9 
                    & c21c_11!=9 & c22_11!=9 & c31a_11!=9 & c31b_11!=9 & c31c_11!=9 & c31d_11!=9 & c31e_11!=9 & c32_11!=9 & c32_11!=9
                    & c32_11!=9 & c41a_11!=9 & c41b_11!=9 & c41c_11!=9 & c51a_11!=9 & c51b_11!=9 & c52_11!=9 & c53a_11!=9 & c53b_11!=9
                    & c53c_11!=9)
data_c1_11$c11_11[which(data_c1_11$c11_11 == "8")] <- '0'
data_c1_11$c12_11[which(data_c1_11$c12_11 == "8")] <- '0'
data_c1_11$c13_11[which(data_c1_11$c13_11 == "8")] <- '0'
data_c1_11$c14_11[which(data_c1_11$c14_11 == "8")] <- '0'
data_c1_11$c15_11[which(data_c1_11$c15_11 == "8")] <- '0'
data_c1_11$c16_11[which(data_c1_11$c16_11 == "88")] <- '0'
data_c1_11$c21a_11[which(data_c1_11$c21a_11 == "8")] <- '0'
data_c1_11$c21b_11[which(data_c1_11$c21b_11 == "8")] <- '0'
data_c1_11$c22_11[which(data_c1_11$c22_11 == "7")] <- '0'
data_c1_11$c22_11[which(data_c1_11$c22_11 == "8")] <- '0'
data_c1_11$c21c_11[which(data_c1_11$c21c_11 == "8")] <- '0'
data_c1_11$c31a_11[which(data_c1_11$c31a_11 == "8")] <- '0'
data_c1_11$c31b_11[which(data_c1_11$c31b_11 == "8")] <- '0'
data_c1_11$c31c_11[which(data_c1_11$c31c_11 == "8")] <- '0'
data_c1_11$c31d_11[which(data_c1_11$c31d_11 == "8")] <- '0'
data_c1_11$c31e_11[which(data_c1_11$c31e_11 == "8")] <- '0'
data_c1_11$c32_11[which(data_c1_11$c32_11 == "8")] <- '0'
data_c1_11$c41a_11[which(data_c1_11$c41a_11 == "8")] <- '0'
data_c1_11$c41b_11[which(data_c1_11$c41b_11 == "8")] <- '0'
data_c1_11$c41c_11[which(data_c1_11$c41c_11 == "8")] <- '0'
data_c1_11$c51b_11[which(data_c1_11$c51b_11 == "8")] <- '0'
data_c1_11$c52_11[which(data_c1_11$c52_11 == "8")] <- '0'
data_c1_11$c53a_11[which(data_c1_11$c53a_11 == "8")] <- '0'
data_c1_11$c53b_11[which(data_c1_11$c53b_11 == "8")] <- '0'
data_c1_11$c53c_11[which(data_c1_11$c53c_11 == "8")] <- '0'

write.csv(data_c1_11,file = "delWave3.csv")

###############################WAVE4#######################################
delWave3 <- read.csv("delWave3.csv")
#b11自我汇报的生活质量
data_b11_14 <- subset(delWave3,b11_14<8)
#b12自我汇报的身体健康
data_b12_14 <- subset(data_b11_14,b12_14<8)
#抑郁
data_b2_14 <- subset(data_b12_14,b21_14<8 & b23_14<8 & b24_14<8 & b23_14<8 & b24_14<8 & b26_14<8 & b27_14<8)
#认知
data_c1_14 <- subset(data_b2_14,c11_14!=9 & c12_14!=9 & c13_14!=9 & c14_14!=9 & c15_14!=9 & c16_14!=99 & c21a_14!=9 & c21b_14!=9 
                     & c21c_14!=9 & c22_14!=9 & c31a_14!=9 & c31b_14!=9 & c31c_14!=9 & c31d_14!=9 & c31e_14!=9 & c32_14!=9 & c32_14!=9
                     & c32_14!=9 & c41a_14!=9 & c41b_14!=9 & c41c_14!=9 & c51a_14!=9 & c51b_14!=9 & c52_14!=9 & c53a_14!=9 & c53b_14!=9
                     & c53c_14!=9)
data_c1_14$c11_14[which(data_c1_14$c11_14 == "8")] <- '0'
data_c1_14$c12_14[which(data_c1_14$c12_14 == "8")] <- '0'
data_c1_14$c13_14[which(data_c1_14$c13_14 == "8")] <- '0'
data_c1_14$c14_14[which(data_c1_14$c14_14 == "8")] <- '0'
data_c1_14$c15_14[which(data_c1_14$c15_14 == "8")] <- '0'
data_c1_14$c16_14[which(data_c1_14$c16_14 == "88")] <- '0'
data_c1_14$c16_14[which(data_c1_14$c16_14 > "7")] <- '7'
data_c1_14$c21a_14[which(data_c1_14$c21a_14 == "8")] <- '0'
data_c1_14$c21b_14[which(data_c1_14$c21b_14 == "8")] <- '0'
data_c1_14$c22_14[which(data_c1_14$c22_14 == "7")] <- '0'
data_c1_14$c22_14[which(data_c1_14$c22_14 == "8")] <- '0'
data_c1_14$c21c_14[which(data_c1_14$c21c_14 == "8")] <- '0'
data_c1_14$c31a_14[which(data_c1_14$c31a_14 == "8")] <- '0'
data_c1_14$c31b_14[which(data_c1_14$c31b_14 == "8")] <- '0'
data_c1_14$c31c_14[which(data_c1_14$c31c_14 == "8")] <- '0'
data_c1_14$c31d_14[which(data_c1_14$c31d_14 == "8")] <- '0'
data_c1_14$c31e_14[which(data_c1_14$c31e_14 == "8")] <- '0'
data_c1_14$c32_14[which(data_c1_14$c32_14 == "8")] <- '0'
data_c1_14$c41a_14[which(data_c1_14$c41a_14 == "8")] <- '0'
data_c1_14$c41b_14[which(data_c1_14$c41b_14 == "8")] <- '0'
data_c1_14$c41c_14[which(data_c1_14$c41c_14 == "8")] <- '0'
data_c1_14$c51b_14[which(data_c1_14$c51b_14 == "8")] <- '0'
data_c1_14$c52_14[which(data_c1_14$c52_14 == "8")] <- '0'
data_c1_14$c53a_14[which(data_c1_14$c53a_14 == "8")] <- '0'
data_c1_14$c53b_14[which(data_c1_14$c53b_14 == "8")] <- '0'
data_c1_14$c53c_14[which(data_c1_14$c53c_14 == "8")] <- '0'

write.csv(data_c1_14,file = "delWave4.csv")

###############################WAVE5#######################################
delWave4 <- read.csv("delWave4.csv")
#b11自我汇报的生活质量
data_b11_18 <- subset(delWave4,b11_18<8)
#b12自我汇报的身体健康
data_b12_18 <- subset(data_b11_18,b12_18<8)
#抑郁
data_b2_18 <- subset(data_b12_18,b21_18<8 & b23_18<8 & b24_18<8 & b23_18<8 & b24_18<8 & b26_18<8 & b27_18<8)
#认知
data_c1_18 <- subset(data_b2_18,c11_18!=9 & c12_18!=9 & c13_18!=9 & c14_18!=9 & c15_18!=9 & c16_18!=99 & c21a_18!=9 & c21b_18!=9 
                     & c21c_18!=9 & c22_18!=9 & c31a_18!=9 & c31b_18!=9 & c31c_18!=9 & c31d_18!=9 & c31e_18!=9 & c32_18!=9 & c32_18!=9
                     & c32_18!=9 & c41a_18!=9 & c41b_18!=9 & c41c_18!=9 & c51a_18!=9 & c51b_18!=9 & c52_18!=9 & c53a_18!=9 & c53b_18!=9
                     & c53c_18!=9)
data_c1_18$c11_18[which(data_c1_18$c11_18 == "8")] <- '0'
data_c1_18$c12_18[which(data_c1_18$c12_18 == "8")] <- '0'
data_c1_18$c13_18[which(data_c1_18$c13_18 == "8")] <- '0'
data_c1_18$c14_18[which(data_c1_18$c14_18 == "8")] <- '0'
data_c1_18$c15_18[which(data_c1_18$c15_18 == "8")] <- '0'
data_c1_18$c16_18[which(data_c1_18$c16_18 == "88")] <- '0'
data_c1_18$c21a_18[which(data_c1_18$c21a_18 == "8")] <- '0'
data_c1_18$c21b_18[which(data_c1_18$c21b_18 == "8")] <- '0'
data_c1_18$c22_18[which(data_c1_18$c22_18 == "7")] <- '0'
data_c1_18$c22_18[which(data_c1_18$c22_18 == "8")] <- '0'
data_c1_18$c21c_18[which(data_c1_18$c21c_18 == "8")] <- '0'
data_c1_18$c31a_18[which(data_c1_18$c31a_18 == "8")] <- '0'
data_c1_18$c31b_18[which(data_c1_18$c31b_18 == "8")] <- '0'
data_c1_18$c31c_18[which(data_c1_18$c31c_18 == "8")] <- '0'
data_c1_18$c31d_18[which(data_c1_18$c31d_18 == "8")] <- '0'
data_c1_18$c31e_18[which(data_c1_18$c31e_18 == "8")] <- '0'
data_c1_18$c32_18[which(data_c1_18$c32_18 == "8")] <- '0'
data_c1_18$c41a_18[which(data_c1_18$c41a_18 == "8")] <- '0'
data_c1_18$c41b_18[which(data_c1_18$c41b_18 == "8")] <- '0'
data_c1_18$c41c_18[which(data_c1_18$c41c_18 == "8")] <- '0'
data_c1_18$c51b_18[which(data_c1_18$c51b_18 == "8")] <- '0'
data_c1_18$c52_18[which(data_c1_18$c52_18 == "8")] <- '0'
data_c1_18$c53a_18[which(data_c1_18$c53a_18 == "8")] <- '0'
data_c1_18$c53b_18[which(data_c1_18$c53b_18 == "8")] <- '0'
data_c1_18$c53c_18[which(data_c1_18$c53c_18 == "8")] <- '0'

write.csv(data_c1_18,file = "delWave5.csv")

#############################################反向计分处理#######################################
data <- read.csv("data-计分.csv")
data$depression_1 <- 6-data$b21+data$b23+data$b24+data$b26+6-data$b27
data$depression_2 <- 6-data$b21_8+data$b23_8+data$b24_8+data$b26_8+6-data$b27_8
data$depression_3 <- 6-data$b21_11+data$b23_11+data$b24_11+data$b26_11+6-data$b27_11
data$depression_4 <- 6-data$b21_14+data$b23_14+data$b24_14+data$b26_14+6-data$b27_14
data$depression_5 <- 6-data$b21_18+data$b23_18+data$b24_18+data$b26_18+6-data$b27_18
data$cognition_1 <- data$c11+data$c12+data$c13+data$c14+data$c15+data$c16+data$c21a+data$c21b+data$c21c+data$c22+data$c31a+data$c31b+
  data$c31c+data$c31d+data$c31e+data$c32+data$c41a+data$c41b+data$c41c+data$c51a+data$c51b+data$c52+data$c53a+data$c53b+data$c53c
data$cognition_2 <- data$c11_8+data$c12_8+data$c13_8+data$c14_8+data$c15_8+data$c16_8+data$c21a_8+data$c21b_8+data$c21c_8+data$c22_8+data$c31a_8+data$c31b_8+
  data$c31c_8+data$c31d_8+data$c31e_8+data$c32_8+data$c41a_8+data$c41b_8+data$c41c_8+data$c51a_8+data$c51b_8+data$c52_8+data$c53a_8+data$c53b_8+data$c53c_8
data$cognition_3 <- data$c11_11+data$c12_11+data$c13_11+data$c14_11+data$c15_11+data$c16_11+data$c21a_11+data$c21b_11+data$c21c_11+data$c22_11+data$c31a_11+data$c31b_11+
  data$c31c_11+data$c31d_11+data$c31e_11+data$c32_11+data$c41a_11+data$c41b_11+data$c41c_11+data$c51a_11+data$c51b_11+data$c52_11+data$c53a_11+data$c53b_11+data$c53c_11
data$cognition_4 <- data$c11_14+data$c12_14+data$c13_14+data$c14_14+data$c15_14+data$c16_14+data$c21a_14+data$c21b_14+data$c21c_14+data$c22_14+data$c31a_14+data$c31b_14+
  data$c31c_14+data$c31d_14+data$c31e_14+data$c32_14+data$c41a_14+data$c41b_14+data$c41c_14+data$c51a_14+data$c51b_14+data$c52_14+data$c53a_14+data$c53b_14+data$c53c_14
data$cognition_5 <- data$c11_18+data$c12_18+data$c13_18+data$c14_18+data$c15_18+data$c16_18+data$c21a_18+data$c21b_18+data$c21c_18+data$c22_18+data$c31a_18+data$c31b_18+
  data$c31c_18+data$c31d_18+data$c31e_18+data$c32_18+data$c41a_18+data$c41b_18+data$c41c_18+data$c51a_18+data$c51b_18+data$c52_18+data$c53a_18+data$c53b_18+data$c53c_18
write.csv(data,file = "尼玛，终于清洗完了.csv")

