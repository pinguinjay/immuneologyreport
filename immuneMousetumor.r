#  Null Hypothesis: The median difference between pairs of observations is zero
#  Alternate Hypothesis: The median difference between pairs of observations is not zero
#
#    Alpha = 0.05
#
#####################################################################
#
tumorSize <- read.csv(file = "immuneMousetumor.csv", header = TRUE, sep = ",")
print(tumorSize)
#
# Check for Normality
shapiro.test(tumorSize$antiPDL1TumorGrowthRate)     # p < 0.05, data are not normal 非常態
shapiro.test(tumorSize$PBSTumorGrowthRate)   # p > 0.05, data are normal 常態
#
# Perform Mann-Whitney Test using "wilcox.test()" function
wilcox.test(tumorSize$antiPDL1TumorGrowthRate,tumorSize$PBSTumorGrowthRate, paired =  FALSE)
#
# Repeat test and swap variables
wilcox.test(tumorSize$PBSTumorGrowthRate, tumorSize$antiPDL1TumorGrowthRate, paired = FALSE)
#
# Report: U = 23, p = 0.9015
# Decision: Fail to reject Null Hypothesis
# Conclusion: No difference in tumor groth rate between Males and Females
#
#結果
#	Wilcoxon rank sum exact test
#data:  tumorSize$antiPDL1TumorGrowthRate and tumorSize$PBSTumorGrowthRate
#W = 23, p-value = 0.9015
#alternative hypothesis: true location shift is not equal to 0
tumorSize2 <- read.csv(file = "immuneMousetumor2.csv", header = TRUE, sep = ",")
library(ggplot2)
ggplot(tumorSize2, aes(x = as.factor(drug), y = TumorGrowthRate,color = as.factor(drug))) +
  geom_boxplot(outlier.colour = "red", # 離群值標示顏色
               outlier.shape = 4,      # 離群值標示樣式
               outlier.size = 4) + # 箱形圖
  xlab("drug") +   # X 軸標示文字
  ylab("TumorGrowthRate")+# Y 軸標示文字
  coord_flip()+
  labs(color = "drug")+
  theme(legend.position = c(0.85, 0.75))+
  geom_dotplot(binaxis = 'y',       # 加入資料點
              stackdir = 'center', # 置中對齊
              dotsize = 0.3, )       # 資料點大小
