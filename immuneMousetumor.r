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
