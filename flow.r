##讀取流式細胞儀檔案
flow<-read.csv(file = "flow_1.csv", header = TRUE, sep = ",")
flowdata<-flow[,-1]

#製作圖二
#install.packages("GGally") #若未安裝套件記得先安裝
library(GGally)
pm<-ggpairs(flowdata,mapping = aes(color = flowdata$treatmemt))
pm

#依照施打藥物拆開
#install.packages("tidyverse") #若未安裝套件記得先安裝
library(tidyverse)
PBS_flow<-filter(flowdata,flow$treatmemt=="PBS")
PDL1_flow<-filter(flowdata,flow$treatmemt=="PDL1")
#######計算細胞平均數#######
ncols <- length(PDL1_flow)
column_means <- vector(mode = "numeric", length = ncols)
for (i in 1:ncols) {   
  column_means[i] <- mean(PDL1_flow[[i]])
}
names(column_means) <- names(PDL1_flow)
c<-column_means
#
ncols <- length(PBS_flow)
column_means <- vector(mode = "numeric", length = ncols)
for (i in 1:ncols) {   
  column_means[i] <- mean(PBS_flow[[i]])
}
names(column_means) <- names(PBS_flow)
column_means
##########################
#各項數值的wilcoxon test
wilcox.test(PBS_flow$Tc,PDL1_flow$Tc, paired =  FALSE)
wilcox.test(PBS_flow$NK,PDL1_flow$NK,paired = FALSE)
wilcox.test(PBS_flow$Neutrophil,PDL1_flow$Neutrophil, paired =  FALSE)
wilcox.test(PBS_flow$NKT,PDL1_flow$NKT, paired =  FALSE)
wilcox.test(PBS_flow$Th,PDL1_flow$Th, paired =  FALSE)
wilcox.test(PBS_flow$CD3,PDL1_flow$CD3, paired =  FALSE)
wilcox.test(PBS_flow$TcellInLiveCell,PDL1_flow$TcellInLiveCell, paired =  FALSE)
wilcox.test(PBS_flow$TcellInImmuneCell,PDL1_flow$TcellInImmuneCell, paired =  FALSE)
wilcox.test(PBS_flow$CD8OverCD3,PDL1_flow$CD8OverCD3, paired =  FALSE)
wilcox.test(PBS_flow$CD8InLiveCell,PDL1_flow$CD8InLiveCell, paired =  FALSE)
wilcox.test(PBS_flow$CD8InLiveCell,PDL1_flow$CD8InLiveCell, paired =  FALSE)
wilcox.test(PBS_flow$Neutrophil/(PBS_flow$Tc+PBS_flow$Th+PBS_flow$NKT),PDL1_flow$Neutrophil/((PDL1_flow$Tc+PDL1_flow$Th+PDL1_flow$NKT)), paired =  FALSE)
wilcox.test((PBS_flow$NK+PBS_flow$NKT)/PBS_flow$liveCell,(PDL1_flow$NK+PDL1_flow$NKT)/PDL1_flow$liveCell,paired = FALSE)
