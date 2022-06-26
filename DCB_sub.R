install.packages('tidyverse')
install.packages('factoextra')


library(dplyr)
library(tidyverse)
library(factoextra)

raw_data <- read.csv('../DCB.csv')
raw_tb <- as_tibble(raw_data)
raw_tb <- subset(raw_tb, select=-c(NO,MRN,LENO, NAME, WT, HT, DATE, FUDR, DAPTDR, CAGDR,
                                   PREMLD, PRERD, POSTMLD, POSTRD, PRETIMIC, POSTTIMIC, FURD, FUMLD,
                                   FULENG, FUDS, FUTIMI, FUTIMIC))
raw_tb$GEN <- factor(raw_tb$GEN, levels=c('M','F'), labels=c('M','F'))
raw_tb$CDX <- factor(raw_tb$CDX, levels=c('SMI','SAP','UAP','NSTEMI','STEMI'),
                     labels=c('SMI','SAP','UAP','NSTEMI','STEMI'))

raw_tb[,5:12] <- lapply(raw_tb[,5:12], as.factor) 
raw_tb[,14:21] <- lapply(raw_tb[,14:21], as.factor) 

raw_tb$TYLE <- factor(raw_tb$TYLE, levels=c(1,2), labels=c(1,2))
raw_tb$LE_C <- factor(raw_tb$LE_C, levels=c('SV','MV'), labels=c('SV', 'MV'))
raw_tb$TIMI <- factor(raw_tb$TIMI, levels=c(0,1,2,3), labels=c(0,1,2,3))
raw_tb$POSTTIMI <- factor(raw_tb$POSTTIMI, levels=c(0,1,2,3), labels=c(0,1,2,3))

bd_srt <- sort(unique(raw_tb$PREBD))
raw_tb$PREBD <- factor(raw_tb$PREBD, levels=bd_srt, labels=bd_srt)

raw_tb$LECL <- factor(raw_tb$LECL, levels=c('B1','B2','C'), labels=c('B1','B2','C'))

tbl <- raw_tb
write.csv(tbl, './dcb_tibble.csv')

#============== PCA - data preprocessing ============================
tbl <- read.csv('./dcb_tibble.csv')

dcb_tbl <- tbl[,27:43] #procedural factor들만 추출 - PCA 위해서... 

dcb_tbl[,1] <- case_when(dcb_tbl[,1]=='Ostium'~1, 
                         dcb_tbl[,1]=='Bifurcation'~2,
                         dcb_tbl[,1]=='CTO'~3, TRUE~4) #1 - ostium, 2-bifurcation, 3-CTO, 4-small
                            
dcb_tbl$LE_C <- factor(dcb_tbl$LE_C, labels=c(0,1))
dcb_tbl$LE_C <- as.integer(dcb_tbl$LE_C) #SV = 1, MV = 2

dcb_tbl$TYLE <- as.integer(dcb_tbl$TYLE)
dcb_tbl[,c('TIMI','POSTTIMI','PREBD','LECL')] <- as.numeric(dcb_tbl[,c('TIMI','POSTTIMI','PREBD','LECL')])

dcb_tbl$LECL <- case_when(dcb_tbl$LECL=='B1'~1,
                          dcb_tbl$LECL=='B2'~2,
                          dcb_tbl$LECL=='C'~3)

sub_tbl <- na.omit(dcb_tbl)

#================= PCA ========================
dcb_model <- prcomp(sub_tbl, center=T, scale=T) 
#summary(dcb_model) # sd...  
#print(dcb_model) #변수별 기저(혹은 계수)

screeplot(dcb_model, type='lines', npcs=length(dcb_model$sdev))
biplot(dcb_model)

#================== K-mean clustering ==================
set.seed(1004)
dcb_kmean <- kmeans(dcb_model$rotation, centers=2)
fviz_cluster(dcb_kmean, dcb_model$rotation, ellipse.type='norm')+theme_minimal()

set.seed(1005)
dcb_kmean <- kmeans(dcb_model$rotation, centers=3)
fviz_cluster(dcb_kmean, dcb_model$rotation, ellipse.type='norm')+theme_minimal()


set.seed(1006)
dcb_kmean <- kmeans(dcb_model$rotation, centers=4)
fviz_cluster(dcb_kmean, dcb_model$rotation, ellipse.type='norm')+theme_minimal()
