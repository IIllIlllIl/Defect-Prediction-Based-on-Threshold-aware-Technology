library(tidyr)
library(ggplot2)
library(effsize)
library(stargazer)

ALL <- read.csv("average4.csv",header=TRUE,)

ALL$gmeasure[is.na(ALL$gmeasure)]<-0
RQ1_table<-NULL
RQ1<-ALL[ALL$ensemble=='ORIGIN'&ALL$subsample=='ORIGIN',]
RQ1<-RQ1[RQ1$repository!='SOFTLAB',]

RQ1$threshold[RQ1$threshold==0]='DV'
RQ1$threshold[RQ1$threshold==1]='BV'
RQ1$threshold[RQ1$threshold==2]='AV'
RQ1$threshold[RQ1$threshold==3]='GV'
RQ1$threshold[RQ1$threshold==4]='DR'
RQ1$threshold[RQ1$threshold==5]='BR'
RQ1$threshold[RQ1$threshold==6]='AR'
RQ1$threshold[RQ1$threshold==7]='GR'
RQ1$threshold[RQ1$threshold==8]='max_f1'
RQ1$threshold[RQ1$threshold==9]='max_mcc'
RQ1$threshold[RQ1$threshold==10]='max_gmean'
RQ1$threshold[RQ1$threshold==11]='max_gmeasure'
RQ1$threshold[RQ1$threshold==12]='min_d2h'

#RQ1$threshold = factor(RQ1$threshold, levels=c("DV","BV","AV","GV","DR","BR","AR","GR"))
RQ1$threshold = factor(RQ1$threshold, levels=c("DV","BV","AV","GV","DR","BR","AR","GR","max_f1","max_mcc","max_gmean","max_gmeasure","min_d2h"))
RQ1<-RQ1[RQ1$classifier!='KNN',]