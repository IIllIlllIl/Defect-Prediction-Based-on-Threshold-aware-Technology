library(tidyr)
library(ggplot2)
library(effsize)
library(readr)

#ALL <- read.csv("C:\\Users\\wu\\Desktop\\dataset\\result.csv",header=TRUE)
ALL <- read.csv("average0.csv",header=TRUE,)
ALL$gmeasure[is.na(ALL$gmeasure)]<-0
RQ1<-ALL[ALL$ensemble=='ORIGIN'&ALL$subsample=='ORIGIN'& ALL$repository=='camel-2.10.0',]
#RQ1<-ALL[ALL$ensemble=='ORIGIN'&ALL$subsample=='ORIGIN',]
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

RQ1<-RQ1[RQ1$repository!='SOFTLAB',]
RQ1<-RQ1[RQ1$classifier!='KNN',]

for (r in unique(RQ1$dataset)){
  CA<-RQ1$mcc[RQ1$threshold=="DV"&RQ1$dataset==r]
  CB<-RQ1$mcc[RQ1$threshold=="BV"&RQ1$dataset==r]
  CC<-RQ1$mcc[RQ1$threshold=="AV"&RQ1$dataset==r]
  CD<-RQ1$mcc[RQ1$threshold=="GV"&RQ1$dataset==r]
  CE<-RQ1$mcc[RQ1$threshold=="DR"&RQ1$dataset==r]
  CF<-RQ1$mcc[RQ1$threshold=="BR"&RQ1$dataset==r]
  CG<-RQ1$mcc[RQ1$threshold=="AR"&RQ1$dataset==r]
  CH<-RQ1$mcc[RQ1$threshold=="GR"&RQ1$dataset==r]
  CI<-RQ1$mcc[RQ1$threshold=="max_f1"&RQ1$dataset==r]
  CJ<-RQ1$mcc[RQ1$threshold=="max_mcc"&RQ1$dataset==r]
  CK<-RQ1$mcc[RQ1$threshold=="max_gmean"&RQ1$dataset==r]
  CL<-RQ1$mcc[RQ1$threshold=="max_gmeasure"&RQ1$dataset==r]
  CM<-RQ1$mcc[RQ1$threshold=="min_d2h"&RQ1$dataset==r]
  
  
  RQ1$PValue[RQ1$threshold=="DV"&RQ1$dataset==r]<-1
  RQ1$CliffDelta[RQ1$threshold=="DV"&RQ1$dataset==r]<-cliff.delta(CA,CA)$estimate
  RQ1$PValue[RQ1$threshold=="BV"&RQ1$dataset==r]<-wilcox.test(CB,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="BV"&RQ1$dataset==r]<-cliff.delta(CB,CA)$estimate
  RQ1$PValue[RQ1$threshold=="AV"&RQ1$dataset==r]<-wilcox.test(CC,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="AV"&RQ1$dataset==r]<-cliff.delta(CC,CA)$estimate
  RQ1$PValue[RQ1$threshold=="GV"&RQ1$dataset==r]<-wilcox.test(CD,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="GV"&RQ1$dataset==r]<-cliff.delta(CD,CA)$estimate
  RQ1$PValue[RQ1$threshold=="DR"&RQ1$dataset==r]<-wilcox.test(CE,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="DR"&RQ1$dataset==r]<-cliff.delta(CE,CA)$estimate
  RQ1$PValue[RQ1$threshold=="BR"&RQ1$dataset==r]<-wilcox.test(CF,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="BR"&RQ1$dataset==r]<-cliff.delta(CF,CA)$estimate
  RQ1$PValue[RQ1$threshold=="AR"&RQ1$dataset==r]<-wilcox.test(CG,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="AR"&RQ1$dataset==r]<-cliff.delta(CG,CA)$estimate
  RQ1$PValue[RQ1$threshold=="GR"&RQ1$dataset==r]<-wilcox.test(CH,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="GR"&RQ1$dataset==r]<-cliff.delta(CH,CA)$estimate
  RQ1$PValue[RQ1$threshold=="max_f1"&RQ1$dataset==r]<-wilcox.test(CI,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="max_f1"&RQ1$dataset==r]<-cliff.delta(CI,CA)$estimate
  RQ1$PValue[RQ1$threshold=="max_mcc"&RQ1$dataset==r]<-wilcox.test(CJ,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="max_mcc"&RQ1$dataset==r]<-cliff.delta(CJ,CA)$estimate
  RQ1$PValue[RQ1$threshold=="max_gmean"&RQ1$dataset==r]<-wilcox.test(CK,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="max_gmean"&RQ1$dataset==r]<-cliff.delta(CK,CA)$estimate
  RQ1$PValue[RQ1$threshold=="max_gmeasure"&RQ1$dataset==r]<-wilcox.test(CL,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="max_gmeasure"&RQ1$dataset==r]<-cliff.delta(CL,CA)$estimate
  RQ1$PValue[RQ1$threshold=="min_d2h"&RQ1$dataset==r]<-wilcox.test(CM,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="min_d2h"&RQ1$dataset==r]<-cliff.delta(CM,CA)$estimate
}

for(i in(1:length(RQ1$dataset))){
  if(RQ1$PValue[i]<0.05&RQ1$CliffDelta[i]>=0){
    RQ1$fill_p[i]="3"
  }
  else if(RQ1$PValue[i]<0.05&RQ1$CliffDelta[i]<0){
    RQ1$fill_p[i]="1"
  }
  else{
    RQ1$fill_p[i]="2"
  }
  
  
  if(abs(RQ1$CliffDelta[i])<0.147){
    RQ1$fill_c[i]="1"
  }
  else if(abs(RQ1$CliffDelta[i])<0.33){
    RQ1$fill_c[i]="2"
  }
  else if(abs(RQ1$CliffDelta[i])<0.474){
    RQ1$fill_c[i]="3"
  }
  else{
    RQ1$fill_c[i]="4"
  }
}

RQ1_2<-RQ1



RQ1<-RQ1_2
RQ1<-RQ1[RQ1$threshold=="BR" | RQ1$threshold=="DV",]
RQ1<-data.frame(RQ1)

RQ1$threshold = factor(RQ1$threshold, levels=c("DV","BV","AV","GV","DR","BR","AR","GR","max_f1","max_mcc","max_gmean","max_gmeasure","min_d2h"))
#p<-ggplot(data=RQ1, aes(x=threshold,y=gmeasure,fill=fill_c,colour=fill_p))+geom_boxplot()+ stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "green")+ theme(legend.position = 'none',strip.text = element_text(size=15),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),axis.title  = element_blank() ,axis.text.x = element_text(face="bold",size=16) )+ scale_fill_manual(breaks = c("1", "2", "3", "4"),values=c( "#ffffff", "#00fff9", "#00ffc5", "#00ff85"))+scale_color_manual(breaks = c("1", "2","3"),values=c( "#ff0000","#000000","#0011ff"))+ scale_y_continuous(limits = c(-0.05,0.6))+ facet_wrap(~ dataset, scales="free")

p<-ggplot(data=RQ1, aes(x=dataset,y=mcc,fill=fill_c,colour=fill_p))+labs(title = "mcc")+geom_boxplot()+ stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 1, fill = "green")+ theme(legend.position = 'none',plot.margin=unit(c(1,3,1,1),'lines'),plot.title=element_text(size=20,hjust=0.5),strip.text = element_text(size=15),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),axis.title  = element_blank() ,axis.text.x = element_text(face="bold",size=2,angle=-60,hjust = 0,vjust = 1))+ scale_fill_manual(breaks = c("1", "2", "3", "4"),values=c( "#ffffff", "#E5E7E9", "#BDC3C7", "#797D7F"))+scale_color_manual(breaks = c("1", "2","3"),values=c("#ff0000","#000000", "#0011ff"))+ scale_y_continuous(limits = c(-0.2,0.7))+ facet_wrap(~ threshold, scales="free")

p

jpeg(file = "final\\BR.jpg",width =20*500,height = 20*500,units = "px",res =20*72) #结果保存
print(p)
dev.off()
#ALL_average$gmeasure[ALL_average$dataset=='JURECZKO-pbeans2'&ALL_average$subsample=='ORIGIN'&ALL_average$ensemble=='ORIGIN']

#multiplot(p1, p2, layout = layout)
