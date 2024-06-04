library(tidyr)
library(ggplot2)
library(effsize)

#ALL <- read.csv("../all.csv",header=TRUE,)
ALL <- read.csv("average0.csv",header=TRUE,)
ALL$gmeasure[is.na(ALL$gmeasure)]<-0
RQ1<-ALL[ALL$ensemble=='ORIGIN'&ALL$subsample=='ORIGIN',]
RQ1<-RQ1[RQ1$repository!='SOFTLAB',]
RQ1<-RQ1[RQ1$classifier!='KNN',]

base<-RQ1$gmeasure[RQ1$threshold==0]
for (t in unique(RQ1$threshold)){
  comp<-RQ1$gmeasure[RQ1$threshold==t]
  if (t == 0){
    RQ1$PValue[RQ1$threshold==t]<-1
    PValue<-1
  }
  else{
    RQ1$PValue[RQ1$threshold==t]<-wilcox.test(comp,base, paired=TRUE)$p.value
    PValue<-wilcox.test(comp,base, paired=TRUE)$p.value
  }
  RQ1$CliffDelta[RQ1$threshold==t]<-cliff.delta(comp,base)$estimate
  CliffDelta<-cliff.delta(comp,base)$estimate
  if(PValue<0.05&CliffDelta>0){
    RQ1$fill_p[RQ1$threshold==t]="3"
  }
  else if(PValue<0.05&CliffDelta<0){
    RQ1$fill_p[RQ1$threshold==t]="1"
  }
  else{
    RQ1$fill_p[RQ1$threshold==t]="2"
  }
  if(abs(CliffDelta)<0.147){
    RQ1$fill_c[RQ1$threshold==t]="1"
  }
  else if(abs(CliffDelta)<0.33){
    RQ1$fill_c[RQ1$threshold==t]="2"
  }
  else if(abs(CliffDelta)<0.474){
    RQ1$fill_c[RQ1$threshold==t]="3"
  }
  else{
    RQ1$fill_c[RQ1$threshold==t]="4"
  }
}
RQ1<-data.frame(RQ1)

RQ1$threshold = factor(RQ1$threshold, levels=c(0,1,2,3,4,5,6,7,8,9,10,11,12))

p<-ggplot(data=RQ1, aes(x=threshold,y=gmeasure,fill=fill_c,colour=fill_p))+geom_boxplot()+ stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "green")+ theme(legend.position = 'none',strip.text = element_text(size=15),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),axis.title  = element_blank() ,axis.text.x = element_text(face="bold",size=16),axis.text.y = element_text(face="bold",size=16) )+ scale_fill_manual(breaks = c("1", "2", "3", "4"),values=c( "#ffffff", "#E5E7E9", "#BDC3C7", "#797D7F"))+scale_color_manual(breaks = c("1", "2","3"),values=c( "#000000","#0011ff","#ff0000"))+ scale_y_continuous(limits = c(0,1)) 
show(p)
