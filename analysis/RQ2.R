#ALL <- read.csv("C:\\Users\\15531\\Desktop \\threshold\\predict\\result.csv",header=TRUE,)
library(tidyr)
library(ggplot2)
library(effsize)
#library(reaDR)
library(Cairo)



library(cowplot)


ALL_real<-ALL

ALL <- read.csv("average0.csv",header=TRUE,stringsAsFactors=FALSE)
#ALL <- read.csv("C:\\Users\\wu\\Desktop\\dataset\\result.csv",header=TRUE)
ALL$repository[ALL$repository=='aeeem']='AEEEM'
ALL$repository[ALL$repository=='MDP']='NASA'
ALL$repository[ALL$repository=='promise']='Promise'


ALL$threshold[ALL$threshold==0]='DV'
ALL$threshold[ALL$threshold==1]='BV'
ALL$threshold[ALL$threshold==2]='AV'
ALL$threshold[ALL$threshold==3]='GV'
ALL$threshold[ALL$threshold==4]='DR'
ALL$threshold[ALL$threshold==5]='BR'
ALL$threshold[ALL$threshold==6]='AR'
ALL$threshold[ALL$threshold==7]='GR'
ALL$threshold[ALL$threshold==8]='??F1'
ALL$threshold[ALL$threshold==9]='??MCC'
ALL$threshold[ALL$threshold==10]='??Gmn'
ALL$threshold[ALL$threshold==11]='??Gms'
ALL$threshold[ALL$threshold==12]='??D2h'


RQ2 <- ALL[(ALL$subsample=="ORIGIN"&ALL$ensemble=="ORIGIN"&(ALL$threshold=="??D2h"))|((ALL$subsample!="ORIGIN"|ALL$ensemble!="ORIGIN")&ALL$threshold=="DV"),]
RQ2<-RQ2[RQ2$repository!='SOFTLAB',]
RQ2<-RQ2[RQ2$classifier!='KNN',] 
RQ2_unite <-unite(RQ2,"X",subsample,ensemble,threshold)

RQ2_unite$X[RQ2_unite$X=='ORIGIN_ORIGIN_??D2h'] = '??D2h'
RQ2_unite$X[RQ2_unite$X=='ORIGIN_BAGGING_DV'] = 'Bag'
RQ2_unite$X[RQ2_unite$X=='ORIGIN_BOOSTING_DV'] = 'Bst'

RQ2_unite$X[RQ2_unite$X=='SMOTE_BAGGING_DV'] = 'SBag'
RQ2_unite$X[RQ2_unite$X=='SMOTE_BOOSTING_DV'] = 'SBst'
RQ2_unite$X[RQ2_unite$X=='SMOTE_ORIGIN_DV'] = 'Smote'

RQ2_unite$X[RQ2_unite$X=='UNDER-SAMPLING_BAGGING_DV'] = 'UBag'
RQ2_unite$X[RQ2_unite$X=='UNDER-SAMPLING_BOOSTING_DV'] = 'UBst'
RQ2_unite$X[RQ2_unite$X=='UNDER-SAMPLING_ORIGIN_DV'] = 'US'

RQ2_unite$X[RQ2_unite$X=='UNDER-OVER-SAMPLING_BAGGING_DV'] = 'UOBag'
RQ2_unite$X[RQ2_unite$X=='UNDER-OVER-SAMPLING_BOOSTING_DV'] = 'UOBst'
RQ2_unite$X[RQ2_unite$X=='UNDER-OVER-SAMPLING_ORIGIN_DV'] = 'UOS'

RQ2_unite$X[RQ2_unite$X=='OVER-SAMPLING_BAGGING_DV'] = 'OBag'
RQ2_unite$X[RQ2_unite$X=='OVER-SAMPLING_BOOSTING_DV'] = 'OBst'
RQ2_unite$X[RQ2_unite$X=='OVER-SAMPLING_ORIGIN_DV'] = 'OS'



for (a in unique(RQ2_unite$X)){
  baseline<-RQ2_unite[RQ2_unite$X=="??D2h",]
  base_gmeasure<-baseline$gmeasure
  comp_gmeasure<-RQ2_unite$gmeasure[RQ2_unite$X==a]
  
  if (length(base_gmeasure)>length(comp_gmeasure)){
    base_gmeasure<-base_gmeasure[1:length(comp_gmeasure)]
  }
  
  if (a == "??D2h"){
    PValue_gmeasure<-1
  }
  else{
    PValue_gmeasure<-wilcox.test(comp_gmeasure,base_gmeasure, paired=TRUE)$p.value
  }
  CliffDelta_gmeasure<-cliff.delta(comp_gmeasure,base_gmeasure)$estimate
  
  RQ2_unite$PValue_gmeasure[RQ2_unite$X==a]<-PValue_gmeasure
  RQ2_unite$CliffDelta_gmeasure[RQ2_unite$X==a]<-CliffDelta_gmeasure
  
  
  #gmeasure
  if(PValue_gmeasure<0.05&CliffDelta_gmeasure>=0){
    RQ2_unite$fill_p_gmeasure[RQ2_unite$X==a]="3"
  }
  else if(PValue_gmeasure<0.05&CliffDelta_gmeasure<0){
    RQ2_unite$fill_p_gmeasure[RQ2_unite$X==a]="1"
  }
  else{
    RQ2_unite$fill_p_gmeasure[RQ2_unite$X==a]="2"
  }
  
  
  if(abs(CliffDelta_gmeasure)<0.147){
    RQ2_unite$fill_c_gmeasure[RQ2_unite$X==a]="1"
  }
  else if(abs(CliffDelta_gmeasure)<0.33){
    RQ2_unite$fill_c_gmeasure[RQ2_unite$X==a]="2"
  }
  else if(abs(CliffDelta_gmeasure)<0.474){
    RQ2_unite$fill_c_gmeasure[RQ2_unite$X==a]="3"
  }
  else{
    RQ2_unite$fill_c_gmeasure[RQ2_unite$X==a]="4"
  }
}
result<-RQ2_unite[,-5][,-5][,-5][,-5][,-5][,-5]
colnames(result)[5] <- 'value'
result$no = "rank2"

bar<-NULL
bar<-rbind(bar,result)

bar$X = factor(bar$X, levels=c("??D2h","US","OS","UOS","Smote","Bag","Bst","UBag","OBag","UOBag","SBag","UBst","OBst","UOBst","SBst"))

bar_RQ2_gmeasure_rank2<-bar



#fig 5
bar5<-bar
p5<-ggplot(data=bar_RQ2_gmeasure_rank2, aes(x=X,y=value,fill=fill_c_gmeasure,colour=fill_p_gmeasure))+ylab("Gmeasure")+
  geom_boxplot()+ 
  stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 1.5, fill = "green")+ 
  theme(text = element_text(family = "serif"),legend.position = 'none',plot.margin=unit(c(1,3,1,1),'lines'),strip.text = element_text(size=15),panel.grid.major =element_blank(),panel.border=element_rect(fill='transparent', color='black'), panel.grid.minor = element_blank(),panel.background = element_blank(),panel.spacing =unit(c(1,3,1,1),'lines'),axis.line = element_line(colour = "black"),axis.title.x  = element_blank() ,axis.title.y  = element_text(size=20) ,axis.text.x = element_text(face="bold",size=18,angle=60,hjust = 1,vjust = 1), axis.text.y = element_text(face="bold",size=16))+
  
  scale_fill_manual(breaks = c("1", "2", "3", "4"),values=c( "#ffffff", "#E5E7E9", "#BDC3C7", "#797D7F"))+
  scale_color_manual(breaks = c("1", "2","3"),values=c("#ff0000","#000000", "#0011ff"))+ 
  scale_y_continuous()
  

show(p5)

theme(text = element_text(family = 'STHeiti'))




ggsave(p5, file='RQ2_Gmeasure_2.pdf', width=8, height=4) # ????ָ????С??????Ϊ12cm????Ϊ10cm????Ҫָ??????·??
warnings()
