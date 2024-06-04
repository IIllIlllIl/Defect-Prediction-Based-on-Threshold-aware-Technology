require(graphics)
require(grDevices)

bar<-NULL

#ALL2 <- read.csv("../all.csv",header=TRUE,stringsAsFactors=FALSE)
ALL2 <- read.csv("average0.csv",header=TRUE,stringsAsFactors=FALSE)

ALL2$threshold[ALL2$threshold==0]='DV'
ALL2$threshold[ALL2$threshold==1]='BV'
ALL2$threshold[ALL2$threshold==2]='AV'
ALL2$threshold[ALL2$threshold==3]='GV'
ALL2$threshold[ALL2$threshold==4]='DR'
ALL2$threshold[ALL2$threshold==5]='BR'
ALL2$threshold[ALL2$threshold==6]='AR'
ALL2$threshold[ALL2$threshold==7]='GR'
ALL2$threshold[ALL2$threshold==8]='max_f1'
ALL2$threshold[ALL2$threshold==9]='max_mcc'
ALL2$threshold[ALL2$threshold==10]='max_gmean'
ALL2$threshold[ALL2$threshold==11]='max_gmeasure'
ALL2$threshold[ALL2$threshold==12]='??D2h'

RQ2 <- ALL2[(ALL2$subsample=="ORIGIN"&ALL2$ensemble=="ORIGIN" & ALL2$threshold=="??D2h")|((ALL2$subsample!="ORIGIN"|ALL2$ensemble!="ORIGIN")&ALL2$threshold=="DV"),]
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

#baseline<-RQ2[RQ2$threshold=="B",]
#base_gmeasure<-baseline$gmean
#base_gmean<-baseline$gmean



for (a in unique(RQ2_unite$X)){
  baseline<-RQ2_unite[RQ2_unite$X=="??D2h",]
  base_f1<-baseline$f1
  comp_f1<-RQ2_unite$f1[RQ2_unite$X==a]
  base_mcc<-baseline$mcc
  comp_mcc<-RQ2_unite$mcc[RQ2_unite$X==a]
  base_recall<-baseline$recall
  comp_recall<-RQ2_unite$recall[RQ2_unite$X==a]
  base_gmean<-baseline$gmean
  comp_gmean<-RQ2_unite$gmean[RQ2_unite$X==a]
  base_gmeasure<-baseline$gmeasure
  comp_gmeasure<-RQ2_unite$gmeasure[RQ2_unite$X==a]
  
  if (length(base_gmeasure)>length(comp_gmeasure)){
    base_gmeasure<-base_gmeasure[1:length(comp_gmeasure)]
  }
  
  if (a == "??D2h"){
    PValue_f1<-1
    PValue_mcc<-1
    PValue_recall<-1
    PValue_gmean<-1
    PValue_gmeasure<-1
  }
  else{
    PValue_f1<-wilcox.test(comp_f1,base_f1, paired=TRUE)$p.value
    PValue_mcc<-wilcox.test(comp_mcc,base_mcc, paired=TRUE)$p.value
    PValue_recall<-wilcox.test(comp_recall,base_recall, paired=TRUE)$p.value
    PValue_gmean<-wilcox.test(comp_gmean,base_gmean, paired=TRUE)$p.value
    PValue_gmeasure<-wilcox.test(comp_gmeasure,base_gmeasure, paired=TRUE)$p.value
  }
  CliffDelta_f1<-cliff.delta(comp_f1,base_f1)$estimate
  CliffDelta_mcc<-cliff.delta(comp_mcc,base_mcc)$estimate
  CliffDelta_recall<-cliff.delta(comp_recall,base_recall)$estimate
  CliffDelta_gmean<-cliff.delta(comp_gmean,base_gmean)$estimate
  CliffDelta_gmeasure<-cliff.delta(comp_gmeasure,base_gmeasure)$estimate
  
  RQ2_unite$PValue_f1[RQ2_unite$X==a]<-PValue_f1
  RQ2_unite$CliffDelta_f1[RQ2_unite$X==a]<-CliffDelta_f1
  
  RQ2_unite$PValue_mcc[RQ2_unite$X==a]<-PValue_mcc
  RQ2_unite$CliffDelta_mcc[RQ2_unite$X==a]<-CliffDelta_mcc
  
  RQ2_unite$PValue_recall[RQ2_unite$X==a]<-PValue_recall
  RQ2_unite$CliffDelta_recall[RQ2_unite$X==a]<-CliffDelta_recall
  
  RQ2_unite$PValue_gmean[RQ2_unite$X==a]<-PValue_gmean
  RQ2_unite$CliffDelta_gmean[RQ2_unite$X==a]<-CliffDelta_gmean
  
  RQ2_unite$PValue_gmeasure[RQ2_unite$X==a]<-PValue_gmeasure
  RQ2_unite$CliffDelta_gmeasure[RQ2_unite$X==a]<-CliffDelta_gmeasure
  
  count<-0

  #f1
  if(PValue_f1<0.05& CliffDelta_f1>0.147){
    count<-count-1;
  }
  else if(PValue_f1<0.05&CliffDelta_f1<(-0.147)){
    count<-count+1;
  }
#  if(a!="f1"){
#    bar<-rbind(bar,data.frame(classifier=a,threshold="f1",count=count))
#  }
  
  #mcc
  if(PValue_mcc<0.05& CliffDelta_mcc>0.147){
    count<-count-1;
  }
  else if(PValue_mcc<0.05&CliffDelta_mcc<(-0.147)){
    count<-count+1;
  }
  
  
  #recall
  if(PValue_recall<0.05& CliffDelta_recall>0.147){
    count<-count-1;
  }
  else if(PValue_recall<0.05&CliffDelta_recall<(-0.147)){
    count<-count+1;
  }
  
  #gmean
  if(PValue_gmean<0.05& CliffDelta_gmean>0.147){
    count<-count-1;
  }
  else if(PValue_gmean<0.05&CliffDelta_gmean<(-0.147)){
    count<-count+1;
  }
  #gmeasure
  if(PValue_gmeasure<0.05& CliffDelta_gmeasure>0.147){
    count<-count-1;
  }
  else if(PValue_gmeasure<0.05&CliffDelta_gmeasure<(-0.147)){
    count<-count+1;
  }
  if(a!="??D2h"){
    bar<-rbind(bar,data.frame(classifier=a,threshold="??D2h",count=count))
  }
}



#bar2<-bar
bar_RQ2_heatmap<-bar

bar_RQ2_heatmap$count= factor(bar_RQ2_heatmap$count, levels=c(-5,-4,-3,-2,-1,0,1,2,3,4,5))
bar_RQ2_heatmap$threshold = factor(bar_RQ2_heatmap$threshold, levels=c("BV","AV","GV","DR","BR","AR","GR","??F1","??MCC","??Gmn","??Gms","??D2h"))
bar_RQ2_heatmap$classifier = factor(bar_RQ2_heatmap$classifier, levels=c("Bag","Bst","Smote","SBag","SBst","OS","OBag","OBst","US","UBag","UBst","UOS","UOBag","UOBst"))


p <- ggplot(bar_RQ2_heatmap, aes(x=threshold,y=classifier)) +
  xlab("Threshold") +ylab("Imbalance")+  
  theme_classic() + 
  theme(panel.grid.major=element_blank()) + 
  theme(legend.key=element_blank())  +
  #theme(text = element_text(family = "serif"),axis.title  = element_text(size=20,face="bold"),axis.text.y = element_text(face = "bold",size=18),axis.text.x = element_text(face="bold",size=18,angle=60,hjust = 1,vjust = 1),panel.background = element_rect(fill="grey95")) + 
  theme(legend.position="right") +  
  geom_tile(aes(fill=factor(count))) +
  scale_fill_manual("Count",values=color,limits=factor(c(-5,-4,-3,-2,-1,0,1,2,3,4,5)),breaks=seq(5,-5,-1))+
  geom_text(aes(x=threshold,y=classifier,label=count),colour="black",size=5)
p

ggsave(p, file='RQ2_heatmap.pdf', width=8, height=6)

