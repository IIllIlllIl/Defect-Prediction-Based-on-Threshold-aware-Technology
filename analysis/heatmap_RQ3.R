ALL <- read.csv("average4.csv",header=TRUE,)
RQ3 <- ALL[((ALL$subsample!="ORIGIN"|ALL$ensemble!="ORIGIN")),]
RQ3<-RQ3[RQ3$repository!='SOFTLAB',]



RQ3<-RQ3[RQ3$classifier!='KNN',]
RQ3<-RQ3[RQ3$dataset!='ar3',]
RQ3<-RQ3[RQ3$dataset!='ar4',]
RQ3_unite <-unite(RQ3,"X",subsample,ensemble)

RQ3_unite$X[RQ3_unite$X=='ORIGIN_BAGGING'] = 'Bag'
RQ3_unite$X[RQ3_unite$X=='ORIGIN_BOOSTING'] = 'Bst'

RQ3_unite$X[RQ3_unite$X=='SMOTE_BAGGING'] = 'SBag'
RQ3_unite$X[RQ3_unite$X=='SMOTE_BOOSTING'] = 'SBst'
RQ3_unite$X[RQ3_unite$X=='SMOTE_ORIGIN'] = 'Smote'

RQ3_unite$X[RQ3_unite$X=='UNDER-SAMPLING_BAGGING'] = 'UBag'
RQ3_unite$X[RQ3_unite$X=='UNDER-SAMPLING_BOOSTING'] = 'UBst'
RQ3_unite$X[RQ3_unite$X=='UNDER-SAMPLING_ORIGIN'] = 'US'

RQ3_unite$X[RQ3_unite$X=='UNDER-OVER-SAMPLING_BAGGING'] = 'UOBag'
RQ3_unite$X[RQ3_unite$X=='UNDER-OVER-SAMPLING_BOOSTING'] = 'UOBst'
RQ3_unite$X[RQ3_unite$X=='UNDER-OVER-SAMPLING_ORIGIN'] = 'UOS'

RQ3_unite$X[RQ3_unite$X=='OVER-SAMPLING_BAGGING'] = 'OBag'
RQ3_unite$X[RQ3_unite$X=='OVER-SAMPLING_BOOSTING'] = 'OBst'
RQ3_unite$X[RQ3_unite$X=='OVER-SAMPLING_ORIGIN'] = 'OS'

RQ3_f<-RQ3_unite
RQ3_f$classifier='ALL'
RQ3_unite<-rbind(RQ3_f,RQ3_unite)


bar<-NULL

RQ3_unite$threshold[RQ3_unite$threshold==0]='DV'
RQ3_unite$threshold[RQ3_unite$threshold==1]='BV'
RQ3_unite$threshold[RQ3_unite$threshold==2]='AV'
RQ3_unite$threshold[RQ3_unite$threshold==3]='GV'
RQ3_unite$threshold[RQ3_unite$threshold==4]='DR'
RQ3_unite$threshold[RQ3_unite$threshold==5]='BR'
RQ3_unite$threshold[RQ3_unite$threshold==6]='AR'
RQ3_unite$threshold[RQ3_unite$threshold==7]='GR'
RQ3_unite$threshold[RQ3_unite$threshold==8]='max_f1'
RQ3_unite$threshold[RQ3_unite$threshold==9]='max_mcc'
RQ3_unite$threshold[RQ3_unite$threshold==10]='max_gmean'
RQ3_unite$threshold[RQ3_unite$threshold==11]='max_gmeasure'
RQ3_unite$threshold[RQ3_unite$threshold==12]='min_d2h'


for (c in unique(RQ3_unite$classifier)){
  for(x in unique(RQ3_unite$X)){
    baseline <- RQ3_unite[RQ3_unite$threshold=="DV"&RQ3_unite$X==x&RQ3_unite$classifier==c,]
    comp<-RQ3_unite[RQ3_unite$threshold!="DV"&RQ3_unite$X==x&RQ3_unite$classifier==c,]
    base_f1 <- baseline$f1
    base_mcc <- baseline$mcc
    base_recall <- baseline$recall
    base_gmean<-baseline$gmean
    base_gmeasure<-baseline$gmeasure
    
    for(t in unique(comp$threshold)){
      print(t)
      comp_f1<-comp$f1[comp$threshold==t]
      comp_mcc<-comp$mcc[comp$threshold==t]
      comp_recall<-comp$recall[comp$threshold==t]
      comp_gmean<-comp$gmean[comp$threshold==t]
      comp_gmeasure<-comp$gmeasure[comp$threshold==t]

      PValue_f1<-wilcox.test(comp_f1,base_f1, paired=TRUE)$p.value
      PValue_mcc<-wilcox.test(comp_mcc,base_mcc, paired=TRUE)$p.value
      PValue_recall<-wilcox.test(comp_recall,base_recall, paired=TRUE)$p.value
      PValue_gmean<-wilcox.test(comp_gmean,base_gmean, paired=TRUE)$p.value
      PValue_gmeasure<-wilcox.test(comp_gmeasure,base_gmeasure, paired=TRUE)$p.value


      CliffDelta_f1<-cliff.delta(comp_f1,base_f1)$estimate
      CliffDelta_mcc<-cliff.delta(comp_mcc,base_mcc)$estimate
      CliffDelta_recall<-cliff.delta(comp_recall,base_recall)$estimate
      CliffDelta_gmean<-cliff.delta(comp_gmean,base_gmean)$estimate
      CliffDelta_gmeasure<-cliff.delta(comp_gmeasure,base_gmeasure)$estimate

      count<-0

      #gmeasure
      if(PValue_f1<0.05& CliffDelta_f1>0.147){
        count<-count+1;
      }
      else if(PValue_f1<0.05&CliffDelta_f1<(-0.147)){
        count<-count-1;
      }


      #gmeasure
      if(PValue_mcc<0.05& CliffDelta_mcc>0.147){
        count<-count+1;
      }
      else if(PValue_mcc<0.05&CliffDelta_mcc<(-0.147)){
        count<-count-1;
      }


      #gmeasure
      if(PValue_recall<0.05& CliffDelta_recall>0.147){
        count<-count+1;
      }
      else if(PValue_recall<0.05&CliffDelta_recall<(-0.147)){
        count<-count-1;
      }

      #gmeasure
      if(PValue_gmean<0.05& CliffDelta_gmean>0.147){
        count<-count+1;
      }
      else if(PValue_gmean<0.05&CliffDelta_gmean<(-0.147)){
        count<-count-1;
      }
      #gmeasure
      if(PValue_gmeasure<0.05& CliffDelta_gmeasure>0.147){
        count<-count+1;
      }
      else if(PValue_gmeasure<0.05&CliffDelta_gmeasure<(-0.147)){
        count<-count-1;
      }

      bar<-rbind(bar,data.frame(classifier=c,threshold=t,X=x,count=count))


    }
  }
}
bar<-bar[bar$classifier=="ALL",]


bar_RQ3_heatmap<-bar
bar_RQ3_heatmap$count= factor(bar_RQ3_heatmap$count, levels=c(-5,-4,-3,-2,-1,0,1,2,3,4,5))
bar_RQ3_heatmap$threshold = factor(bar_RQ3_heatmap$threshold, levels=c("BV","AV","GV","DR","BR","AR","GR","max_f1","max_mcc","max_gmean","max_gmeasure","min_d2h"))
bar_RQ3_heatmap$classifier = factor(bar_RQ3_heatmap$classifier, levels=c("Bag","Bst","Smote","SBag","SBst","OS","OBag","OBst","US","UBag","UBst","UOS","UOBag","UOBst"))

p <- ggplot(bar_RQ3_heatmap, aes(x=threshold,y=X)) +
  xlab("Threshold") +ylab("Imbalance")+  
  theme_classic() + 
  theme(panel.grid.major=element_blank()) + 
  theme(legend.key=element_blank())  +
  #theme(text = element_text(family = "serif"),axis.title  = element_text(size=20,face="bold"),axis.text.y = element_text(face = "bold",size=18),axis.text.x = element_text(face="bold",size=18,angle=60,hjust = 1,vjust = 1),panel.background = element_rect(fill="grey95")) + 
  theme(legend.position="right") +  
  geom_tile(aes(fill=factor(count))) +
  scale_fill_manual("Count",values=color,limits=factor(c(-5,-4,-3,-2,-1,0,1,2,3,4,5)),breaks=seq(5,-5,-1))+
  geom_text(aes(x=threshold,y=X,label=count),colour="black",size=5)
p


ggsave(p, file='RQ3_heatmap.pdf', width=8, height=6)
