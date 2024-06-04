library(tidyr)
library(ggplot2)
library(effsize)
library(stargazer)
library(RColorBrewer)


ALL <- read.csv("average4.csv",header=TRUE,)

ALL$gmeasure[is.na(ALL$gmeasure)]<-0
RQ1_table<-NULL
bar<-NULL
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

#RQ1$threshold = factor(RQ1$threshold, levels=c("DV","BV","AV","GV","DR","BR","AR","GR","??F1","??MCC","??Gmn","??Gms","??D2h"))
RQ1$threshold = factor(RQ1$threshold, levels=c("DV","BV","AV","GV","DR","BR","AR","GR","max_f1","max_mcc","max_gmean","max_gmeasure","min_d2h"))

RQ1<-RQ1[RQ1$classifier!='KNN',]


for (d in unique(RQ1$classifier)){
  base_f1<-RQ1$f1[RQ1$classifier==d&RQ1$threshold=="DV"]
  base_mcc<-RQ1$mcc[RQ1$classifier==d&RQ1$threshold=="DV"]
  base_recall<-RQ1$recall[RQ1$classifier==d&RQ1$threshold=="DV"]
  base_gmeasure<-RQ1$gmeasure[RQ1$classifier==d&RQ1$threshold=="DV"]
  base_gmean<-RQ1$gmean[RQ1$classifier==d&RQ1$threshold=="DV"]
  
  for (t in unique(RQ1$threshold)){
    f1<-mean(RQ1$f1[RQ1$classifier==d&RQ1$threshold==t])
    mcc<-mean(RQ1$mcc[RQ1$classifier==d&RQ1$threshold==t])
    recall<-mean(RQ1$recall[RQ1$classifier==d&RQ1$threshold==t])
    gmeasure<-mean(RQ1$gmeasure[RQ1$classifier==d&RQ1$threshold==t])
    gmean<-mean(RQ1$gmean[RQ1$classifier==d&RQ1$threshold==t])
    
    comp_f1<-RQ1$f1[RQ1$classifier==d&RQ1$threshold==t]
    comp_mcc<-RQ1$mcc[RQ1$classifier==d&RQ1$threshold==t]
    comp_recall<-RQ1$recall[RQ1$classifier==d&RQ1$threshold==t]
    comp_gmeasure<-RQ1$gmeasure[RQ1$classifier==d&RQ1$threshold==t]
    comp_gmean<-RQ1$gmean[RQ1$classifier==d&RQ1$threshold==t]
    
    f1_cd<-cliff.delta(comp_f1,base_f1)$estimate
    mcc_cd<-cliff.delta(comp_mcc,base_mcc)$estimate
    recall_cd<-cliff.delta(comp_recall,base_recall)$estimate
    gmeasure_cd<-cliff.delta(comp_gmeasure,base_gmeasure)$estimate
    gmean_cd<-cliff.delta(comp_gmean,base_gmean)$estimate
    if (t == "DV"){
      mcc_p<-1
      f1_p<-1
      recall_p<-1
      gmeasure_p<-1
      gmean_p<-1
    }
    else{
      f1_p<-wilcox.test(comp_f1,base_f1, paicyan=TRUE)$p.value
      mcc_p<-wilcox.test(comp_mcc,base_mcc, paicyan=TRUE)$p.value
      recall_p<-wilcox.test(comp_recall,base_recall, paicyan=TRUE)$p.value
      gmeasure_p<-wilcox.test(comp_gmeasure,base_gmeasure, paicyan=TRUE)$p.value
      gmean_p<-wilcox.test(comp_gmean,base_gmean, paicyan=TRUE)$p.value
    }
    RQ1_table<-rbind(RQ1_table,data.frame(classifier=d,threshold=t,f1=f1,f1_cd=f1_cd,f1_p=f1_p,mcc=mcc,mcc_cd=mcc_cd,mcc_p=mcc_p,recall=recall,recall_cd=recall_cd,recall_p=recall_p,gmeasure=gmeasure,gmeasure_cd=gmeasure_cd,gmeasure_p=gmeasure_p,gmean=gmean,gmean_cd=gmean_cd,gmean_p=gmean_p))
  }
}
RQ1_table$f1_p = p.adjust(RQ1_table$f1_p, method = "BH")   
RQ1_table$mcc_p = p.adjust(RQ1_table$mcc_p, method = "BH")  
RQ1_table$recall_p = p.adjust(RQ1_table$recall_p, method = "BH")  
RQ1_table$gmeasure_p = p.adjust(RQ1_table$gmeasure_p, method = "BH")  
RQ1_table$gmean_p = p.adjust(RQ1_table$gmean_p, method = "BH")  

print(base_f1)

bar<-NULL

for(c in unique(RQ1_table$classifier)){
  for(t in unique(RQ1_table$threshold)){
    if(t=="DV") next
    count<-0
    #recall
    if(RQ1_table$recall_cd[RQ1_table$threshold==t&RQ1_table$classifier==c]>0.147&RQ1_table$recall_p[RQ1_table$threshold==t&RQ1_table$classifier==c]<0.05){
      
      count<-count+1
      
    }else if(RQ1_table$recall_cd[RQ1_table$threshold==t&RQ1_table$classifier==c]<(-0.147)&RQ1_table$recall_p[RQ1_table$threshold==t&RQ1_table$classifier==c]<0.05){
      count<-count-1
    }
    
    #f1
    if(RQ1_table$f1_cd[RQ1_table$threshold==t&RQ1_table$classifier==c]>0.147&RQ1_table$f1_p[RQ1_table$threshold==t&RQ1_table$classifier==c]<0.05){
      
      count<-count+1
      
    }else if(RQ1_table$f1_cd[RQ1_table$threshold==t&RQ1_table$classifier==c]<(-0.147)&RQ1_table$f1_p[RQ1_table$threshold==t&RQ1_table$classifier==c]<0.05){
      count<-count-1
    }
    
    #mcc
    if(RQ1_table$mcc_cd[RQ1_table$threshold==t&RQ1_table$classifier==c]>0.147&RQ1_table$mcc_p[RQ1_table$threshold==t&RQ1_table$classifier==c]<0.05){
      
      count<-count+1
      
    }else if(RQ1_table$mcc_cd[RQ1_table$threshold==t&RQ1_table$classifier==c]<(-0.147)&RQ1_table$mcc_p[RQ1_table$threshold==t&RQ1_table$classifier==c]<0.05){
      count<-count-1
    }
    
    #gmean
    if(RQ1_table$gmean_cd[RQ1_table$threshold==t&RQ1_table$classifier==c]>0.147&RQ1_table$gmean_p[RQ1_table$threshold==t&RQ1_table$classifier==c]<0.05){
      
      count<-count+1
      
    }else if(RQ1_table$gmean_cd[RQ1_table$threshold==t&RQ1_table$classifier==c]<(-0.147)&RQ1_table$gmean_p[RQ1_table$threshold==t&RQ1_table$classifier==c]<0.05){
      count<-count-1
    }
    
    #gmeasure
    if(RQ1_table$gmeasure_cd[RQ1_table$threshold==t&RQ1_table$classifier==c]>0.147&RQ1_table$gmeasure_p[RQ1_table$threshold==t&RQ1_table$classifier==c]<0.05){
      
      count<-count+1
      
    }else if(RQ1_table$gmeasure_cd[RQ1_table$threshold==t&RQ1_table$classifier==c]<(-0.147)&RQ1_table$gmeasure_p[RQ1_table$threshold==t&RQ1_table$classifier==c]<0.05){
      count<-count-1
    }
    bar<-rbind(bar,data.frame(classifier=c,threshold=t,count=count))
    
  }
}

require(graphics)
require(grDevices)


color<-colorRampPalette((c("#3A75B1","#2E92C3","#60AED3","#99CAE0","#C3DBEE","#F7F7F7","#FFE8CA","#FFBB89","#FF8D61","#F7654E","#DF2F2A")))(11)

mycolor<-brewer.pal(11,"RdBu")
mycolor<- rev(mycolor)

#plotCol(mycolor)
#bar2<-bar
bar_RQ1_heatmap<-NULL
bar_RQ1_heatmap<-bar



bar_RQ1_heatmap$count= factor(bar_RQ1_heatmap$count, levels=c(-5,-4,-3,-2,-1,0,1,2,3,4,5))

p <- ggplot(bar_RQ1_heatmap, aes(x=threshold,y=classifier)) +
  xlab("Threshold") +ylab("Classifier")+  
  theme_classic() + 
  theme(panel.grid.major=element_blank()) + 
  theme(legend.key=element_blank())  +
  #theme(text = element_text(family = "serif"),axis.title  = element_text(size=20,face="bold"),axis.text.y = element_text(face = "bold",size=18),axis.text.x = element_text(face="bold",size=18,angle=60,hjust = 1,vjust = 1),panel.background = element_rect(fill="grey95")) + 
  theme(legend.position="right") +  
  geom_tile(aes(fill=factor(count))) +
  scale_fill_manual("Count",values=color,limits=factor(c(5,4,3,2,1,0,-1,-2,-3,-4,-5)),breaks=seq(5,-5,-1))+
  geom_text(aes(x=threshold,y=classifier,label=count),colour="black",size=5)
p







ggsave(p, file='RQ1_heatmap.pdf', width=8, height=6)

