ALL<-read.csv("average4.csv",header=TRUE)

# RQ2 <- ALL[((ALL$subsample!="ORIGIN"|ALL$ensemble!="ORIGIN")),]
RQ2 <- ALL
RQ2<-RQ2[RQ2$repository!='SOFTLAB',]
RQ2<-RQ2[RQ2$classifier!='KNN',]
RQ2_unite <-unite(RQ2,"X",subsample,ensemble)

RQ2_unite$X[RQ2_unite$X=='ORIGIN_ORIGIN'] = 'ori'

RQ2_unite$X[RQ2_unite$X=='ORIGIN_BAGGING'] = 'Bag'
RQ2_unite$X[RQ2_unite$X=='ORIGIN_BOOSTING'] = 'Bst'

RQ2_unite$X[RQ2_unite$X=='SMOTE_BAGGING'] = 'SBag'
RQ2_unite$X[RQ2_unite$X=='SMOTE_BOOSTING'] = 'SBst'
RQ2_unite$X[RQ2_unite$X=='SMOTE_ORIGIN'] = 'Smote'

RQ2_unite$X[RQ2_unite$X=='UNDER-SAMPLING_BAGGING'] = 'UBag'
RQ2_unite$X[RQ2_unite$X=='UNDER-SAMPLING_BOOSTING'] = 'UBst'
RQ2_unite$X[RQ2_unite$X=='UNDER-SAMPLING_ORIGIN'] = 'US'

RQ2_unite$X[RQ2_unite$X=='UNDER-OVER-SAMPLING_BAGGING'] = 'UOBag'
RQ2_unite$X[RQ2_unite$X=='UNDER-OVER-SAMPLING_BOOSTING'] = 'UOBst'
RQ2_unite$X[RQ2_unite$X=='UNDER-OVER-SAMPLING_ORIGIN'] = 'UOS'

RQ2_unite$X[RQ2_unite$X=='OVER-SAMPLING_BAGGING'] = 'OBag'
RQ2_unite$X[RQ2_unite$X=='OVER-SAMPLING_BOOSTING'] = 'OBst'
RQ2_unite$X[RQ2_unite$X=='OVER-SAMPLING_ORIGIN'] = 'OS'

bar_RQ2_1<-NULL

RQ2_unite$threshold[RQ2_unite$threshold==0]='DV'
RQ2_unite$threshold[RQ2_unite$threshold==1]='BV'
RQ2_unite$threshold[RQ2_unite$threshold==2]='AV'
RQ2_unite$threshold[RQ2_unite$threshold==3]='GV'
RQ2_unite$threshold[RQ2_unite$threshold==4]='DR'
RQ2_unite$threshold[RQ2_unite$threshold==5]='BR'
RQ2_unite$threshold[RQ2_unite$threshold==6]='AR'
RQ2_unite$threshold[RQ2_unite$threshold==7]='GR'
RQ2_unite$threshold[RQ2_unite$threshold==8]='max_f1'
RQ2_unite$threshold[RQ2_unite$threshold==9]='max_mcc'
RQ2_unite$threshold[RQ2_unite$threshold==10]='max_gmean'
RQ2_unite$threshold[RQ2_unite$threshold==11]='max_gmeasure'
RQ2_unite$threshold[RQ2_unite$threshold==12]='min_d2h'

# f1
for (x in unique(RQ2_unite$X)){
  if (x!='ori') {
    win<-0
    tie<-0
    loss<-0
    base<-RQ2_unite$f1[RQ2_unite$X==x&RQ2_unite$threshold=='DV']
    for (t in unique(RQ2_unite$threshold)){
      if (t!='DV'){
        comp<-RQ2_unite$f1[RQ2_unite$X=='ori'&RQ2_unite$threshold==t]
        
        #p<-wilcox.test(comp,base, paired=TRUE)$p.value
        p<-wilcox.test(comp,base, paired=TRUE)$p.value
        
        cd<-cliff.delta(comp,base)$estimate
        if (cd>0.147&p<0.05) {
          win<-win+1
        }
        else if(cd<(-0.147)&p<0.05) {
          loss<-loss+1
        }
        else{
          tie<-tie+1
        }
      }
    }
    bar_RQ2_1<-rbind(bar_RQ2_1,data.frame(X=x, status="Win", number=win,indicator="F1"))
    bar_RQ2_1<-rbind(bar_RQ2_1,data.frame(X=x, status="Tie", number=tie,indicator="F1"))
    bar_RQ2_1<-rbind(bar_RQ2_1,data.frame(X=x, status="Loss", number=loss,indicator="F1"))
  }
}

bar_RQ2_1<-bar_RQ2_1[bar_RQ2_1$number!=0,]
bar_RQ2_1$indicator= factor(bar_RQ2_1$indicator, levels=c("Recall","F1","MCC","Gmean","Gmeasure"))
bar_RQ2_1$X = factor(bar_RQ2_1$X, levels=c("US","OS","UOS","Smote","Bag","Bst","UBag","OBag","UOBag","SBag","UBst","OBst","UOBst","SBst"))
bar_RQ2_1<-bar_RQ2_1[bar_RQ2_1$number!=0,]
ggplot(data = bar_RQ2_1, mapping = aes(x = X, y = number, fill = status)) +labs(title = "RQ2. f1")+ geom_bar(stat = 'identity',width=0.5, position = 'stack') + geom_text(aes(label=number),position=position_stack(0.5), color="white", size=3.5)+ theme(strip.text = element_text(size=15),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),axis.title  = element_blank() ,axis.text.y = element_text(face = "bold",size=16),axis.text.x = element_text(face="bold",size=16,angle=60,hjust = 1,vjust = 1))+scale_fill_manual(values=c("#0011ff","#7B7B7B","#ff0000"))

p<- ggplot(data = bar_RQ2_1, mapping = aes(x = number, y = X, fill = status))+geom_bar(stat = 'identity',width=0.5, position = 'stack') +geom_text(aes(label=number),position=position_stack(0.5), color="white", size=5) +scale_fill_manual(values=c("#0011ff","#7B7B7B","#ff0000"))+scale_x_continuous(limits = c(0,12),breaks = seq(0,12,2))+facet_grid(.~indicator, scales="free")+guides(fill=FALSE)
# theme(plot.title=element_text(face="bold",hjust=0.5),strip.text = element_text(face="bold",size=20),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),legend.background=element_blank(),axis.line = element_line(colour = "black"),axis.title  = element_blank() ,axis.text.y = element_text(face = "bold",size=18),text = element_text(family = "serif"),axis.text.x = element_text(face="bold",size=14))

p 

ggsave(p, file='RQ2_f1.pdf', width=14, height=8) # ????ָ????С??????Ϊ12cm????Ϊ10cm????Ҫָ??????·??

# recall
for (x in unique(RQ2_unite$X)){
  if (x!='ori') {
    win<-0
    tie<-0
    loss<-0
    base<-RQ2_unite$recall[RQ2_unite$X==x&RQ2_unite$threshold=='DV']
    for (t in unique(RQ2_unite$threshold)){
      if (t!='DV'){
        comp<-RQ2_unite$recall[RQ2_unite$X=='ori'&RQ2_unite$threshold==t]
        
        #p<-wilcox.test(comp,base, paired=TRUE)$p.value
        p<-wilcox.test(comp,base, paired=TRUE)$p.value
        
        cd<-cliff.delta(comp,base)$estimate
        if (cd>0.147&p<0.05) {
          win<-win+1
        }
        else if(cd<(-0.147)&p<0.05) {
          loss<-loss+1
        }
        else{
          tie<-tie+1
        }
      }
    }
    bar_RQ2_1<-rbind(bar_RQ2_1,data.frame(X=x, status="Win", number=win,indicator="Recall"))
    bar_RQ2_1<-rbind(bar_RQ2_1,data.frame(X=x, status="Tie", number=tie,indicator="Recall"))
    bar_RQ2_1<-rbind(bar_RQ2_1,data.frame(X=x, status="Loss", number=loss,indicator="Recall"))
  }
}

bar_RQ2_1<-bar_RQ2_1[bar_RQ2_1$number!=0,]
bar_RQ2_1$indicator= factor(bar_RQ2_1$indicator, levels=c("Recall","F1","MCC","Gmean","Gmeasure"))
bar_RQ2_1$X = factor(bar_RQ2_1$X, levels=c("US","OS","UOS","Smote","Bag","Bst","UBag","OBag","UOBag","SBag","UBst","OBst","UOBst","SBst"))
bar_RQ2_1<-bar_RQ2_1[bar_RQ2_1$number!=0,]
ggplot(data = bar_RQ2_1, mapping = aes(x = X, y = number, fill = status)) +labs(title = "RQ2. recall")+ geom_bar(stat = 'identity',width=0.5, position = 'stack') + geom_text(aes(label=number),position=position_stack(0.5), color="white", size=3.5)+ theme(strip.text = element_text(size=15),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),axis.title  = element_blank() ,axis.text.y = element_text(face = "bold",size=16),axis.text.x = element_text(face="bold",size=16,angle=60,hjust = 1,vjust = 1))+scale_fill_manual(values=c("#0011ff","#7B7B7B","#ff0000"))

p<- ggplot(data = bar_RQ2_1, mapping = aes(x = number, y = X, fill = status))+geom_bar(stat = 'identity',width=0.5, position = 'stack') +geom_text(aes(label=number),position=position_stack(0.5), color="white", size=5) +scale_fill_manual(values=c("#0011ff","#7B7B7B","#ff0000"))+scale_x_continuous(limits = c(0,12),breaks = seq(0,12,2))+facet_grid(.~indicator, scales="free")+guides(fill=FALSE)
# theme(plot.title=element_text(face="bold",hjust=0.5),strip.text = element_text(face="bold",size=20),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),legend.background=element_blank(),axis.line = element_line(colour = "black"),axis.title  = element_blank() ,axis.text.y = element_text(face = "bold",size=18),text = element_text(family = "serif"),axis.text.x = element_text(face="bold",size=14))

p 

ggsave(p, file='RQ2_recall.pdf', width=14, height=8) # ????ָ????С??????Ϊ12cm????Ϊ10cm????Ҫָ??????·??

# mcc
for (x in unique(RQ2_unite$X)){
  if (x!='ori') {
    win<-0
    tie<-0
    loss<-0
    base<-RQ2_unite$mcc[RQ2_unite$X==x&RQ2_unite$threshold=='DV']
    for (t in unique(RQ2_unite$threshold)){
      if (t!='DV'){
        comp<-RQ2_unite$mcc[RQ2_unite$X=='ori'&RQ2_unite$threshold==t]
        
        #p<-wilcox.test(comp,base, paired=TRUE)$p.value
        p<-wilcox.test(comp,base, paired=TRUE)$p.value
        
        cd<-cliff.delta(comp,base)$estimate
        if (cd>0.147&p<0.05) {
          win<-win+1
        }
        else if(cd<(-0.147)&p<0.05) {
          loss<-loss+1
        }
        else{
          tie<-tie+1
        }
      }
    }
    bar_RQ2_1<-rbind(bar_RQ2_1,data.frame(X=x, status="Win", number=win,indicator="MCC"))
    bar_RQ2_1<-rbind(bar_RQ2_1,data.frame(X=x, status="Tie", number=tie,indicator="MCC"))
    bar_RQ2_1<-rbind(bar_RQ2_1,data.frame(X=x, status="Loss", number=loss,indicator="MCC"))
  }
}

bar_RQ2_1<-bar_RQ2_1[bar_RQ2_1$number!=0,]
bar_RQ2_1$indicator= factor(bar_RQ2_1$indicator, levels=c("Recall","F1","MCC","Gmean","Gmeasure"))
bar_RQ2_1$X = factor(bar_RQ2_1$X, levels=c("US","OS","UOS","Smote","Bag","Bst","UBag","OBag","UOBag","SBag","UBst","OBst","UOBst","SBst"))
bar_RQ2_1<-bar_RQ2_1[bar_RQ2_1$number!=0,]
ggplot(data = bar_RQ2_1, mapping = aes(x = X, y = number, fill = status)) +labs(title = "RQ2. mcc")+ geom_bar(stat = 'identity',width=0.5, position = 'stack') + geom_text(aes(label=number),position=position_stack(0.5), color="white", size=3.5)+ theme(strip.text = element_text(size=15),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),axis.title  = element_blank() ,axis.text.y = element_text(face = "bold",size=16),axis.text.x = element_text(face="bold",size=16,angle=60,hjust = 1,vjust = 1))+scale_fill_manual(values=c("#0011ff","#7B7B7B","#ff0000"))

p<- ggplot(data = bar_RQ2_1, mapping = aes(x = number, y = X, fill = status))+geom_bar(stat = 'identity',width=0.5, position = 'stack') +geom_text(aes(label=number),position=position_stack(0.5), color="white", size=5) +scale_fill_manual(values=c("#0011ff","#7B7B7B","#ff0000"))+scale_x_continuous(limits = c(0,12),breaks = seq(0,12,2))+facet_grid(.~indicator, scales="free")+guides(fill=FALSE)
# theme(plot.title=element_text(face="bold",hjust=0.5),strip.text = element_text(face="bold",size=20),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),legend.background=element_blank(),axis.line = element_line(colour = "black"),axis.title  = element_blank() ,axis.text.y = element_text(face = "bold",size=18),text = element_text(family = "serif"),axis.text.x = element_text(face="bold",size=14))

p 

ggsave(p, file='RQ2_mcc.pdf', width=14, height=8) # ????ָ????С??????Ϊ12cm????Ϊ10cm????Ҫָ??????·??

# gmean
for (x in unique(RQ2_unite$X)){
  if (x!='ori') {
    win<-0
    tie<-0
    loss<-0
    base<-RQ2_unite$gmean[RQ2_unite$X=='ori'&RQ2_unite$threshold=='DV']
    for (t in unique(RQ2_unite$threshold)){
      if (t!='DV'){
        comp<-RQ2_unite$gmean[RQ2_unite$X==x&RQ2_unite$threshold==t]
        
        #p<-wilcox.test(comp,base, paired=TRUE)$p.value
        p<-wilcox.test(comp,base, paired=TRUE)$p.value
        
        cd<-cliff.delta(comp,base)$estimate
        if (cd>0.147&p<0.05) {
          win<-win+1
        }
        else if(cd<(-0.147)&p<0.05) {
          loss<-loss+1
        }
        else{
          tie<-tie+1
        }
      }
    }
    bar_RQ2_1<-rbind(bar_RQ2_1,data.frame(X=x, status="Win", number=win,indicator="Gmean"))
    bar_RQ2_1<-rbind(bar_RQ2_1,data.frame(X=x, status="Tie", number=tie,indicator="Gmean"))
    bar_RQ2_1<-rbind(bar_RQ2_1,data.frame(X=x, status="Loss", number=loss,indicator="Gmean"))
  }
}

bar_RQ2_1<-bar_RQ2_1[bar_RQ2_1$number!=0,]
bar_RQ2_1$indicator= factor(bar_RQ2_1$indicator, levels=c("Recall","F1","MCC","Gmean","Gmeasure"))
bar_RQ2_1$X = factor(bar_RQ2_1$X, levels=c("US","OS","UOS","Smote","Bag","Bst","UBag","OBag","UOBag","SBag","UBst","OBst","UOBst","SBst"))
bar_RQ2_1<-bar_RQ2_1[bar_RQ2_1$number!=0,]
ggplot(data = bar_RQ2_1, mapping = aes(x = X, y = number, fill = status)) +labs(title = "RQ2. gmean")+ geom_bar(stat = 'identity',width=0.5, position = 'stack') + geom_text(aes(label=number),position=position_stack(0.5), color="white", size=3.5)+ theme(strip.text = element_text(size=15),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),axis.title  = element_blank() ,axis.text.y = element_text(face = "bold",size=16),axis.text.x = element_text(face="bold",size=16,angle=60,hjust = 1,vjust = 1))+scale_fill_manual(values=c("#0011ff","#7B7B7B","#ff0000"))

p<- ggplot(data = bar_RQ2_1, mapping = aes(x = number, y = X, fill = status))+geom_bar(stat = 'identity',width=0.5, position = 'stack') +geom_text(aes(label=number),position=position_stack(0.5), color="white", size=5) +scale_fill_manual(values=c("#0011ff","#7B7B7B","#ff0000"))+scale_x_continuous(limits = c(0,12),breaks = seq(0,12,2))+facet_grid(.~indicator, scales="free")+guides(fill=FALSE)
# theme(plot.title=element_text(face="bold",hjust=0.5),strip.text = element_text(face="bold",size=20),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),legend.background=element_blank(),axis.line = element_line(colour = "black"),axis.title  = element_blank() ,axis.text.y = element_text(face = "bold",size=18),text = element_text(family = "serif"),axis.text.x = element_text(face="bold",size=14))

p 

ggsave(p, file='RQ2_gmean.pdf', width=14, height=8) # ????ָ????С??????Ϊ12cm????Ϊ10cm????Ҫָ??????·??


# gmeasure
for (x in unique(RQ2_unite$X)){
  if (x!= 'ori') {
    win<-0
    tie<-0
    loss<-0
    base<-RQ2_unite$gmeasure[RQ2_unite$X==x&RQ2_unite$threshold=='DV']
    for (t in unique(RQ2_unite$threshold)){
      if (t!='DV'){
        comp<-RQ2_unite$gmeasure[RQ2_unite$X=='ori'&RQ2_unite$threshold==t]
        
        #p<-wilcox.test(comp,base, paired=TRUE)$p.value
        p<-wilcox.test(comp,base, paired=TRUE)$p.value
        
        cd<-cliff.delta(comp,base)$estimate
        if (cd>0.147&p<0.05) {
          win<-win+1
        }
        else if(cd<(-0.147)&p<0.05) {
          loss<-loss+1
        }
        else{
          tie<-tie+1
        }
      }
    }
    bar_RQ2_1<-rbind(bar_RQ2_1,data.frame(X=x, status="Win", number=win,indicator="Gmeasure"))
    bar_RQ2_1<-rbind(bar_RQ2_1,data.frame(X=x, status="Tie", number=tie,indicator="Gmeasure"))
    bar_RQ2_1<-rbind(bar_RQ2_1,data.frame(X=x, status="Loss", number=loss,indicator="Gmeasure"))
  }
}

bar_RQ2_1<-bar_RQ2_1[bar_RQ2_1$number!=0,]
bar_RQ2_1$indicator= factor(bar_RQ2_1$indicator, levels=c("Recall","F1","MCC","Gmean","Gmeasure"))
bar_RQ2_1$X = factor(bar_RQ2_1$X, levels=c("US","OS","UOS","Smote","Bag","Bst","UBag","OBag","UOBag","SBag","UBst","OBst","UOBst","SBst"))
bar_RQ2_1<-bar_RQ2_1[bar_RQ2_1$number!=0,]
ggplot(data = bar_RQ2_1, mapping = aes(x = X, y = number, fill = status)) +labs(title = "RQ2. gmeasure")+ geom_bar(stat = 'identity',width=0.5, position = 'stack') + geom_text(aes(label=number),position=position_stack(0.5), color="white", size=3.5)+ theme(strip.text = element_text(size=15),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),axis.title  = element_blank() ,axis.text.y = element_text(face = "bold",size=16),axis.text.x = element_text(face="bold",size=16,angle=60,hjust = 1,vjust = 1))+scale_fill_manual(values=c("#0011ff","#7B7B7B","#ff0000"))

p<- ggplot(data = bar_RQ2_1, mapping = aes(x = number, y = X, fill = status))+geom_bar(stat = 'identity',width=0.5, position = 'stack') +geom_text(aes(label=number),position=position_stack(0.5), color="white", size=5) +scale_fill_manual(values=c("#ff0000","#7B7B7B","#0011ff"))+scale_x_continuous(limits = c(0,12),breaks = seq(0,12,2))+facet_grid(.~indicator, scales="free")+guides(fill=FALSE)
# theme(plot.title=element_text(face="bold",hjust=0.5),strip.text = element_text(face="bold",size=20),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),legend.background=element_blank(),axis.line = element_line(colour = "black"),axis.title  = element_blank() ,axis.text.y = element_text(face = "bold",size=18),text = element_text(family = "serif"),axis.text.x = element_text(face="bold",size=14))

p 

ggsave(p, file='RQ2_4.pdf', width=14, height=8) # ????ָ????С??????Ϊ12cm????Ϊ10cm????Ҫָ??????·??