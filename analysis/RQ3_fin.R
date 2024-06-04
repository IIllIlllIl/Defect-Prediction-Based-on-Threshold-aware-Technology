ALL <- read.csv("../all0.csv",header=TRUE,stringsAsFactors=FALSE)

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

RQ3_unite2<-unite(RQ3_unite,"X",X,threshold)

bar<-NULL

win<-0
tie<-0
loss<-0
for (r in unique(RQ3_unite2$dataset)){
  pd<-RQ3_unite2[RQ3_unite2$dataset==r,]
  pd$X <- with(pd, reorder(pd$X,1-gmeasure,mean))
  comp_x<-NULL
  base_x<-NULL
  for (x in levels(pd$X)){
    test_vr = strsplit(x,"_")[[1]][length(strsplit(x,"_")[[1]])]
    if (strsplit(x,"_")[[1]][length(strsplit(x,"_")[[1]])]!='DV'){
      base_x<-append(base_x,x)
    }
    if (strsplit(x,"_")[[1]][length(strsplit(x,"_")[[1]])]=='DV'){
      comp_x<-append(comp_x,x)
    }
    print(test_vr)
  }

  comp<-pd$gmeasure[pd$X==comp_x[1]]
  base<-pd$gmeasure[pd$X==base_x[1]]
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

bar<-rbind(bar,data.frame(x="T1 VS. S1", status="Win", number=loss,indicator="gmeasure"))
bar<-rbind(bar,data.frame(x="T1 VS. S1", status="Tie", number=tie,indicator="gmeasure"))
bar<-rbind(bar,data.frame(x="T1 VS. S1", status="Loss", number=win,indicator="gmeasure"))

win<-0
tie<-0
loss<-0
for (r in unique(RQ3_unite2$dataset)){
  pd<-RQ3_unite2[RQ3_unite2$dataset==r,]
  pd$X <- with(pd, reorder(pd$X,1-gmeasure,mean))
  comp_x<-NULL
  base_x<-NULL
  for (x in levels(pd$X)){
    if (strsplit(x,"_")[[1]][length(strsplit(x,"_")[[1]])]!='DV'){
      base_x<-append(base_x,x)
    }
    if (strsplit(x,"_")[[1]][length(strsplit(x,"_")[[1]])]=='DV'){
      comp_x<-append(comp_x,x)
    }
  }
  
  comp<-pd$gmeasure[pd$X==comp_x[1]]
  base<-pd$gmeasure[pd$X==base_x[2]]
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

bar<-rbind(bar,data.frame(x="T2 VS. S1", status="Win", number=loss,indicator="gmeasure"))
bar<-rbind(bar,data.frame(x="T2 VS. S1", status="Tie", number=tie,indicator="gmeasure"))
bar<-rbind(bar,data.frame(x="T2 VS. S1", status="Loss", number=win,indicator="gmeasure"))

win<-0
tie<-0
loss<-0
for (r in unique(RQ3_unite2$dataset)){
  pd<-RQ3_unite2[RQ3_unite2$dataset==r,]
  pd$X <- with(pd, reorder(pd$X,1-gmeasure,mean))
  comp_x<-NULL
  base_x<-NULL
  for (x in levels(pd$X)){
    if (strsplit(x,"_")[[1]][length(strsplit(x,"_")[[1]])]!='DV'){
      base_x<-append(base_x,x)
    }
    if (strsplit(x,"_")[[1]][length(strsplit(x,"_")[[1]])]=='DV'){
      comp_x<-append(comp_x,x)
    }
  }
  
  comp<-pd$gmeasure[pd$X==comp_x[2]]
  base<-pd$gmeasure[pd$X==base_x[1]]
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

bar<-rbind(bar,data.frame(x="T1 VS. S2", status="Win", number=loss,indicator="gmeasure"))
bar<-rbind(bar,data.frame(x="T1 VS. S2", status="Tie", number=tie,indicator="gmeasure"))
bar<-rbind(bar,data.frame(x="T1 VS. S2", status="Loss", number=win,indicator="gmeasure"))

win<-0
tie<-0
loss<-0
for (r in unique(RQ3_unite2$dataset)){
  pd<-RQ3_unite2[RQ3_unite2$dataset==r,]
  pd$X <- with(pd, reorder(pd$X,1-gmeasure,mean))
  comp_x<-NULL
  base_x<-NULL
  for (x in levels(pd$X)){
    if (strsplit(x,"_")[[1]][length(strsplit(x,"_")[[1]])]!='DV'){
      base_x<-append(base_x,x)
    }
    if (strsplit(x,"_")[[1]][length(strsplit(x,"_")[[1]])]=='DV'){
      comp_x<-append(comp_x,x)
    }
  }
  
  comp<-pd$gmeasure[pd$X==comp_x[2]]
  base<-pd$gmeasure[pd$X==base_x[2]]
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

bar<-rbind(bar,data.frame(x="T2 VS. S2", status="Win", number=loss,indicator="gmeasure"))
bar<-rbind(bar,data.frame(x="T2 VS. S2", status="Tie", number=tie,indicator="gmeasure"))
bar<-rbind(bar,data.frame(x="T2 VS. S2", status="Loss", number=win,indicator="gmeasure"))

bar_dataset<-bar[bar$number!=0,]
bar_dataset$indicator[bar_dataset$indicator=='f1']<-'f1'
bar_dataset$indicator[bar_dataset$indicator=='mcc']<-'mcc'
bar_dataset$indicator[bar_dataset$indicator=='recall']<-'recall'
bar_dataset$indicator[bar_dataset$indicator=='gmean']<-'gmean'
bar_dataset$indicator[bar_dataset$indicator=='gmeasure']<-'gmeasure'
bar_dataset$x= factor(bar_dataset$x, levels=c("T1 VS. S1","T1 VS. S2","T2 VS. S1","T2 VS. S2"))
bar_dataset$indicator= factor(bar_dataset$indicator, levels=c("f1","mcc","recall","gmean","gmeasure"))

ggplot(data = bar_dataset, mapping = aes(x = x, y = number, fill = status)) + geom_bar(stat = 'identity',width=0.5, position = 'stack') + geom_text(aes(label=number),position=position_stack(0.5), color="white", size=3.5) + facet_wrap(~ indicator, nrow=1) + theme(strip.text = element_text(size=15),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),axis.title  = element_blank() ,axis.text.y = element_text(face = "bold",size=16),axis.text.x = element_text(face="bold",size=16,angle=60,hjust = 1,vjust = 1))+scale_fill_manual(values=c("#0011ff","#7B7B7B","#ff0000"))

p<-ggplot(data = bar_dataset, mapping = aes(x = x, y = number, fill = status))+labs(title = "RQ3. dataset Win/Tie/Loss") + geom_bar(stat = 'identity',width=0.5, position = 'stack') + geom_text(aes(label=number),position=position_stack(0.5), color="white", size=3.5) + facet_wrap(~ indicator, nrow=1) + theme(plot.title=element_text(size=20,hjust=0.5),strip.text = element_text(size=15),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),axis.title  = element_blank() ,axis.text.y = element_text(face = "bold",size=16),axis.text.x = element_text(face="bold",size=16,angle=60,hjust = 1,vjust = 1))+scale_fill_manual(values=c("#0011ff","#7B7B7B","#ff0000"))

p

jpeg(file = "RQ3_dataset.jpg",width =3000,height = 2200,units = "px",res =300) #结果保存
print(p)
dev.off()