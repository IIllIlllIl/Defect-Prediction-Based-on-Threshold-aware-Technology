
library(tidyr)
library(ggplot2)
library(effsize)
library(readr)

#ALL2 <- read.csv("C:\\Users\\15531\\Desktop\\threshold\\IMB_8threshold_AVERAGE(CLASSIFIER_without_KNN)_20200308.csv",header=TRUE)
#ALL2 <- read.csv("C:\\Users\\15531\\Desktop\\threshold\\ALL_AVERAGE_20200308.csv",header=TRUE)
#ALL2 <-read.csv("C:\\Users\\15531\\Desktop\\threshold\\ALL_20200307.csv",header=TRUE)
#ALL <- read.csv("C:\\Users\\15531\\Desktop\\threshold\\R\\threshold\\average.csv",header=TRUE)
#ALL2 <- read.csv("average.csv",header=TRUE,stringsAsFactors=FALSE)
ALL2 <- read.csv("../all4.csv",header=TRUE,stringsAsFactors=FALSE)

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
ALL2$threshold[ALL2$threshold==12]='min_d2h'


ALL<-ALL2
#ALL$ensemble <- ALL$ensample

#RQ2 <- ALL2[(ALL2$subsample=="ORIGIN"&ALL2$ensemble=="ORIGIN")|((ALL2$subsample!="ORIGIN"|ALL2$ensemble!="ORIGIN")&ALL2$threshold=="DV"),]
RQ2 <- ALL[(ALL$subsample=="ORIGIN"&ALL$ensemble=="ORIGIN")|((ALL$subsample!="ORIGIN"|ALL$ensemble!="ORIGIN")&ALL$threshold=="DV"),]

RQ2<-RQ2[RQ2$classifier!='KNN',]
RQ2_unite <-unite(RQ2,"X",subsample,ensemble,threshold)


#classifier <- data.frame(levels(unique(RQ2$classifier)))


bar2_classifier<-NULL

win<-0
tie<-0
loss<-0
for (r in unique(RQ2_unite$classifier)){
  # print(r)
  pd<-RQ2_unite[RQ2_unite$classifier==r,]
  pd$X <- with(pd, reorder(pd$X,1-f1,mean))
  
  
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
   a = data.frame(base_x)
  b = data.frame(comp_x)
  comp<-pd$f1[pd$X==comp_x[1]]
  base<-pd$f1[pd$X==base_x[1]]
  # print(comp_x)
  # print(base_x)
  
  p<-wilcox.test(comp,base, paired=TRUE)$p.value
  cd<-cliff.delta(comp,base)$estimate
  # print(p)
  # print(cd)
  
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
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S1", status="Win", number=loss,indicator="F1"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S1", status="Tie", number=tie,indicator="F1"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S1", status="Loss", number=win,indicator="F1"))

win<-0
tie<-0
loss<-0
for (r in unique(RQ2_unite$classifier)){
  pd<-RQ2_unite[RQ2_unite$classifier==r,]
  pd$X <- with(pd, reorder(pd$X,1-f1,mean))
  a<-pd$X
  a<-data.frame(a)
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
  comp<-pd$f1[pd$X==comp_x[2]]
  base<-pd$f1[pd$X==base_x[1]]
  p<-wilcox.test(comp,base, paired=TRUE)$p.value
  cd<-cliff.delta(comp,base)$estimate
  print(p)
  
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
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S2", status="Win", number=loss,indicator="F1"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S2", status="Tie", number=tie,indicator="F1"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S2", status="Loss", number=win,indicator="F1"))



win<-0
tie<-0
loss<-0
for (r in unique(RQ2_unite$classifier)){
  pd<-RQ2_unite[RQ2_unite$classifier==r,]
  pd$X <- with(pd, reorder(pd$X,1-f1,mean))
  a<-pd$X
  a<-data.frame(a)
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
  comp<-pd$f1[pd$X==comp_x[1]]
  base<-pd$f1[pd$X==base_x[2]]
  p<-wilcox.test(comp,base, paired=TRUE)$p.value
  cd<-cliff.delta(comp,base)$estimate
  print(p)
  
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
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S1", status="Win", number=loss,indicator="F1"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S1", status="Tie", number=tie,indicator="F1"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S1", status="Loss", number=win,indicator="F1"))


win<-0
tie<-0
loss<-0
for (r in unique(RQ2_unite$classifier)){
  pd<-RQ2_unite[RQ2_unite$classifier==r,]
  pd$X <- with(pd, reorder(pd$X,1-f1,mean))
  a<-pd$X
  a<-data.frame(a)
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
  comp<-pd$f1[pd$X==comp_x[2]]
  base<-pd$f1[pd$X==base_x[2]]
  p<-wilcox.test(comp,base, paired=TRUE)$p.value
  cd<-cliff.delta(comp,base)$estimate
  print(p)
  
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
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S2", status="Win", number=loss,indicator="F1"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S2", status="Tie", number=tie,indicator="F1"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S2", status="Loss", number=win,indicator="F1"))


win<-0
tie<-0
loss<-0
for (r in unique(RQ2_unite$classifier)){
  # print(r)
  pd<-RQ2_unite[RQ2_unite$classifier==r,]
  pd$X <- with(pd, reorder(pd$X,1-recall,mean))
  
  
  comp_x<-NULL
  base_x<-NULL
  viewer<-levels(pd$X)
  for (x in levels(pd$X)){
    if (strsplit(x,"_")[[1]][length(strsplit(x,"_")[[1]])]!='DV'){
      base_x<-append(base_x,x)
    }
    if (strsplit(x,"_")[[1]][length(strsplit(x,"_")[[1]])]=='DV'){
      comp_x<-append(comp_x,x)
    }
  }
  a = data.frame(base_x)
  b = data.frame(comp_x)
  comp<-pd$recall[pd$X==comp_x[1]]
  base<-pd$recall[pd$X==base_x[1]]
  # print(comp_x)
  # print(base_x)
  # viewer<-base_x
  
  p<-wilcox.test(comp,base, paired=TRUE)$p.value
  cd<-cliff.delta(comp,base)$estimate
  # print(p)
  # print(cd)
  
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
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S1", status="Win", number=loss,indicator="Recall"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S1", status="Tie", number=tie,indicator="Recall"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S1", status="Loss", number=win,indicator="Recall"))

win<-0
tie<-0
loss<-0
for (r in unique(RQ2_unite$classifier)){
  pd<-RQ2_unite[RQ2_unite$classifier==r,]
  pd$X <- with(pd, reorder(pd$X,1-recall,mean))
  a<-pd$X
  a<-data.frame(a)
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
  comp<-pd$recall[pd$X==comp_x[2]]
  base<-pd$recall[pd$X==base_x[1]]
  p<-wilcox.test(comp,base, paired=TRUE)$p.value
  cd<-cliff.delta(comp,base)$estimate
  #print(p)
  
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
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S2", status="Win", number=loss,indicator="Recall"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S2", status="Tie", number=tie,indicator="Recall"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S2", status="Loss", number=win,indicator="Recall"))



win<-0
tie<-0
loss<-0
for (r in unique(RQ2_unite$classifier)){
  pd<-RQ2_unite[RQ2_unite$classifier==r,]
  pd$X <- with(pd, reorder(pd$X,1-recall,mean))
  a<-pd$X
  a<-data.frame(a)
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
  comp<-pd$recall[pd$X==comp_x[1]]
  base<-pd$recall[pd$X==base_x[2]]
  p<-wilcox.test(comp,base, paired=TRUE)$p.value
  cd<-cliff.delta(comp,base)$estimate
  #print(p)
  
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
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S1", status="Win", number=loss,indicator="Recall"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S1", status="Tie", number=tie,indicator="Recall"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S1", status="Loss", number=win,indicator="Recall"))


win<-0
tie<-0
loss<-0
for (r in unique(RQ2_unite$classifier)){
  pd<-RQ2_unite[RQ2_unite$classifier==r,]
  pd$X <- with(pd, reorder(pd$X,1-recall,mean))
  a<-pd$X
  a<-data.frame(a)
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
  comp<-pd$recall[pd$X==comp_x[2]]
  base<-pd$recall[pd$X==base_x[2]]
  p<-wilcox.test(comp,base, paired=TRUE)$p.value
  cd<-cliff.delta(comp,base)$estimate
  #print(p)
  
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
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S2", status="Win", number=loss,indicator="Recall"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S2", status="Tie", number=tie,indicator="Recall"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S2", status="Loss", number=win,indicator="Recall"))


win<-0
tie<-0
loss<-0
for (r in unique(RQ2_unite$classifier)){
  #print(r)
  pd<-RQ2_unite[RQ2_unite$classifier==r,]
  pd$X <- with(pd, reorder(pd$X,1-mcc,mean))
  
  
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
  a = data.frame(base_x)
  b = data.frame(comp_x)
  comp<-pd$mcc[pd$X==comp_x[1]]
  base<-pd$mcc[pd$X==base_x[1]]
  #print(comp_x)
  #print(base_x)
  
  p<-wilcox.test(comp,base, paired=TRUE)$p.value
  cd<-cliff.delta(comp,base)$estimate
  #print(p)
  #print(cd)
  
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
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S1", status="Win", number=loss,indicator="MCC"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S1", status="Tie", number=tie,indicator="MCC"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S1", status="Loss", number=win,indicator="MCC"))

win<-0
tie<-0
loss<-0
for (r in unique(RQ2_unite$classifier)){
  pd<-RQ2_unite[RQ2_unite$classifier==r,]
  pd$X <- with(pd, reorder(pd$X,1-mcc,mean))
  a<-pd$X
  a<-data.frame(a)
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
  comp<-pd$mcc[pd$X==comp_x[2]]
  base<-pd$mcc[pd$X==base_x[1]]
  p<-wilcox.test(comp,base, paired=TRUE)$p.value
  cd<-cliff.delta(comp,base)$estimate
  #print(p)
  
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
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S2", status="Win", number=loss,indicator="MCC"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S2", status="Tie", number=tie,indicator="MCC"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S2", status="Loss", number=win,indicator="MCC"))



win<-0
tie<-0
loss<-0
for (r in unique(RQ2_unite$classifier)){
  pd<-RQ2_unite[RQ2_unite$classifier==r,]
  pd$X <- with(pd, reorder(pd$X,1-mcc,mean))
  a<-pd$X
  a<-data.frame(a)
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
  comp<-pd$mcc[pd$X==comp_x[1]]
  base<-pd$mcc[pd$X==base_x[2]]
  p<-wilcox.test(comp,base, paired=TRUE)$p.value
  cd<-cliff.delta(comp,base)$estimate
  #print(p)
  
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
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S1", status="Win", number=loss,indicator="MCC"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S1", status="Tie", number=tie,indicator="MCC"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S1", status="Loss", number=win,indicator="MCC"))


win<-0
tie<-0
loss<-0
for (r in unique(RQ2_unite$classifier)){
  pd<-RQ2_unite[RQ2_unite$classifier==r,]
  pd$X <- with(pd, reorder(pd$X,1-mcc,mean))
  a<-pd$X
  a<-data.frame(a)
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
  comp<-pd$mcc[pd$X==comp_x[2]]
  base<-pd$mcc[pd$X==base_x[2]]
  p<-wilcox.test(comp,base, paired=TRUE)$p.value
  cd<-cliff.delta(comp,base)$estimate
  #print(p)
  
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
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S2", status="Win", number=loss,indicator="MCC"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S2", status="Tie", number=tie,indicator="MCC"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S2", status="Loss", number=win,indicator="MCC"))


win<-0
tie<-0
loss<-0
for (r in unique(RQ2_unite$classifier)){
  #print(r)
  pd<-RQ2_unite[RQ2_unite$classifier==r,]
  pd$X <- with(pd, reorder(pd$X,1-gmean,mean))
  
  
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
  a = data.frame(base_x)
  b = data.frame(comp_x)
  comp<-pd$gmean[pd$X==comp_x[1]]
  base<-pd$gmean[pd$X==base_x[1]]
  #print(comp_x)
  #print(base_x)
  
  p<-wilcox.test(comp,base, paired=TRUE)$p.value
  cd<-cliff.delta(comp,base)$estimate
  #print(p)
  #print(cd)
  
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
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S1", status="Win", number=loss,indicator="Gmean"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S1", status="Tie", number=tie,indicator="Gmean"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S1", status="Loss", number=win,indicator="Gmean"))

win<-0
tie<-0
loss<-0
for (r in unique(RQ2_unite$classifier)){
  pd<-RQ2_unite[RQ2_unite$classifier==r,]
  pd$X <- with(pd, reorder(pd$X,1-gmean,mean))
  a<-pd$X
  a<-data.frame(a)
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
  comp<-pd$gmean[pd$X==comp_x[2]]
  base<-pd$gmean[pd$X==base_x[1]]
  p<-wilcox.test(comp,base, paired=TRUE)$p.value
  cd<-cliff.delta(comp,base)$estimate
  #print(p)
  
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
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S2", status="Win", number=loss,indicator="Gmean"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S2", status="Tie", number=tie,indicator="Gmean"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S2", status="Loss", number=win,indicator="Gmean"))



win<-0
tie<-0
loss<-0
for (r in unique(RQ2_unite$classifier)){
  pd<-RQ2_unite[RQ2_unite$classifier==r,]
  pd$X <- with(pd, reorder(pd$X,1-gmean,mean))
  a<-pd$X
  a<-data.frame(a)
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
  comp<-pd$gmean[pd$X==comp_x[1]]
  base<-pd$gmean[pd$X==base_x[2]]
  p<-wilcox.test(comp,base, paired=TRUE)$p.value
  cd<-cliff.delta(comp,base)$estimate
  #print(p)
  
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
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S1", status="Win", number=loss,indicator="Gmean"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S1", status="Tie", number=tie,indicator="Gmean"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S1", status="Loss", number=win,indicator="Gmean"))


win<-0
tie<-0
loss<-0
for (r in unique(RQ2_unite$classifier)){
  pd<-RQ2_unite[RQ2_unite$classifier==r,]
  pd$X <- with(pd, reorder(pd$X,1-gmean,mean))
  a<-pd$X
  a<-data.frame(a)
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
  comp<-pd$gmean[pd$X==comp_x[2]]
  base<-pd$gmean[pd$X==base_x[2]]
  p<-wilcox.test(comp,base, paired=TRUE)$p.value
  cd<-cliff.delta(comp,base)$estimate
  #print(p)
  
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
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S2", status="Win", number=loss,indicator="Gmean"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S2", status="Tie", number=tie,indicator="Gmean"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S2", status="Loss", number=win,indicator="Gmean"))

win<-0
tie<-0
loss<-0
for (r in unique(RQ2_unite$classifier)){
  #print(r)
  pd<-RQ2_unite[RQ2_unite$classifier==r,]
  pd$X <- with(pd, reorder(pd$X,1-gmean,mean))
  
  
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
  a = data.frame(base_x)
  b = data.frame(comp_x)
  comp<-pd$gmean[pd$X==comp_x[1]]
  base<-pd$gmean[pd$X==base_x[1]]
  #print(comp_x)
  #print(base_x)
  
  p<-wilcox.test(comp,base, paired=TRUE)$p.value
  cd<-cliff.delta(comp,base)$estimate
  #print(p)
  #print(cd)
  
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
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S1", status="Win", number=loss,indicator="Gmeasure"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S1", status="Tie", number=tie,indicator="Gmeasure"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S1", status="Loss", number=win,indicator="Gmeasure"))

win<-0
tie<-0
loss<-0
for (r in unique(RQ2_unite$classifier)){
  pd<-RQ2_unite[RQ2_unite$classifier==r,]
  pd$X <- with(pd, reorder(pd$X,1-gmeasure,mean))
  a<-pd$X
  a<-data.frame(a)
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
  #print(p)
  
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
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S2", status="Win", number=loss,indicator="Gmeasure"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S2", status="Tie", number=tie,indicator="Gmeasure"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T1 VS. S2", status="Loss", number=win,indicator="Gmeasure"))



win<-0
tie<-0
loss<-0
for (r in unique(RQ2_unite$classifier)){
  pd<-RQ2_unite[RQ2_unite$classifier==r,]
  pd$X <- with(pd, reorder(pd$X,1-gmeasure,mean))
  a<-pd$X
  a<-data.frame(a)
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
  #print(p)
  
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
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S1", status="Win", number=loss,indicator="Gmeasure"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S1", status="Tie", number=tie,indicator="Gmeasure"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S1", status="Loss", number=win,indicator="Gmeasure"))


win<-0
tie<-0
loss<-0
for (r in unique(RQ2_unite$classifier)){
  pd<-RQ2_unite[RQ2_unite$classifier==r,]
  pd$X <- with(pd, reorder(pd$X,1-gmeasure,mean))
  a<-pd$X
  a<-data.frame(a)
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
  #print(p)
  
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
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S2", status="Win", number=loss,indicator="Gmeasure"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S2", status="Tie", number=tie,indicator="Gmeasure"))
bar2_classifier<-rbind(bar2_classifier,data.frame(x="T2 VS. S2", status="Loss", number=win,indicator="Gmeasure"))


bar2_classifier_classifier<-bar2_classifier[bar2_classifier$number!=0,]
bar2_classifier_classifier$indicator[bar2_classifier_classifier$indicator=='F1']<-'F1'
bar2_classifier_classifier$indicator[bar2_classifier_classifier$indicator=='MCC']<-'MCC'
bar2_classifier_classifier$indicator[bar2_classifier_classifier$indicator=='Recall']<-'Recall'
bar2_classifier_classifier$indicator[bar2_classifier_classifier$indicator=='Gmean']<-'Gmean'
bar2_classifier_classifier$indicator[bar2_classifier_classifier$indicator=='Gmeasure']<-'Gmeasure'
bar2_classifier_classifier$x= factor(bar2_classifier_classifier$x, levels=c("T1 VS. S1","T1 VS. S2","T2 VS. S1","T2 VS. S2"))
bar2_classifier_classifier$indicator= factor(bar2_classifier_classifier$indicator, levels=c("Recall","F1","MCC","Gmean","Gmeasure"))

ggplot(data = bar2_classifier_classifier, mapping = aes(x = x, y = number, fill = status)) + geom_bar(stat = 'identity',width=0.6, position = 'stack') + geom_text(aes(label=number),position=position_stack(0.5), color="white", size=3.5) + facet_wrap(~ indicator, nrow=1) + theme(strip.text = element_text(size=15),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),axis.title  = element_blank() ,axis.text.y = element_text(face = "bold",size=16),axis.text.x = element_text(face="bold",size=16,angle=60,hjust = 1,vjust = 1))+scale_fill_manual(values=c("#0011ff","#7B7B7B","#ff0000"))

p<-ggplot(data = bar2_classifier_classifier, mapping = aes(x = x, y = number, fill = status)) + geom_bar(stat = 'identity',width=0.6, position = 'stack') + geom_text(aes(label=number),position=position_stack(0.5), color="white", size=5) + facet_wrap(~ indicator, nrow=1) + theme(plot.margin=unit(c(1,1,1,1),'lines'),text = element_text(family = "serif"),strip.text = element_text(size=20),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),plot.title=element_text(size=20,hjust=0.5),legend.title=element_text(size=16),legend.text=element_text(size=16),axis.line = element_line(colour = "black"),axis.title  = element_blank() ,axis.text.y = element_text(face = "bold",size=18),axis.text.x = element_text(face="bold",size=18,angle=60,hjust = 1,vjust = 1))+scale_fill_manual(values=c("#0011ff","#7B7B7B","#ff0000"))+guides(fill=FALSE)

p

ggsave(p, file='RQ2_classifier.pdf', width=10, height=6) # ????ָ????С??????Ϊ12cm????Ϊ10cm????Ҫָ??????·??

print(viewer)
