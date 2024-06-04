
library(tidyr)
library(ggplot2)
library(effsize)
library(readr)
ALL$threshold[ALL$threshold==0]='DV'
ALL$threshold[ALL$threshold==1]='BV'
ALL$threshold[ALL$threshold==2]='AV'
ALL$threshold[ALL$threshold==3]='GV'
ALL$threshold[ALL$threshold==4]='DR'
ALL$threshold[ALL$threshold==5]='BR'
ALL$threshold[ALL$threshold==6]='AR'
ALL$threshold[ALL$threshold==7]='GR'
ALL$threshold[ALL$threshold==8]='max_f1'
ALL$threshold[ALL$threshold==9]='max_mcc'
ALL$threshold[ALL$threshold==10]='max_gmean'
ALL$threshold[ALL$threshold==11]='max_gmeasure'
ALL$threshold[ALL$threshold==12]='min_d2h'

RQ2_table<-NULL
RQ2 <- ALL[(ALL$subsample=="ORIGIN"&ALL$ensemble=="ORIGIN")|((ALL$subsample!="ORIGIN"|ALL$ensemble!="ORIGIN")&ALL$threshold=="DV"),]
RQ2<-RQ2[RQ2$repository!='SOFTLAB',]
RQ2<-RQ2[RQ2$classifier!='KNN',]
RQ2_unite <-unite(RQ2,"X",subsample,ensemble,threshold)
RQ2_unite$gmeasure[is.na(RQ2_unite$gmeasure)]<-0


for (d in unique(RQ2_unite$classifier)){
  base_f1<-RQ2_unite$f1[RQ2_unite$classifier==d&RQ2_unite$X=="ORIGIN_ORIGIN_DV"]
  base_mcc<-RQ2_unite$mcc[RQ2_unite$classifier==d&RQ2_unite$X=="ORIGIN_ORIGIN_DV"]
  base_recall<-RQ2_unite$recall[RQ2_unite$classifier==d&RQ2_unite$X=="ORIGIN_ORIGIN_DV"]
  base_gmean<-RQ2_unite$gmean[RQ2_unite$classifier==d&RQ2_unite$X=="ORIGIN_ORIGIN_DV"]
  base_gmeasure<-RQ2_unite$gmeasure[RQ2_unite$classifier==d&RQ2_unite$X=="ORIGIN_ORIGIN_DV"]
  
  for (t in unique(RQ2_unite$X)){
    f1<-mean(RQ2_unite$f1[RQ2_unite$classifier==d&RQ2_unite$X==t])
    mcc<-mean(RQ2_unite$mcc[RQ2_unite$classifier==d&RQ2_unite$X==t])
    recall<-mean(RQ2_unite$recall[RQ2_unite$classifier==d&RQ2_unite$X==t])
    gmean<-mean(RQ2_unite$gmean[RQ2_unite$classifier==d&RQ2_unite$X==t])
    gmeasure<-mean(RQ2_unite$gmeasure[RQ2_unite$classifier==d&RQ2_unite$X==t])
    
    comp_f1<-RQ2_unite$f1[RQ2_unite$classifier==d&RQ2_unite$X==t]
    comp_mcc<-RQ2_unite$mcc[RQ2_unite$classifier==d&RQ2_unite$X==t]
    comp_recall<-RQ2_unite$recall[RQ2_unite$classifier==d&RQ2_unite$X==t]
    comp_gmean<-RQ2_unite$gmean[RQ2_unite$classifier==d&RQ2_unite$X==t]
    comp_gmeasure<-RQ2_unite$gmeasure[RQ2_unite$classifier==d&RQ2_unite$X==t]
    
    f1_cd<-cliff.delta(comp_f1,base_f1)$estimate
    mcc_cd<-cliff.delta(comp_mcc,base_mcc)$estimate
    recall_cd<-cliff.delta(comp_recall,base_recall)$estimate
    gmean_cd<-cliff.delta(comp_gmean,base_gmean)$estimate
    gmeasure_cd<-cliff.delta(comp_gmeasure,base_gmeasure)$estimate
    if (t == "ORIGIN_ORIGIN_DV"){
      mcc_p<-1
      f1_p<-1
      recall_p<-1
      gmean_p<-1
      gmeasure_p<-1
    }
    else{
      f1_p<-wilcox.test(comp_f1,base_f1, paired=TRUE)$p.value
      mcc_p<-wilcox.test(comp_mcc,base_mcc, paired=TRUE)$p.value
      recall_p<-wilcox.test(comp_recall,base_recall, paired=TRUE)$p.value
      gmean_p<-wilcox.test(comp_gmean,base_gmean, paired=TRUE)$p.value
      gmeasure_p<-wilcox.test(comp_gmeasure,base_gmeasure, paired=TRUE)$p.value
      
    }
    RQ2_table<-rbind(RQ2_table,data.frame(classifier=d,X=t,f1=f1,f1_cd=f1_cd,f1_p=f1_p,mcc=mcc,mcc_cd=mcc_cd,mcc_p=mcc_p,recall=recall,recall_cd=recall_cd,recall_p=recall_p,gmean=gmean,gmean_cd=gmean_cd,gmean_p=gmean_p,gmeasure=gmeasure,gmeasure_cd=gmeasure_cd,gmeasure_p=gmeasure_p))
  }
}

RQ2_table_latex_str<-NULL
for (d in unique(RQ2_table$classifier)){
  RQ2_table_latex_str<-paste(RQ2_table_latex_str,'\\multirow{7}{*}{F1}&',d,sep = "")
  for (t in unique(RQ2_table$X)){
   
      temp<-sprintf("%.3f",RQ2_table$f1[RQ2_table$X==t&RQ2_table$classifier==d])

      RQ2_table_latex_str<-paste(RQ2_table_latex_str,temp,sep = "&")
    
  }
  RQ2_table_latex_str<-paste(RQ2_table_latex_str,"\\\\ \\cline{3-10}\n",sep = "")
}
RQ2_table_latex_str<-paste(RQ2_table_latex_str,"&AVG","")
for (t in unique(RQ2_table$X)){
  RQ2_table_latex_str<-paste(RQ2_table_latex_str,sprintf("%.3f",mean(RQ2_table$f1[RQ2_table$X==t])),sep = "&")
}
RQ2_table_latex_str<-paste(RQ2_table_latex_str,"\\\\ \\cline{3-10}\n",sep = "")
RQ2_table_latex_str<-paste(RQ2_table_latex_str,"&W/T/L&"," & & & & & & & ")
RQ2_table_latex_str<-paste(RQ2_table_latex_str,"\\\\ \\hline\n",sep = "")

for (d in unique(RQ2_table$classifier)){
  RQ2_table_latex_str<-paste(RQ2_table_latex_str,'\\multirow{7}{*}{MCC}&',d,sep = "")
  for (t in unique(RQ2_table$X)){
    
      temp<-sprintf("%.3f",RQ2_table$mcc[RQ2_table$X==t&RQ2_table$classifier==d])

      RQ2_table_latex_str<-paste(RQ2_table_latex_str,temp,sep = "&")
    }
  
  RQ2_table_latex_str<-paste(RQ2_table_latex_str,"\\\\ \\cline{3-10}\n",sep = "")
}
RQ2_table_latex_str<-paste(RQ2_table_latex_str,"&AVG","")
for (t in unique(RQ2_table$X)){
  RQ2_table_latex_str<-paste(RQ2_table_latex_str,sprintf("%.3f",mean(RQ2_table$mcc[RQ2_table$X==t])),sep = "&")
}
RQ2_table_latex_str<-paste(RQ2_table_latex_str,"\\\\ \\cline{3-10}\n",sep = "")
RQ2_table_latex_str<-paste(RQ2_table_latex_str,"&W/T/L&"," & & & & & & & ")
RQ2_table_latex_str<-paste(RQ2_table_latex_str,"\\\\ \\hline\n",sep = "")




for (d in unique(RQ2_table$classifier)){
  RQ2_table_latex_str<-paste(RQ2_table_latex_str,'\\multirow{7}{*}{Recall}&',d,sep = "")
  for (t in unique(RQ2_table$X)){
    
      temp<-sprintf("%.3f",RQ2_table$recall[RQ2_table$X==t&RQ2_table$classifier==d])
      
      RQ2_table_latex_str<-paste(RQ2_table_latex_str,temp,sep = "&")
    
  }
  RQ2_table_latex_str<-paste(RQ2_table_latex_str,"\\\\ \\cline{3-10}\n",sep = "")
}
RQ2_table_latex_str<-paste(RQ2_table_latex_str,"&AVG","")
for (t in unique(RQ2_table$X)){
  RQ2_table_latex_str<-paste(RQ2_table_latex_str,sprintf("%.3f",mean(RQ2_table$recall[RQ2_table$X==t])),sep = "&")
}
RQ2_table_latex_str<-paste(RQ2_table_latex_str,"\\\\ \\cline{3-10}\n",sep = "")
RQ2_table_latex_str<-paste(RQ2_table_latex_str,"&W/T/L&"," & & & & & & & ")
RQ2_table_latex_str<-paste(RQ2_table_latex_str,"\\\\ \\hline\n",sep = "")




for (d in unique(RQ2_table$classifier)){
  RQ2_table_latex_str<-paste(RQ2_table_latex_str,'\\multirow{7}{*}{Gmeasure}&',d,sep = "")
  for (t in unique(RQ2_table$X)){
    
      temp<-sprintf("%.3f",RQ2_table$recall[RQ2_table$X==t&RQ2_table$classifier==d])
     
      RQ2_table_latex_str<-paste(RQ2_table_latex_str,temp,sep = "&")
    
  }
  RQ2_table_latex_str<-paste(RQ2_table_latex_str,"\\\\ \\cline{3-10}\n",sep = "")
}
RQ2_table_latex_str<-paste(RQ2_table_latex_str,"&AVG","")
for (t in unique(RQ2_table$X)){
  RQ2_table_latex_str<-paste(RQ2_table_latex_str,sprintf("%.3f",mean(RQ2_table$gmeasure[RQ2_table$X==t])),sep = "&")
}
RQ2_table_latex_str<-paste(RQ2_table_latex_str,"\\\\ \\cline{3-10}\n",sep = "")
RQ2_table_latex_str<-paste(RQ2_table_latex_str,"&W/T/L&"," & & & & & & & ")
RQ2_table_latex_str<-paste(RQ2_table_latex_str,"\\\\ \\hline\n",sep = "")




for (d in unique(RQ2_table$classifier)){
  RQ2_table_latex_str<-paste(RQ2_table_latex_str,'\\multirow{7}{*}{Gmean}&',d,sep = "")
  for (t in unique(RQ2_table$X)){
    
      temp<-sprintf("%.3f",RQ2_table$gmean[RQ2_table$X==t&RQ2_table$classifier==d])
     
      RQ2_table_latex_str<-paste(RQ2_table_latex_str,temp,sep = "&")
    
  }
  RQ2_table_latex_str<-paste(RQ2_table_latex_str,"\\\\ \\cline{3-10}\n",sep = "")
}
RQ2_table_latex_str<-paste(RQ2_table_latex_str,"&AVG","")
for (t in unique(RQ2_table$X)){
  RQ2_table_latex_str<-paste(RQ2_table_latex_str,sprintf("%.3f",mean(RQ2_table$gmean[RQ2_table$X==t])),sep = "&")
}
RQ2_table_latex_str<-paste(RQ2_table_latex_str,"\\\\ \\cline{3-10}\n",sep = "")
RQ2_table_latex_str<-paste(RQ2_table_latex_str,"&W/T/L&"," & & & & & & & ")
RQ2_table_latex_str<-paste(RQ2_table_latex_str,"\\\\ \\hline\n",sep = "")
# for (d in unique(RQ2_unite$dataset)){
#   RQ2_table_latex_str<-paste(RQ2_table_latex_str,d,sep = "")
#   if(RQ2_table$f1_cd[RQ2_table$X=='ORIGIN_ORIGIN_GV'&RQ2_table$dataset==d]>0.147&RQ2_table$f1_p[RQ2_table$X=='ORIGIN_ORIGIN_GV'&RQ2_table$dataset==d]<0.05){
#     ORIGIN_ORIGIN_GV_F1<-paste("\\cellcolor{mygray}",sprintf("%.3f",RQ2_table$f1[RQ2_table$X=='ORIGIN_ORIGIN_GV'&RQ2_table$dataset==d]),sep = "")
#   }
#   else{
#     ORIGIN_ORIGIN_GV_F1<-sprintf("%.3f",RQ2_table$f1[RQ2_table$X=='ORIGIN_ORIGIN_GV'&RQ2_table$dataset==d])
#     
#   }
#   
#   if(RQ2_table$f1_cd[RQ2_table$X=='OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]>0.147&RQ2_table$f1_p[RQ2_table$X=='OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]<0.05){
#     OVER_SAMPLING_BAGGING_DV_F1<-paste("\\cellcolor{mygray}",sprintf("%.3f",RQ2_table$f1[RQ2_table$X=='OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]),' (',sprintf("%.3f",RQ2_table$f1_cd[RQ2_table$X=='OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]),")",sep = "")
#   }
#   else{
#     OVER_SAMPLING_BAGGING_DV_F1<-paste(sprintf("%.3f",RQ2_table$f1[RQ2_table$X=='OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]),' (',sprintf("%.3f",RQ2_table$f1_cd[RQ2_table$X=='OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]),")",sep = "")
#     
#   }
#   
#   if(RQ2_table$f1_cd[RQ2_table$X=='SMOTE_BAGGING_DV'&RQ2_table$dataset==d]>0.147&RQ2_table$f1_p[RQ2_table$X=='SMOTE_BAGGING_DV'&RQ2_table$dataset==d]<0.05){
#     SMOTE_BAGGING_DV_F1<-paste("\\cellcolor{mygray}",sprintf("%.3f",RQ2_table$f1[RQ2_table$X=='SMOTE_BAGGING_DV'&RQ2_table$dataset==d]),' (',sprintf("%.3f",RQ2_table$f1_cd[RQ2_table$X=='SMOTE_BAGGING_DV'&RQ2_table$dataset==d]),")",sep = "")
#   }
#   else{
#     SMOTE_BAGGING_DV_F1<-paste(sprintf("%.3f",RQ2_table$f1[RQ2_table$X=='SMOTE_BAGGING_DV'&RQ2_table$dataset==d]),' (',sprintf("%.3f",RQ2_table$f1_cd[RQ2_table$X=='SMOTE_BAGGING_DV'&RQ2_table$dataset==d]),")",sep = "")
#     
#   }
#   
#   if(RQ2_table$f1_cd[RQ2_table$X=='UNDER-OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]>0.147&RQ2_table$f1_p[RQ2_table$X=='UNDER-OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]<0.05){
#     UNDER_OVER_SAMPLING_BAGGING_DV_F1<-paste("\\cellcolor{mygray}",sprintf("%.3f",RQ2_table$f1[RQ2_table$X=='UNDER-OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]),' (',sprintf("%.3f",RQ2_table$f1_cd[RQ2_table$X=='UNDER-OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]),")",sep = "")
#   }
#   else{
#     UNDER_OVER_SAMPLING_BAGGING_DV_F1<-paste(sprintf("%.3f",RQ2_table$f1[RQ2_table$X=='UNDER-OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]),' (',sprintf("%.3f",RQ2_table$f1_cd[RQ2_table$X=='UNDER-OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]),")",sep = "")
#     
#   }
#   
#   if(RQ2_table$f1_cd[RQ2_table$X=='UNDER-OVER-SAMPLING_ORIGIN_DV'&RQ2_table$dataset==d]>0.147&RQ2_table$f1_p[RQ2_table$X=='UNDER-OVER-SAMPLING_ORIGIN_DV'&RQ2_table$dataset==d]<0.05){
#     UNDER_OVER_SAMPLING_ORIGIN_DV_F1<-paste("\\cellcolor{mygray}",sprintf("%.3f",RQ2_table$f1[RQ2_table$X=='UNDER-OVER-SAMPLING_ORIGIN_DV'&RQ2_table$dataset==d]),' (',sprintf("%.3f",RQ2_table$f1_cd[RQ2_table$X=='UNDER-OVER-SAMPLING_ORIGIN_DV'&RQ2_table$dataset==d]),")",sep = "")
#   }
#   else{
#     UNDER_OVER_SAMPLING_ORIGIN_DV_F1<-paste(sprintf("%.3f",RQ2_table$f1[RQ2_table$X=='UNDER-OVER-SAMPLING_ORIGIN_DV'&RQ2_table$dataset==d]),' (',sprintf("%.3f",RQ2_table$f1_cd[RQ2_table$X=='UNDER-OVER-SAMPLING_ORIGIN_DV'&RQ2_table$dataset==d]),")",sep = "")
#     
#   }
#   
#   if(RQ2_table$mcc_cd[RQ2_table$X=='ORIGIN_ORIGIN_GV'&RQ2_table$dataset==d]>0.147&RQ2_table$mcc_p[RQ2_table$X=='ORIGIN_ORIGIN_GV'&RQ2_table$dataset==d]<0.05){
#     ORIGIN_ORIGIN_GV_mcc<-paste("\\cellcolor{mygray}",sprintf("%.3f",RQ2_table$mcc[RQ2_table$X=='ORIGIN_ORIGIN_GV'&RQ2_table$dataset==d]),sep = "")
#   }
#   else{
#     ORIGIN_ORIGIN_GV_mcc<-sprintf("%.3f",RQ2_table$mcc[RQ2_table$X=='ORIGIN_ORIGIN_GV'&RQ2_table$dataset==d])
#   }
#   if(RQ2_table$mcc_cd[RQ2_table$X=='OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]>0.147&RQ2_table$mcc_p[RQ2_table$X=='OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]<0.05){
#     OVER_SAMPLING_BAGGING_DV_mcc<-paste("\\cellcolor{mygray}",sprintf("%.3f",RQ2_table$mcc[RQ2_table$X=='OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]),' (',sprintf("%.3f",RQ2_table$mcc_cd[RQ2_table$X=='OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]),")",sep = "")
#   }
#   else{
#     OVER_SAMPLING_BAGGING_DV_mcc<-paste(sprintf("%.3f",RQ2_table$mcc[RQ2_table$X=='OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]),' (',sprintf("%.3f",RQ2_table$mcc_cd[RQ2_table$X=='OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]),")",sep = "")
#     
#   }
#   if(RQ2_table$mcc_cd[RQ2_table$X=='SMOTE_BAGGING_DV'&RQ2_table$dataset==d]>0.147&RQ2_table$mcc_p[RQ2_table$X=='SMOTE_BAGGING_DV'&RQ2_table$dataset==d]<0.05){
#     SMOTE_BAGGING_DV_mcc<-paste("\\cellcolor{mygray}",sprintf("%.3f",RQ2_table$mcc[RQ2_table$X=='SMOTE_BAGGING_DV'&RQ2_table$dataset==d]),' (',sprintf("%.3f",RQ2_table$mcc_cd[RQ2_table$X=='SMOTE_BAGGING_DV'&RQ2_table$dataset==d]),")",sep = "")
#   }
#   else{
#     SMOTE_BAGGING_DV_mcc<-paste(sprintf("%.3f",RQ2_table$mcc[RQ2_table$X=='SMOTE_BAGGING_DV'&RQ2_table$dataset==d]),' (',sprintf("%.3f",RQ2_table$mcc_cd[RQ2_table$X=='SMOTE_BAGGING_DV'&RQ2_table$dataset==d]),")",sep = "")
#     
#   }
#   if(RQ2_table$mcc_cd[RQ2_table$X=='UNDER-OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]>0.147&RQ2_table$mcc_p[RQ2_table$X=='UNDER-OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]<0.05){
#     UNDER_OVER_SAMPLING_BAGGING_DV_mcc<-paste("\\cellcolor{mygray}",sprintf("%.3f",RQ2_table$mcc[RQ2_table$X=='UNDER-OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]),' (',sprintf("%.3f",RQ2_table$mcc_cd[RQ2_table$X=='UNDER-OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]),")",sep = "")
#   }
#   else{
#     UNDER_OVER_SAMPLING_BAGGING_DV_mcc<-paste(sprintf("%.3f",RQ2_table$mcc[RQ2_table$X=='UNDER-OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]),' (',sprintf("%.3f",RQ2_table$mcc_cd[RQ2_table$X=='UNDER-OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]),")",sep = "")
#     
#   }
#   if(RQ2_table$mcc_cd[RQ2_table$X=='UNDER-OVER-SAMPLING_ORIGIN_DV'&RQ2_table$dataset==d]>0.147&RQ2_table$mcc_p[RQ2_table$X=='UNDER-OVER-SAMPLING_ORIGIN_DV'&RQ2_table$dataset==d]<0.05){
#     UNDER_OVER_SAMPLING_ORIGIN_DV_mcc<-paste("\\cellcolor{mygray}",sprintf("%.3f",RQ2_table$mcc[RQ2_table$X=='UNDER-OVER-SAMPLING_ORIGIN_DV'&RQ2_table$dataset==d]),' (',sprintf("%.3f",RQ2_table$mcc_cd[RQ2_table$X=='UNDER-OVER-SAMPLING_ORIGIN_DV'&RQ2_table$dataset==d]),")",sep = "")
#   }
#   else{
#     UNDER_OVER_SAMPLING_ORIGIN_DV_mcc<-paste(sprintf("%.3f",RQ2_table$mcc[RQ2_table$X=='UNDER-OVER-SAMPLING_ORIGIN_DV'&RQ2_table$dataset==d]),' (',sprintf("%.3f",RQ2_table$mcc_cd[RQ2_table$X=='UNDER-OVER-SAMPLING_ORIGIN_DV'&RQ2_table$dataset==d]),")",sep = "")
#     
#   
#   }
#   RQ2_table_latex_str<-paste(RQ2_table_latex_str,ORIGIN_ORIGIN_GV_F1,OVER_SAMPLING_BAGGING_DV_F1,SMOTE_BAGGING_DV_F1,UNDER_OVER_SAMPLING_BAGGING_DV_F1,UNDER_OVER_SAMPLING_ORIGIN_DV_F1,ORIGIN_ORIGIN_GV_mcc,OVER_SAMPLING_BAGGING_DV_mcc,SMOTE_BAGGING_DV_mcc,UNDER_OVER_SAMPLING_BAGGING_DV_mcc,UNDER_OVER_SAMPLING_ORIGIN_DV_mcc,sep = "&")
#   RQ2_table_latex_str<-paste(RQ2_table_latex_str,"\\\\ \\hline\n",sep = "")
# }
# 
# print(RQ2_table_latex_str)
# 
# RQ2_table_latex<-NULL
# for (d in unique(RQ2_unite$dataset)){
#   ORIGIN_ORIGIN_GV_F1<-paste(sprintf("%.3f",RQ2_table$f1[RQ2_table$X=='ORIGIN_ORIGIN_GV'&RQ2_table$dataset==d]),' (',sprintf("%.3f",RQ2_table$f1_cd[RQ2_table$X=='ORIGIN_ORIGIN_GV'&RQ2_table$dataset==d]),")",sep = "")
#   OVER_SAMPLING_BAGGING_DV_F1<-paste(sprintf("%.3f",RQ2_table$f1[RQ2_table$X=='OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]),' (',sprintf("%.3f",RQ2_table$f1_cd[RQ2_table$X=='OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]),")",sep = "")
#   SMOTE_BAGGING_DV_F1<-paste(sprintf("%.3f",RQ2_table$f1[RQ2_table$X=='SMOTE_BAGGING_DV'&RQ2_table$dataset==d]),' (',sprintf("%.3f",RQ2_table$f1_cd[RQ2_table$X=='SMOTE_BAGGING_DV'&RQ2_table$dataset==d]),")",sep = "")
#   UNDER_OVER_SAMPLING_BAGGING_DV_F1<-paste(sprintf("%.3f",RQ2_table$f1[RQ2_table$X=='UNDER-OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]),' (',sprintf("%.3f",RQ2_table$f1_cd[RQ2_table$X=='UNDER-OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]),")",sep = "")
#   UNDER_OVER_SAMPLING_ORIGIN_DV_F1<-paste(sprintf("%.3f",RQ2_table$f1[RQ2_table$X=='UNDER-OVER-SAMPLING_ORIGIN_DV'&RQ2_table$dataset==d]),' (',sprintf("%.3f",RQ2_table$f1_cd[RQ2_table$X=='UNDER-OVER-SAMPLING_ORIGIN_DV'&RQ2_table$dataset==d]),")",sep = "")
#   ORIGIN_ORIGIN_GV_mcc<-paste(sprintf("%.3f",RQ2_table$mcc[RQ2_table$X=='ORIGIN_ORIGIN_GV'&RQ2_table$dataset==d]),' (',sprintf("%.3f",RQ2_table$mcc_cd[RQ2_table$X=='ORIGIN_ORIGIN_GV'&RQ2_table$dataset==d]),")",sep = "")
#   OVER_SAMPLING_BAGGING_DV_mcc<-paste(sprintf("%.3f",RQ2_table$mcc[RQ2_table$X=='OVER_SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]),' (',sprintf("%.3f",RQ2_table$mcc_cd[RQ2_table$X=='OVER_SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]),")",sep = "")
#   SMOTE_BAGGING_DV_mcc<-paste(sprintf("%.3f",RQ2_table$mcc[RQ2_table$X=='SMOTE_BAGGING_DV'&RQ2_table$dataset==d]),' (',sprintf("%.3f",RQ2_table$mcc_cd[RQ2_table$X=='SMOTE_BAGGING_DV'&RQ2_table$dataset==d]),")",sep = "")
#   UNDER_OVER_SAMPLING_BAGGING_DV_mcc<-paste(sprintf("%.3f",RQ2_table$mcc[RQ2_table$X=='UNDER-OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]),' (',sprintf("%.3f",RQ2_table$mcc_cd[RQ2_table$X=='UNDER-OVER-SAMPLING_BAGGING_DV'&RQ2_table$dataset==d]),")",sep = "")
#   UNDER_OVER_SAMPLING_ORIGIN_DV_mcc<-paste(sprintf("%.3f",RQ2_table$mcc[RQ2_table$X=='UNDER-OVER-SAMPLING_ORIGIN_DV'&RQ2_table$dataset==d]),' (',sprintf("%.3f",RQ2_table$mcc_cd[RQ2_table$X=='UNDER-OVER-SAMPLING_ORIGIN_DV'&RQ2_table$dataset==d]),")",sep = "")
#   RQ2_table_latex<-rbind(RQ2_table_latex,data.frame(dataset=d,ORIGIN_ORIGIN_GV_F1=ORIGIN_ORIGIN_GV_F1,OVER_SAMPLING_BAGGING_DV_F1=OVER_SAMPLING_BAGGING_DV_F1,SMOTE_BAGGING_DV_F1=SMOTE_BAGGING_DV_F1,UNDER_OVER_SAMPLING_BAGGING_DV_F1=UNDER_OVER_SAMPLING_BAGGING_DV_F1,UNDER_OVER_SAMPLING_ORIGIN_DV_F1=UNDER_OVER_SAMPLING_ORIGIN_DV_F1,ORIGIN_ORIGIN_GV_mcc=ORIGIN_ORIGIN_GV_mcc,OVER_SAMPLING_BAGGING_DV_mcc=OVER_SAMPLING_BAGGING_DV_mcc,SMOTE_BAGGING_DV_mcc=SMOTE_BAGGING_DV_mcc,UNDER_OVER_SAMPLING_BAGGING_DV_mcc=UNDER_OVER_SAMPLING_BAGGING_DV_mcc,UNDER_OVER_SAMPLING_ORIGIN_DV_mcc=UNDER_OVER_SAMPLING_ORIGIN_DV_mcc))
# }

print(RQ2_table_latex_str)
print(unique(RQ2_table$X))

write.table(RQ2_table_latex_str, "./RQ2_LateX2.txt")
