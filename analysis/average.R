ALL_average<-NULL


ALL <- read.csv("../all4.csv",header=TRUE,stringsAsFactors=FALSE)

for (d in unique(ALL$dataset)){
  print(d)
  fast<-ALL[ALL$dataset==d,]
  temp<-NULL
  for (s in unique(fast$subsample)){
    for (e in unique(fast$ensemble)){
      fast_1<-fast[fast$subsample==s&fast$ensemble==e,]
      for (c in unique(fast_1$classifier)){
        for (t in unique(fast_1$threshold)){
          recall<-mean(fast_1$recall[fast_1$classifier==c&fast_1$threshold==t])
          precision<-mean(fast_1$precision[fast_1$classifier==c&fast_1$threshold==t])
          f1<-mean(fast_1$f1[fast_1$classifier==c&fast_1$threshold==t])
          mcc<-mean(fast_1$mcc[fast_1$classifier==c&fast_1$threshold==t])
          auc<-mean(fast_1$auc[fast_1$classifier==c&fast_1$threshold==t])
          
          gmean<-mean(fast_1$gmean[fast_1$classifier==c&fast_1$threshold==t])
          gmeasure<-mean(fast_1$gmeasure[fast_1$classifier==c&fast_1$threshold==t])
          temp<-rbind(temp,data.frame(repository=ALL$repository[ALL$dataset==d][1],dataset=d,subsample=s,ensemble=e,classifier=c,threshold=t,recall=recall,precision=precision,f1=f1,mcc=mcc,auc=auc,gmean=gmean,gmeasure=gmeasure))
        }
      }
    }
  }
  ALL_average<-rbind(ALL_average,temp)
}
write.csv(ALL_average,file = "average4.csv",row.names = F)
