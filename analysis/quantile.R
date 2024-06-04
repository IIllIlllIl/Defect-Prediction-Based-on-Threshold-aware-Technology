bugratio<-read.csv("C:\\Users\\wu\\Desktop\\Rnalytica\\bug_ratio_all.csv")

quantile<-quantile(bugratio$bug_ratio)
dis<-ALL[((ALL$subsample=="ORIGIN"&ALL$ensemble=="ORIGIN")),]
dis<-dis[dis$classifier!='KNN',]


dis_f<-dis
dis_f$classifier='ALL'
dis<- rbind(dis_f,dis)
unique(dis$dataset)


groupA<- bugratio[bugratio$bug_ratio>=quantile[[4]],]
groupB<- bugratio[bugratio$bug_ratio<quantile[[4]]&bugratio$bug_ratio>=quantile[[3]],]
groupC<- bugratio[bugratio$bug_ratio<quantile[[3]]&bugratio$bug_ratio>=quantile[[2]],]
groupD<- bugratio[bugratio$bug_ratio<quantile[[2]]&bugratio$bug_ratio>=quantile[[1]],]

bar_quantile<-NULL

for(f in unique(groupA$file)){
  group<-dis[dis$dataset==f,]
  for(c in unique(group$classifier)){
    for(d in unique(group$dataset)){
      baseline<-group[group$dataset==d & group$classifier==c & group$threshold=="DV",]
       
      for(t in unique(group$threshold)){
        if(t=='DV') next;
        comp<-group[group$dataset==d & group$classifier==c & group$threshold==t,]
        bar_quantile<-rbind(bar_quantile,data.frame(dataset=d,threshold=t,classifier=c,value=comp$recall-baseline$recall,measure="Recall",group="75%-100%"))
        bar_quantile<-rbind(bar_quantile,data.frame(dataset=d,threshold=t,classifier=c,value=comp$gmean-baseline$gmean,measure="Gmean",group="75%-100%"))
        bar_quantile<-rbind(bar_quantile,data.frame(dataset=d,threshold=t,classifier=c,value=comp$gmeasure-baseline$gmeasure,measure="Gmeasure",group="75%-100%"))
        bar_quantile<-rbind(bar_quantile,data.frame(dataset=d,threshold=t,classifier=c,value=comp$mcc-baseline$mcc,measure="MCC",group="75%-100%"))
        bar_quantile<-rbind(bar_quantile,data.frame(dataset=d,threshold=t,classifier=c,value=comp$f1-baseline$f1,measure="F1",group="75%-100%"))
        }
    }
  }
}

for(f in unique(groupB$file)){
  group<-dis[dis$dataset==f,]
  for(c in unique(group$classifier)){
    for(d in unique(group$dataset)){
      baseline<-group[group$dataset==d & group$classifier==c & group$threshold=="DV",]
      
      for(t in unique(group$threshold)){
        if(t=='DV') next;
        comp<-group[group$dataset==d & group$classifier==c & group$threshold==t,]
        bar_quantile<-rbind(bar_quantile,data.frame(dataset=d,threshold=t,classifier=c,value=comp$recall-baseline$recall,measure="Recall",group="50%-75%"))
        bar_quantile<-rbind(bar_quantile,data.frame(dataset=d,threshold=t,classifier=c,value=comp$gmean-baseline$gmean,measure="Gmean",group="50%-75%"))
        bar_quantile<-rbind(bar_quantile,data.frame(dataset=d,threshold=t,classifier=c,value=comp$gmeasure-baseline$gmeasure,measure="Gmeasure",group="50%-75%"))
        bar_quantile<-rbind(bar_quantile,data.frame(dataset=d,threshold=t,classifier=c,value=comp$mcc-baseline$mcc,measure="MCC",group="50%-75%"))
        bar_quantile<-rbind(bar_quantile,data.frame(dataset=d,threshold=t,classifier=c,value=comp$f1-baseline$f1,measure="F1",group="50%-75%"))
      }
    }
  }
}

for(f in unique(groupC$file)){
  group<-dis[dis$dataset==f,]
  for(c in unique(group$classifier)){
    for(d in unique(group$dataset)){
      baseline<-group[group$dataset==d & group$classifier==c & group$threshold=="DV",]
      
      for(t in unique(group$threshold)){
        if(t=='DV') next;
        comp<-group[group$dataset==d & group$classifier==c & group$threshold==t,]
        bar_quantile<-rbind(bar_quantile,data.frame(dataset=d,threshold=t,classifier=c,value=comp$recall-baseline$recall,measure="Recall",group="25%-50%"))
        bar_quantile<-rbind(bar_quantile,data.frame(dataset=d,threshold=t,classifier=c,value=comp$gmean-baseline$gmean,measure="Gmean",group="25%-50%"))
        bar_quantile<-rbind(bar_quantile,data.frame(dataset=d,threshold=t,classifier=c,value=comp$gmeasure-baseline$gmeasure,measure="Gmeasure",group="25%-50%"))
        bar_quantile<-rbind(bar_quantile,data.frame(dataset=d,threshold=t,classifier=c,value=comp$mcc-baseline$mcc,measure="MCC",group="25%-50%"))
        bar_quantile<-rbind(bar_quantile,data.frame(dataset=d,threshold=t,classifier=c,value=comp$f1-baseline$f1,measure="F1",group="25%-50%"))
      }
    }
  }
}

for(f in unique(groupD$file)){
  group<-dis[dis$dataset==f,]
  for(c in unique(group$classifier)){
    for(d in unique(group$dataset)){
      baseline<-group[group$dataset==d & group$classifier==c & group$threshold=="DV",]
      
      for(t in unique(group$threshold)){
        if(t=='DV') next;
        comp<-group[group$dataset==d & group$classifier==c & group$threshold==t,]
        bar_quantile<-rbind(bar_quantile,data.frame(dataset=d,threshold=t,classifier=c,value=comp$recall-baseline$recall,measure="Recall",group="0%-25%"))
        bar_quantile<-rbind(bar_quantile,data.frame(dataset=d,threshold=t,classifier=c,value=comp$gmean-baseline$gmean,measure="Gmean",group="0%-25%"))
        bar_quantile<-rbind(bar_quantile,data.frame(dataset=d,threshold=t,classifier=c,value=comp$gmeasure-baseline$gmeasure,measure="Gmeasure",group="0%-25%"))
        bar_quantile<-rbind(bar_quantile,data.frame(dataset=d,threshold=t,classifier=c,value=comp$mcc-baseline$mcc,measure="MCC",group="0%-25%"))
        bar_quantile<-rbind(bar_quantile,data.frame(dataset=d,threshold=t,classifier=c,value=comp$f1-baseline$f1,measure="F1",group="0%-25%"))
      }
    }
  }
}


bar_quantile$measure<-factor(bar_quantile$measure,levels = c("Recall","F1","MCC","Gmean","Gmeasure"))
bar_quantile$classifier<-factor(bar_quantile$classifier,levels = c("ALL","LR","NB","RF","DT","SVM"))
bar_quantile$group<-factor(bar_quantile$group ,levels = c("0%-25%","25%-50%","50%-75%","75%-100%"))

p<-ggplot(bar_quantile, aes(x=group,y=value))+geom_boxplot()+ stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "green")+ theme(text = element_text(family = "serif"),legend.position = 'none',plot.margin=unit(c(1,3,1,1),'lines'),plot.title=element_text(size=20,hjust=0.5),strip.text = element_text(size=16),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),axis.title  = element_blank() ,axis.text.x = element_text(face="bold",size=12,angle=60,hjust = 1,vjust = 1),axis.text.y = element_text(size=12))+ scale_y_continuous()+ facet_wrap(~ measure, scales="free")

p
ggsave(p, file='quantile.pdf', width=8, height=4)
