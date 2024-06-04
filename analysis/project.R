#install.packages("psych")
library(psych)
library(ggplot2)
#install.packages("igraph")
library(igraph)
#install.packages("ggm")
library(ggm)


ALL<-read.csv("./average0.csv",header=TRUE)

RQ3 <- ALL[((ALL$subsample=="ORIGIN"&ALL$ensemble=="ORIGIN")),]


RQ3<-RQ3[RQ3$classifier!='KNN',]
RQ3<-RQ3[RQ3$dataset!='ar3',]
RQ3<-RQ3[RQ3$dataset!='ar4',]
RQ3<-RQ3[RQ3$threshold!='DV',]


RQ3_f<-RQ3
RQ3_f$classifier='ALL'
RQ3_unite = rbind(RQ3_f,RQ3)



oldlist<-c('ant 1.3','ant 1.5',
           'ivy 1.4','ivy 2.0',
           'jedit 4.2','jedit 4.3',
           'xerces 1.2','xerces 1.3',
           'camel 1.0','camel 1.0','camel 1.4','camel 1.4','camel 1.6','camel 1.6',
           'activemq-5.0.0','activemq-5.0.0','activemq-5.0.0','activemq-5.0.0','activemq-5.1.0','activemq-5.1.0','activemq-5.1.0','activemq-5.1.0','activemq-5.2.0','activemq-5.2.0','activemq-5.2.0','activemq-5.2.0','activemq-5.3.0','activemq-5.3.0','activemq-5.3.0','activemq-5.3.0','activemq-5.8.0','activemq-5.8.0','activemq-5.8.0','activemq-5.8.0',
           'groovy-1 5 7','groovy-1 5 7',"groovy-1 6 BETA 1","groovy-1 6 BETA 1",'groovy-1 6 BETA 2','groovy-1 6 BETA 2',
           'hive-0.9.0','hive-0.9.0','hive-0.10.0','hive-0.10.0','hive-0.12.0','hive-0.12.0',
           'jruby-1.1','jruby-1.1','jruby-1.1','jruby-1.4.0','jruby-1.4.0','jruby-1.4.0','jruby-1.5.0','jruby-1.5.0','jruby-1.5.0','jruby-1.7.0.preview1','jruby-1.7.0.preview1','jruby-1.7.0.preview1',
           'lucene-2.9.0','lucene-2.9.0','lucene-3.0.0','lucene-3.0.0','lucene-3.1','lucene-3.1',
           'wicket-1.3.0-incubating-beta-1','wicket-1.3.0-incubating-beta-1','wicket-1.3.0-beta2','wicket-1.3.0-beta2','wicket-1.5.3','wicket-1.5.3',
           'camel-1.4.0','camel-1.4.0','camel-1.4.0','camel-2.9.0','camel-2.9.0','camel-2.9.0','camel-2.10.0','camel-2.10.0','camel-2.10.0','camel-2.11.0','camel-2.11.0','camel-2.11.0'
           )

newlist<-c('ant 1.5','ant 1.3',
           'ivy 2.0','ivy 1.4',
           'jedit 4.3','jedit 4.2',
           'xerces 1.3','xerces 1.2',
           'camel 1.4','camel 1.6','camel 1.0','camel 1.6','camel 1.0','camel 1.4',
           'activemq-5.1.0','activemq-5.2.0','activemq-5.3.0','activemq-5.8.0','activemq-5.0.0','activemq-5.2.0','activemq-5.3.0','activemq-5.8.0','activemq-5.0.0','activemq-5.1.0','activemq-5.3.0','activemq-5.8.0','activemq-5.0.0','activemq-5.1.0','activemq-5.2.0','activemq-5.8.0','activemq-5.0.0','activemq-5.1.0','activemq-5.2.0','activemq-5.3.0',
           "groovy-1 6 BETA 1",'groovy-1 6 BETA 2','groovy-1 5 7','groovy-1 6 BETA 2','groovy-1 5 7',"groovy-1 6 BETA 1",
           'hive-0.10.0','hive-0.12.0','hive-0.9.0','hive-0.12.0','hive-0.9.0','hive-0.10.0',
           'jruby-1.4.0','jruby-1.5.0','jruby-1.7.0.preview1','jruby-1.1.0','jruby-1.5.0','jruby-1.7.0.preview1','jruby-1.1.0','jruby-1.4.0','jruby-1.7.0.preview1','jruby-1.1.0','jruby-1.4.0','jruby-1.7.0',
           'lucene-3.0.0','lucene-3.1','lucene-2.9.0','lucene-3.1','lucene-2.9.0','lucene-3.0.0',
           'wicket-1.3.0-beta2','wicket-1.5.3','wicket-1.3.0-incubating-beta-1','wicket-1.5.3','wicket-1.3.0-incubating-beta-1','wicket-1.3.0-beta2',
           'camel-2.10.0','camel-2.11.0','camel-2.9.0','camel-1.4.0','camel-2.10.0','camel-2.11.0','camel-1.4.0','camel-2.9.0','camel-2.11.0','camel-1.4.0','camel-2.9.0','camel-2.10.0')


# oldlist<-c('ant 1.3','ant 1.5','ivy 1.4','ivy 2.0','jedit 4.2','jedit 4.3','xerces 1.2','xerces 1.3','camel 1.0','camel 1.0','camel 1.4','camel 1.4','camel 1.6','camel 1.6')
# newlist<-c('ant 1.5','ant 1.3','ivy 2.0','ivy 1.4','jedit 4.3','jedit 4.2','xerces 1.3','xerces 1.2','camel 1.4','camel 1.6','camel 1.0','camel 1.6','camel 1.0','camel 1.4')


newlist[which(oldlist=='groovy-1 5 7')]

bar<-NULL

RQ3_unite$threshold[RQ3_unite$threshold==0]='DV'
RQ3_unite$threshold[RQ3_unite$threshold==1]='BV'
RQ3_unite$threshold[RQ3_unite$threshold==2]='AV'
RQ3_unite$threshold[RQ3_unite$threshold==3]='GV'
RQ3_unite$threshold[RQ3_unite$threshold==4]='DR'
RQ3_unite$threshold[RQ3_unite$threshold==5]='BR'
RQ3_unite$threshold[RQ3_unite$threshold==6]='AR'
RQ3_unite$threshold[RQ3_unite$threshold==7]='GR'
#RQ3_unite$threshold[RQ3_unite$threshold==8]='↑F1'
#RQ3_unite$threshold[RQ3_unite$threshold==9]='↑MCC'
#RQ3_unite$threshold[RQ3_unite$threshold==10]='↑Gmn'
#RQ3_unite$threshold[RQ3_unite$threshold==11]='↑Gms'
#RQ3_unite$threshold[RQ3_unite$threshold==12]='↓D2h'
RQ3_unite$threshold[RQ3_unite$threshold==8]='??F1'
RQ3_unite$threshold[RQ3_unite$threshold==9]='??MCC'
RQ3_unite$threshold[RQ3_unite$threshold==10]='??Gmn'
RQ3_unite$threshold[RQ3_unite$threshold==11]='??Gms'
RQ3_unite$threshold[RQ3_unite$threshold==12]='??D2h'

for(c in unique(RQ3_unite$classifier)){
  for(r in unique(RQ3_unite$repository)){
  
    
    base<-RQ3_unite[RQ3_unite$classifier==c  & RQ3_unite$repository==r ,]
    print(base)
    
    for (d1 in unique(base$dataset)){
      
      baseline<-base[base$dataset==d1,]
      
      for(d2 in unique(base$dataset)){
        
        if(d1==d2) next
        if(d1 %in% oldlist){
          if(d2 %in% newlist[which(oldlist==d1)]){
            next
          }
        }
        
      
        comp<-base[base$dataset==d2,]
        
        
        #f1
        baseline$threshold <- with(baseline, reorder(baseline$threshold,1-f1,mean))
        comp$threshold <- with(comp, reorder(comp$threshold,1-f1,mean))
        # 
        # base_x<-c(0,0,0,0,0,0,0,0,0,0,0,0)
        # comp_x<-c(0,0,0,0,0,0,0,0,0,0,0,0)
        
        base_x<-c(0,0,0,0,0,0,0)
        comp_x<-c(0,0,0,0,0,0,0)
        count<-1
        
        for (x in levels(baseline$threshold)){
          if(x=="BV") {base_x[1]=count}
          else if(x=="AV") {base_x[2]=count}
          else if(x=="GV") {base_x[3]=count}
          else if(x=="DR") {base_x[4]=count}
          else if(x=="BR") {base_x[5]=count}
          else if(x=="AR") {base_x[6]=count}
          else if(x=="GR") {base_x[7]=count}
          #else if(x=="↑F1") {base_x[8]=count}
          #else if(x=="↑MCC") {base_x[9]=count}
          #else if(x=="↑Gmn") {base_x[10]=count}
          #else if(x=="↑Gms") {base_x[11]=count}
          #else if(x=="↓D2h") {base_x[12]=count}
          else if(x=="??F1") {base_x[8]=count}
          else if(x=="??MCC") {base_x[9]=count}
          else if(x=="??Gmn") {base_x[10]=count}
          else if(x=="??Gms") {base_x[11]=count}
          else if(x=="??D2h") {base_x[12]=count}
          else {next}
          count<-count+1;
          
        }
        count<-1
        for (x in levels(comp$threshold)){
          if(x=="BV") {comp_x[1]=count}
          else if(x=="AV") {comp_x[2]=count}
          else if(x=="GV") {comp_x[3]=count}
          else if(x=="DR") {comp_x[4]=count}
          else if(x=="BR") {comp_x[5]=count}
          else if(x=="AR") {comp_x[6]=count}
          else if(x=="GR") {comp_x[7]=count}
          #else if(x=="↑F1") {base_x[8]=count}
          #else if(x=="↑MCC") {base_x[9]=count}
          #else if(x=="↑Gmn") {base_x[10]=count}
          #else if(x=="↑Gms") {base_x[11]=count}
          #else if(x=="↓D2h") {base_x[12]=count}
          else if(x=="??F1") {base_x[8]=count}
          else if(x=="??MCC") {base_x[9]=count}
          else if(x=="??Gmn") {base_x[10]=count}
          else if(x=="??Gms") {base_x[11]=count}
          else if(x=="??D2h") {base_x[12]=count}
          else {next}
          count<-count+1;
        }
        print("f1")
        print(base_x)
        print(comp_x)
        score<-cor(base_x,comp_x,method="spearman")
        bar<-rbind(bar,data.frame(base=d1,comp=d2,cor=score,measure="F1",classifier=c))
        
        
        #mcc
        baseline$threshold <- with(baseline, reorder(baseline$threshold,1-mcc,mean))
        comp$threshold <- with(comp, reorder(comp$threshold,1-mcc,mean))
        
        # base_x<-c(0,0,0,0,0,0,0,0,0,0,0,0)
        # comp_x<-c(0,0,0,0,0,0,0,0,0,0,0,0)
        
        base_x<-c(0,0,0,0,0,0,0)
        comp_x<-c(0,0,0,0,0,0,0)
        count<-1
        
        for (x in levels(baseline$threshold)){
          if(x=="BV") {base_x[1]=count}
          else if(x=="AV") {base_x[2]=count}
          else if(x=="GV") {base_x[3]=count}
          else if(x=="DR") {base_x[4]=count}
          else if(x=="BR") {base_x[5]=count}
          else if(x=="AR") {base_x[6]=count}
          else if(x=="GR") {base_x[7]=count}
          #else if(x=="↑F1") {base_x[8]=count}
          #else if(x=="↑MCC") {base_x[9]=count}
          #else if(x=="↑Gmn") {base_x[10]=count}
          #else if(x=="↑Gms") {base_x[11]=count}
          #else if(x=="↓D2h") {base_x[12]=count}
          else if(x=="??F1") {base_x[8]=count}
          else if(x=="??MCC") {base_x[9]=count}
          else if(x=="??Gmn") {base_x[10]=count}
          else if(x=="??Gms") {base_x[11]=count}
          else if(x=="??D2h") {base_x[12]=count}
          else {next}
          count<-count+1;
          
        }
        count<-1
        for (x in levels(comp$threshold)){
          if(x=="BV") {comp_x[1]=count}
          else if(x=="AV") {comp_x[2]=count}
          else if(x=="GV") {comp_x[3]=count}
          else if(x=="DR") {comp_x[4]=count}
          else if(x=="BR") {comp_x[5]=count}
          else if(x=="AR") {comp_x[6]=count}
          else if(x=="GR") {comp_x[7]=count}
          #else if(x=="↑F1") {base_x[8]=count}
          #else if(x=="↑MCC") {base_x[9]=count}
          #else if(x=="↑Gmn") {base_x[10]=count}
          #else if(x=="↑Gms") {base_x[11]=count}
          #else if(x=="↓D2h") {base_x[12]=count}
          else if(x=="??F1") {base_x[8]=count}
          else if(x=="??MCC") {base_x[9]=count}
          else if(x=="??Gmn") {base_x[10]=count}
          else if(x=="??Gms") {base_x[11]=count}
          else if(x=="??D2h") {base_x[12]=count}
          else {next}
          count<-count+1;
        }
        
        print("mcc")
        print(base_x)
        print(comp_x)
        score<-cor(base_x,comp_x,method="spearman")
        bar<-rbind(bar,data.frame(base=d1,comp=d2,cor=score,measure="MCC",classifier=c))
        
        #recall
        baseline$threshold <- with(baseline, reorder(baseline$threshold,1-recall,mean))
        comp$threshold <- with(comp, reorder(comp$threshold,1-recall,mean))
        
        # base_x<-c(0,0,0,0,0,0,0,0,0,0,0,0)
        # comp_x<-c(0,0,0,0,0,0,0,0,0,0,0,0)
        
        base_x<-c(0,0,0,0,0,0,0)
        comp_x<-c(0,0,0,0,0,0,0)
        count<-1
        
        for (x in levels(baseline$threshold)){
          if(x=="BV") {base_x[1]=count}
          else if(x=="AV") {base_x[2]=count}
          else if(x=="GV") {base_x[3]=count}
          else if(x=="DR") {base_x[4]=count}
          else if(x=="BR") {base_x[5]=count}
          else if(x=="AR") {base_x[6]=count}
          else if(x=="GR") {base_x[7]=count}
          #else if(x=="↑F1") {base_x[8]=count}
          #else if(x=="↑MCC") {base_x[9]=count}
          #else if(x=="↑Gmn") {base_x[10]=count}
          #else if(x=="↑Gms") {base_x[11]=count}
          #else if(x=="↓D2h") {base_x[12]=count}
          else if(x=="??F1") {base_x[8]=count}
          else if(x=="??MCC") {base_x[9]=count}
          else if(x=="??Gmn") {base_x[10]=count}
          else if(x=="??Gms") {base_x[11]=count}
          else if(x=="??D2h") {base_x[12]=count}
          else {next}
          count<-count+1;
          
        }
        count<-1
        for (x in levels(comp$threshold)){
          if(x=="BV") {comp_x[1]=count}
          else if(x=="AV") {comp_x[2]=count}
          else if(x=="GV") {comp_x[3]=count}
          else if(x=="DR") {comp_x[4]=count}
          else if(x=="BR") {comp_x[5]=count}
          else if(x=="AR") {comp_x[6]=count}
          else if(x=="GR") {comp_x[7]=count}
          #else if(x=="↑F1") {base_x[8]=count}
          #else if(x=="↑MCC") {base_x[9]=count}
          #else if(x=="↑Gmn") {base_x[10]=count}
          #else if(x=="↑Gms") {base_x[11]=count}
          #else if(x=="↓D2h") {base_x[12]=count}
          else if(x=="??F1") {base_x[8]=count}
          else if(x=="??MCC") {base_x[9]=count}
          else if(x=="??Gmn") {base_x[10]=count}
          else if(x=="??Gms") {base_x[11]=count}
          else if(x=="??D2h") {base_x[12]=count}
          else {next}
          count<-count+1;
        }
        
        print("recall")
        print(base_x)
        print(comp_x)
        score<-cor(base_x,comp_x,method="spearman")
        bar<-rbind(bar,data.frame(base=d1,comp=d2,cor=score,measure="Recall",classifier=c))
        
        #gmean
        baseline$threshold <- with(baseline, reorder(baseline$threshold,1-gmean,mean))
        comp$threshold <- with(comp, reorder(comp$threshold,1-gmean,mean))
        
        # base_x<-c(0,0,0,0,0,0,0,0,0,0,0,0)
        # comp_x<-c(0,0,0,0,0,0,0,0,0,0,0,0)
        base_x<-c(0,0,0,0,0,0,0)
        comp_x<-c(0,0,0,0,0,0,0)
        count<-1
        
        for (x in levels(baseline$threshold)){
          if(x=="BV") {base_x[1]=count}
          else if(x=="AV") {base_x[2]=count}
          else if(x=="GV") {base_x[3]=count}
          else if(x=="DR") {base_x[4]=count}
          else if(x=="BR") {base_x[5]=count}
          else if(x=="AR") {base_x[6]=count}
          else if(x=="GR") {base_x[7]=count}
          #else if(x=="↑F1") {base_x[8]=count}
          #else if(x=="↑MCC") {base_x[9]=count}
          #else if(x=="↑Gmn") {base_x[10]=count}
          #else if(x=="↑Gms") {base_x[11]=count}
          #else if(x=="↓D2h") {base_x[12]=count}
          else if(x=="??F1") {base_x[8]=count}
          else if(x=="??MCC") {base_x[9]=count}
          else if(x=="??Gmn") {base_x[10]=count}
          else if(x=="??Gms") {base_x[11]=count}
          else if(x=="??D2h") {base_x[12]=count}
          else {next}
          count<-count+1;
          
        }
        count<-1
        for (x in levels(comp$threshold)){
          if(x=="BV") {comp_x[1]=count}
          else if(x=="AV") {comp_x[2]=count}
          else if(x=="GV") {comp_x[3]=count}
          else if(x=="DR") {comp_x[4]=count}
          else if(x=="BR") {comp_x[5]=count}
          else if(x=="AR") {comp_x[6]=count}
          else if(x=="GR") {comp_x[7]=count}
          #else if(x=="↑F1") {base_x[8]=count}
          #else if(x=="↑MCC") {base_x[9]=count}
          #else if(x=="↑Gmn") {base_x[10]=count}
          #else if(x=="↑Gms") {base_x[11]=count}
          #else if(x=="↓D2h") {base_x[12]=count}
          else if(x=="??F1") {base_x[8]=count}
          else if(x=="??MCC") {base_x[9]=count}
          else if(x=="??Gmn") {base_x[10]=count}
          else if(x=="??Gms") {base_x[11]=count}
          else if(x=="??D2h") {base_x[12]=count}
          else {next}
          count<-count+1;
        }
        print("gmean")
        print(base_x)
        print(comp_x)
        score<-cor(base_x,comp_x,method="spearman")
        bar<-rbind(bar,data.frame(base=d1,comp=d2,cor=score,measure="Gmean",classifier=c))
        
        #g_measure
        baseline$threshold <- with(baseline, reorder(baseline$threshold,1-gmeasure,mean))
        comp$threshold <- with(comp, reorder(comp$threshold,1-gmeasure,mean))
        
        # base_x<-c(0,0,0,0,0,0,0,0,0,0,0,0)
        # comp_x<-c(0,0,0,0,0,0,0,0,0,0,0,0)
        base_x<-c(0,0,0,0,0,0,0)
        comp_x<-c(0,0,0,0,0,0,0)
        count<-1
        
        for (x in levels(baseline$threshold)){
          if(x=="BV") {base_x[1]=count}
          else if(x=="AV") {base_x[2]=count}
          else if(x=="GV") {base_x[3]=count}
          else if(x=="DR") {base_x[4]=count}
          else if(x=="BR") {base_x[5]=count}
          else if(x=="AR") {base_x[6]=count}
          else if(x=="GR") {base_x[7]=count}
          #else if(x=="↑F1") {base_x[8]=count}
          #else if(x=="↑MCC") {base_x[9]=count}
          #else if(x=="↑Gmn") {base_x[10]=count}
          #else if(x=="↑Gms") {base_x[11]=count}
          #else if(x=="↓D2h") {base_x[12]=count}
          else if(x=="??F1") {base_x[8]=count}
          else if(x=="??MCC") {base_x[9]=count}
          else if(x=="??Gmn") {base_x[10]=count}
          else if(x=="??Gms") {base_x[11]=count}
          else if(x=="??D2h") {base_x[12]=count}
          else {next}
          count<-count+1;
          
        }
        count<-1
        for (x in levels(comp$threshold)){
          if(x=="BV") {comp_x[1]=count}
          else if(x=="AV") {comp_x[2]=count}
          else if(x=="GV") {comp_x[3]=count}
          else if(x=="DR") {comp_x[4]=count}
          else if(x=="BR") {comp_x[5]=count}
          else if(x=="AR") {comp_x[6]=count}
          else if(x=="GR") {comp_x[7]=count}
          #else if(x=="↑F1") {base_x[8]=count}
          #else if(x=="↑MCC") {base_x[9]=count}
          #else if(x=="↑Gmn") {base_x[10]=count}
          #else if(x=="↑Gms") {base_x[11]=count}
          #else if(x=="↓D2h") {base_x[12]=count}
          else if(x=="??F1") {base_x[8]=count}
          else if(x=="??MCC") {base_x[9]=count}
          else if(x=="??Gmn") {base_x[10]=count}
          else if(x=="??Gms") {base_x[11]=count}
          else if(x=="??D2h") {base_x[12]=count}
          else {next}
          count<-count+1;
        }
        print("gmeasure")
        print(base_x)
        print(comp_x)
        score<-cor(base_x,comp_x,method="spearman")
        bar<-rbind(bar,data.frame(base=d1,comp=d2,cor=score,measure="Gmeasure",classifier=c))
        
      }
    }
  }
}
# ? not understand
# temp<-bar
# bar<-temp


bar_temp_project<-NULL
bar_temp_project<-bar

bar_temp_project$measure = factor(bar_temp_project$measure, levels=c("Recall","F1","MCC","Gmean","Gmeasure"))

bar_temp_project<-data.frame(bar_temp_project)

# p<-ggplot(bar_temp_project, aes(x=measure,y=cor))+geom_boxplot()+ stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "green")+ theme(text = element_text(family = "serif"),legend.position = 'none',plot.margin=unit(c(1,3,1,1),'lines'),plot.title=element_text(size=20,hjust=0.5),strip.text = element_text(size=20),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),axis.title  = element_blank() ,axis.text.x = element_text(face="bold",size=18,angle=60,hjust = 1,vjust = 1),axis.text.y = element_text(size=18))+ scale_y_continuous(limits = c(-1,1))+ facet_wrap(~ classifier, scales="free",nrow = 1)
p<-ggplot(bar_temp_project, aes(x=measure,y=cor))+geom_boxplot()+ stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "green")+ theme(text = element_text(family = "serif"),legend.position = 'none',plot.margin=unit(c(1,3,1,1),'lines'),plot.title=element_text(size=20,hjust=0.5),strip.text = element_text(size=20),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),axis.title  = element_blank() ,axis.text.x = element_text(face="bold",size=18,angle=60,hjust = 1,vjust = 1),axis.text.y = element_text(size=18))+ scale_y_continuous(limits = c(-1,1))+ facet_wrap(~ classifier, scales="free",nrow = 1)

p
ggsave(p, file='project_old.pdf', width=14, height=6)
