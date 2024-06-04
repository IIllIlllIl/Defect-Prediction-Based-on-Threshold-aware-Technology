library(tidyr)
library(ggplot2)
library(effsize)
library(readr)

library(extrafont)
library(showtext)

ALL <- read.csv("average4.csv",header=TRUE,stringsAsFactors=FALSE)
#ALL <- read.csv("C:\\Users\\wu\\Desktop\\dataset\\result.csv",header=TRUE)

ALL$repository[ALL$repository=='ML']='AEEEM'
ALL$repository[ALL$repository=='LC']='AEEEM'
ALL$repository[ALL$repository=='PDE']='AEEEM'
ALL$repository[ALL$repository=='PC1']='NASA'
ALL$repository[ALL$repository=='CM1']='NASA'
ALL$repository[ALL$repository=='KC3']='NASA'
ALL$repository[ALL$repository=='PC3']='NASA'
ALL$repository[ALL$repository=='PC2']='NASA'
ALL$repository[ALL$repository=='PC4']='NASA'
ALL$repository[ALL$repository=='MC1']='NASA'
ALL$repository[ALL$repository=='MW1']='NASA'
ALL$repository[ALL$repository=='Jedit 4.3']='Promise'
ALL$repository[ALL$repository=='Ivy 1.4']='Promise'
ALL$repository[ALL$repository=='Redaktor']='Promise'
ALL$repository[ALL$repository=='E-learning']='Promise'
ALL$repository[ALL$repository=='Xerces 1.3']='Promise'
ALL$repository[ALL$repository=='Ant 1.5']='Promise'
ALL$repository[ALL$repository=='Camel 1.6']='Promise'
ALL$repository[ALL$repository=='Ant 1.3']='Promise'
ALL$repository[ALL$repository=='Tomcat']='Promise'
ALL$repository[ALL$repository=='Poi 2.0']='Promise'
ALL$repository[ALL$repository=='Camel 1.0']='Promise'
ALL$repository[ALL$repository=='Pbeans 2']='Promise'
ALL$repository[ALL$repository=='Xerces 1.2']='Promise'
ALL$repository[ALL$repository=='Ivy 2.0']='Promise'
ALL$repository[ALL$repository=='Synapse 1.0']='Promise'
ALL$repository[ALL$repository=='Jedit 4.2']='Promise'
ALL$repository[ALL$repository=='Camel 1.4']='Promise'
ALL$repository[ALL$repository=='Forrest 0.7']='Promise'
ALL$repository[ALL$repository=='Arc']='Promise'
ALL$repository[ALL$repository=='Xalan 2.4']='Promise'
ALL$repository[ALL$repository=='Systemdata']='Promise'
ALL$repository[ALL$repository=='lucene-3.1']='Rnalytica'
ALL$repository[ALL$repository=='camel-2.10.0']='Rnalytica'
ALL$repository[ALL$repository=='wicket-1.3.0-beta2']='Rnalytica'
ALL$repository[ALL$repository=='activemq-5.3.0']='Rnalytica'
ALL$repository[ALL$repository=='jruby-1.7.0.preview1']='Rnalytica'
ALL$repository[ALL$repository=='lucene-3.0.0']='Rnalytica'
ALL$repository[ALL$repository=='camel-2.11.0']='Rnalytica'
ALL$repository[ALL$repository=='wicket-1.3.0-incubating-beta-1']='Rnalytica'
ALL$repository[ALL$repository=='hive-0.10.0']='Rnalytica'
ALL$repository[ALL$repository=='groovy-1_6_BETA_2']='Rnalytica'
ALL$repository[ALL$repository=='wicket-1.5.3']='Rnalytica'
ALL$repository[ALL$repository=='derby-10.5.1.1']='Rnalytica'
ALL$repository[ALL$repository=='hive-0.9.0']='Rnalytica'
ALL$repository[ALL$repository=='activemq-5.1.0']='Rnalytica'
ALL$repository[ALL$repository=='lucene-2.9.0']='Rnalytica'
ALL$repository[ALL$repository=='hive-0.12.0']='Rnalytica'
ALL$repository[ALL$repository=='jruby-1.4.0']='Rnalytica'
ALL$repository[ALL$repository=='camel-2.9.0']='Rnalytica'
ALL$repository[ALL$repository=='jruby-1.5.0']='Rnalytica'
ALL$repository[ALL$repository=='activemq-5.8.0']='Rnalytica'
ALL$repository[ALL$repository=='activemq-5.0.0']='Rnalytica'
ALL$repository[ALL$repository=='groovy-1_6_BETA_1']='Rnalytica'
ALL$repository[ALL$repository=='groovy-1_5_7']='Rnalytica'
ALL$repository[ALL$repository=='activemq-5.2.0']='Rnalytica'
ALL$repository[ALL$repository=='camel-1.4.0']='Rnalytica'
ALL$repository[ALL$repository=='jruby-1.1']='Rnalytica'
ALL$measure='Gmeasure'


ALL$threshold[ALL$threshold==0]='DV'
ALL$threshold[ALL$threshold==1]='BV'
ALL$threshold[ALL$threshold==2]='AV'
ALL$threshold[ALL$threshold==3]='GV'
ALL$threshold[ALL$threshold==4]='DR'
ALL$threshold[ALL$threshold==5]='BR'
ALL$threshold[ALL$threshold==6]='AR'
ALL$threshold[ALL$threshold==7]='GR'
ALL$threshold[ALL$threshold==8]='↑F1'
ALL$threshold[ALL$threshold==9]='↑MCC'
ALL$threshold[ALL$threshold==10]='↑Gmn'
ALL$threshold[ALL$threshold==11]='↑Gms'
ALL$threshold[ALL$threshold==12]='vD2h'
#print(str(ALL))
RQ1<-ALL[ALL$ensemble=='ORIGIN'&ALL$subsample=='ORIGIN',]
RQ1<-RQ1[RQ1$repository!='SOFTLAB',]
RQ1<-RQ1[RQ1$classifier!='KNN',]
RQ1_f<-RQ1
RQ1_f$repository='ALL'
RQ1<-rbind(RQ1,RQ1_f)

for (r in unique(RQ1$repository)){
  CA<-RQ1$gmeasure[RQ1$threshold=="DV"&RQ1$repository==r]
  CB<-RQ1$gmeasure[RQ1$threshold=="BV"&RQ1$repository==r]
  CC<-RQ1$gmeasure[RQ1$threshold=="AV"&RQ1$repository==r]
  CD<-RQ1$gmeasure[RQ1$threshold=="GV"&RQ1$repository==r]
  CE<-RQ1$gmeasure[RQ1$threshold=="DR"&RQ1$repository==r]
  CF<-RQ1$gmeasure[RQ1$threshold=="BR"&RQ1$repository==r]
  CG<-RQ1$gmeasure[RQ1$threshold=="AR"&RQ1$repository==r]
  CH<-RQ1$gmeasure[RQ1$threshold=="GR"&RQ1$repository==r]
  CI<-RQ1$gmeasure[RQ1$threshold=="↑F1"&RQ1$repository==r]
  CJ<-RQ1$gmeasure[RQ1$threshold=="↑MCC"&RQ1$repository==r]
  CK<-RQ1$gmeasure[RQ1$threshold=="↑Gmn"&RQ1$repository==r]
  CL<-RQ1$gmeasure[RQ1$threshold=="↑Gms"&RQ1$repository==r]
  CM<-RQ1$gmeasure[RQ1$threshold=="vD2h"&RQ1$repository==r]
  
  
  RQ1$PValue[RQ1$threshold=="DV"&RQ1$repository==r]<-1
  RQ1$CliffDelta[RQ1$threshold=="DV"&RQ1$repository==r]<-cliff.delta(CA,CA)$estimate
  RQ1$PValue[RQ1$threshold=="BV"&RQ1$repository==r]<-wilcox.test(CB,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="BV"&RQ1$repository==r]<-cliff.delta(CB,CA)$estimate
  RQ1$PValue[RQ1$threshold=="AV"&RQ1$repository==r]<-wilcox.test(CC,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="AV"&RQ1$repository==r]<-cliff.delta(CC,CA)$estimate
  RQ1$PValue[RQ1$threshold=="GV"&RQ1$repository==r]<-wilcox.test(CD,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="GV"&RQ1$repository==r]<-cliff.delta(CD,CA)$estimate
  RQ1$PValue[RQ1$threshold=="DR"&RQ1$repository==r]<-wilcox.test(CE,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="DR"&RQ1$repository==r]<-cliff.delta(CE,CA)$estimate
  RQ1$PValue[RQ1$threshold=="BR"&RQ1$repository==r]<-wilcox.test(CF,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="BR"&RQ1$repository==r]<-cliff.delta(CF,CA)$estimate
  RQ1$PValue[RQ1$threshold=="AR"&RQ1$repository==r]<-wilcox.test(CG,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="AR"&RQ1$repository==r]<-cliff.delta(CG,CA)$estimate
  RQ1$PValue[RQ1$threshold=="GR"&RQ1$repository==r]<-wilcox.test(CH,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="GR"&RQ1$repository==r]<-cliff.delta(CH,CA)$estimate
  RQ1$PValue[RQ1$threshold=="↑F1"&RQ1$repository==r]<-wilcox.test(CI,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="↑F1"&RQ1$repository==r]<-cliff.delta(CI,CA)$estimate
  RQ1$PValue[RQ1$threshold=="↑MCC"&RQ1$repository==r]<-wilcox.test(CJ,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="↑MCC"&RQ1$repository==r]<-cliff.delta(CJ,CA)$estimate
  RQ1$PValue[RQ1$threshold=="↑Gmn"&RQ1$repository==r]<-wilcox.test(CK,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="↑Gmn"&RQ1$repository==r]<-cliff.delta(CK,CA)$estimate
  RQ1$PValue[RQ1$threshold=="↑Gms"&RQ1$repository==r]<-wilcox.test(CL,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="↑Gms"&RQ1$repository==r]<-cliff.delta(CL,CA)$estimate
  RQ1$PValue[RQ1$threshold=="vD2h"&RQ1$repository==r]<-wilcox.test(CM,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="vD2h"&RQ1$repository==r]<-cliff.delta(CM,CA)$estimate
}

for(i in(1:length(RQ1$repository))){
  if(RQ1$PValue[i]<0.05&RQ1$CliffDelta[i]>=0){
    RQ1$fill_p[i]="3"
  }
  else if(RQ1$PValue[i]<0.05&RQ1$CliffDelta[i]<0){
    RQ1$fill_p[i]="1"
  }
  else{
    RQ1$fill_p[i]="2"
  }
  
  
  if(abs(RQ1$CliffDelta[i])<0.147){
    RQ1$fill_c[i]="1"
  }
  else if(abs(RQ1$CliffDelta[i])<0.33){
    RQ1$fill_c[i]="2"
  }
  else if(abs(RQ1$CliffDelta[i])<0.474){
    RQ1$fill_c[i]="3"
  }
  else{
    RQ1$fill_c[i]="4"
  }
}

result<-RQ1[,-7][,-7][,-7][,-7][,-7][,-7]
colnames(result)[7] <- 'value'


bar<-NULL
bar<-rbind(bar,result)


ALL <- read.csv("average0.csv",header=TRUE,stringsAsFactors=FALSE)
#ALL <- read.csv("C:\\Users\\wu\\Desktop\\dataset\\result.csv",header=TRUE)
ALL$repository[ALL$repository=='ML']='AEEEM'
ALL$repository[ALL$repository=='LC']='AEEEM'
ALL$repository[ALL$repository=='PDE']='AEEEM'
ALL$repository[ALL$repository=='PC1']='NASA'
ALL$repository[ALL$repository=='CM1']='NASA'
ALL$repository[ALL$repository=='KC3']='NASA'
ALL$repository[ALL$repository=='PC3']='NASA'
ALL$repository[ALL$repository=='PC2']='NASA'
ALL$repository[ALL$repository=='PC4']='NASA'
ALL$repository[ALL$repository=='MC1']='NASA'
ALL$repository[ALL$repository=='MW1']='NASA'
ALL$repository[ALL$repository=='Jedit 4.3']='Promise'
ALL$repository[ALL$repository=='Ivy 1.4']='Promise'
ALL$repository[ALL$repository=='Redaktor']='Promise'
ALL$repository[ALL$repository=='E-learning']='Promise'
ALL$repository[ALL$repository=='Xerces 1.3']='Promise'
ALL$repository[ALL$repository=='Ant 1.5']='Promise'
ALL$repository[ALL$repository=='Camel 1.6']='Promise'
ALL$repository[ALL$repository=='Ant 1.3']='Promise'
ALL$repository[ALL$repository=='Tomcat']='Promise'
ALL$repository[ALL$repository=='Poi 2.0']='Promise'
ALL$repository[ALL$repository=='Camel 1.0']='Promise'
ALL$repository[ALL$repository=='Pbeans 2']='Promise'
ALL$repository[ALL$repository=='Xerces 1.2']='Promise'
ALL$repository[ALL$repository=='Ivy 2.0']='Promise'
ALL$repository[ALL$repository=='Synapse 1.0']='Promise'
ALL$repository[ALL$repository=='Jedit 4.2']='Promise'
ALL$repository[ALL$repository=='Camel 1.4']='Promise'
ALL$repository[ALL$repository=='Forrest 0.7']='Promise'
ALL$repository[ALL$repository=='Arc']='Promise'
ALL$repository[ALL$repository=='Xalan 2.4']='Promise'
ALL$repository[ALL$repository=='Systemdata']='Promise'
ALL$repository[ALL$repository=='lucene-3.1']='Rnalytica'
ALL$repository[ALL$repository=='camel-2.10.0']='Rnalytica'
ALL$repository[ALL$repository=='wicket-1.3.0-beta2']='Rnalytica'
ALL$repository[ALL$repository=='activemq-5.3.0']='Rnalytica'
ALL$repository[ALL$repository=='jruby-1.7.0.preview1']='Rnalytica'
ALL$repository[ALL$repository=='lucene-3.0.0']='Rnalytica'
ALL$repository[ALL$repository=='camel-2.11.0']='Rnalytica'
ALL$repository[ALL$repository=='wicket-1.3.0-incubating-beta-1']='Rnalytica'
ALL$repository[ALL$repository=='hive-0.10.0']='Rnalytica'
ALL$repository[ALL$repository=='groovy-1_6_BETA_2']='Rnalytica'
ALL$repository[ALL$repository=='wicket-1.5.3']='Rnalytica'
ALL$repository[ALL$repository=='derby-10.5.1.1']='Rnalytica'
ALL$repository[ALL$repository=='hive-0.9.0']='Rnalytica'
ALL$repository[ALL$repository=='activemq-5.1.0']='Rnalytica'
ALL$repository[ALL$repository=='lucene-2.9.0']='Rnalytica'
ALL$repository[ALL$repository=='hive-0.12.0']='Rnalytica'
ALL$repository[ALL$repository=='jruby-1.4.0']='Rnalytica'
ALL$repository[ALL$repository=='camel-2.9.0']='Rnalytica'
ALL$repository[ALL$repository=='jruby-1.5.0']='Rnalytica'
ALL$repository[ALL$repository=='activemq-5.8.0']='Rnalytica'
ALL$repository[ALL$repository=='activemq-5.0.0']='Rnalytica'
ALL$repository[ALL$repository=='groovy-1_6_BETA_1']='Rnalytica'
ALL$repository[ALL$repository=='groovy-1_5_7']='Rnalytica'
ALL$repository[ALL$repository=='activemq-5.2.0']='Rnalytica'
ALL$repository[ALL$repository=='camel-1.4.0']='Rnalytica'
ALL$repository[ALL$repository=='jruby-1.1']='Rnalytica'
ALL$measure='Gmean'


ALL$threshold[ALL$threshold==0]='DV'
ALL$threshold[ALL$threshold==1]='BV'
ALL$threshold[ALL$threshold==2]='AV'
ALL$threshold[ALL$threshold==3]='GV'
ALL$threshold[ALL$threshold==4]='DR'
ALL$threshold[ALL$threshold==5]='BR'
ALL$threshold[ALL$threshold==6]='AR'
ALL$threshold[ALL$threshold==7]='GR'
ALL$threshold[ALL$threshold==8]='↑F1'
ALL$threshold[ALL$threshold==9]='↑MCC'
ALL$threshold[ALL$threshold==10]='↑Gmn'
ALL$threshold[ALL$threshold==11]='↑Gms'
ALL$threshold[ALL$threshold==12]='vD2h'
#print(str(ALL))
RQ1<-ALL[ALL$ensemble=='ORIGIN'&ALL$subsample=='ORIGIN',]
RQ1<-RQ1[RQ1$repository!='SOFTLAB',]
RQ1<-RQ1[RQ1$classifier!='KNN',]
RQ1_f<-RQ1
RQ1_f$repository='ALL'
RQ1<-rbind(RQ1,RQ1_f)

for (r in unique(RQ1$repository)){
  CA<-RQ1$gmean[RQ1$threshold=="DV"&RQ1$repository==r]
  CB<-RQ1$gmean[RQ1$threshold=="BV"&RQ1$repository==r]
  CC<-RQ1$gmean[RQ1$threshold=="AV"&RQ1$repository==r]
  CD<-RQ1$gmean[RQ1$threshold=="GV"&RQ1$repository==r]
  CE<-RQ1$gmean[RQ1$threshold=="DR"&RQ1$repository==r]
  CF<-RQ1$gmean[RQ1$threshold=="BR"&RQ1$repository==r]
  CG<-RQ1$gmean[RQ1$threshold=="AR"&RQ1$repository==r]
  CH<-RQ1$gmean[RQ1$threshold=="GR"&RQ1$repository==r]
  CI<-RQ1$gmean[RQ1$threshold=="↑F1"&RQ1$repository==r]
  CJ<-RQ1$gmean[RQ1$threshold=="↑MCC"&RQ1$repository==r]
  CK<-RQ1$gmean[RQ1$threshold=="↑Gmn"&RQ1$repository==r]
  CL<-RQ1$gmean[RQ1$threshold=="↑Gms"&RQ1$repository==r]
  CM<-RQ1$gmean[RQ1$threshold=="vD2h"&RQ1$repository==r]
  
  
  RQ1$PValue[RQ1$threshold=="DV"&RQ1$repository==r]<-1
  RQ1$CliffDelta[RQ1$threshold=="DV"&RQ1$repository==r]<-cliff.delta(CA,CA)$estimate
  RQ1$PValue[RQ1$threshold=="BV"&RQ1$repository==r]<-wilcox.test(CB,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="BV"&RQ1$repository==r]<-cliff.delta(CB,CA)$estimate
  RQ1$PValue[RQ1$threshold=="AV"&RQ1$repository==r]<-wilcox.test(CC,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="AV"&RQ1$repository==r]<-cliff.delta(CC,CA)$estimate
  RQ1$PValue[RQ1$threshold=="GV"&RQ1$repository==r]<-wilcox.test(CD,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="GV"&RQ1$repository==r]<-cliff.delta(CD,CA)$estimate
  RQ1$PValue[RQ1$threshold=="DR"&RQ1$repository==r]<-wilcox.test(CE,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="DR"&RQ1$repository==r]<-cliff.delta(CE,CA)$estimate
  RQ1$PValue[RQ1$threshold=="BR"&RQ1$repository==r]<-wilcox.test(CF,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="BR"&RQ1$repository==r]<-cliff.delta(CF,CA)$estimate
  RQ1$PValue[RQ1$threshold=="AR"&RQ1$repository==r]<-wilcox.test(CG,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="AR"&RQ1$repository==r]<-cliff.delta(CG,CA)$estimate
  RQ1$PValue[RQ1$threshold=="GR"&RQ1$repository==r]<-wilcox.test(CH,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="GR"&RQ1$repository==r]<-cliff.delta(CH,CA)$estimate
  RQ1$PValue[RQ1$threshold=="↑F1"&RQ1$repository==r]<-wilcox.test(CI,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="↑F1"&RQ1$repository==r]<-cliff.delta(CI,CA)$estimate
  RQ1$PValue[RQ1$threshold=="↑MCC"&RQ1$repository==r]<-wilcox.test(CJ,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="↑MCC"&RQ1$repository==r]<-cliff.delta(CJ,CA)$estimate
  RQ1$PValue[RQ1$threshold=="↑Gmn"&RQ1$repository==r]<-wilcox.test(CK,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="↑Gmn"&RQ1$repository==r]<-cliff.delta(CK,CA)$estimate
  RQ1$PValue[RQ1$threshold=="↑Gms"&RQ1$repository==r]<-wilcox.test(CL,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="↑Gms"&RQ1$repository==r]<-cliff.delta(CL,CA)$estimate
  RQ1$PValue[RQ1$threshold=="vD2h"&RQ1$repository==r]<-wilcox.test(CM,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="vD2h"&RQ1$repository==r]<-cliff.delta(CM,CA)$estimate
}

for(i in(1:length(RQ1$repository))){
  if(RQ1$PValue[i]<0.05&RQ1$CliffDelta[i]>=0){
    RQ1$fill_p[i]="3"
  }
  else if(RQ1$PValue[i]<0.05&RQ1$CliffDelta[i]<0){
    RQ1$fill_p[i]="1"
  }
  else{
    RQ1$fill_p[i]="2"
  }
  
  
  if(abs(RQ1$CliffDelta[i])<0.147){
    RQ1$fill_c[i]="1"
  }
  else if(abs(RQ1$CliffDelta[i])<0.33){
    RQ1$fill_c[i]="2"
  }
  else if(abs(RQ1$CliffDelta[i])<0.474){
    RQ1$fill_c[i]="3"
  }
  else{
    RQ1$fill_c[i]="4"
  }
}

result<-RQ1[,-7][,-7][,-7][,-7][,-7][,-8]
colnames(result)[7] <- 'value'

bar<-rbind(bar,result)


ALL <- read.csv("average0.csv",header=TRUE,stringsAsFactors=FALSE)
#ALL <- read.csv("C:\\Users\\wu\\Desktop\\dataset\\result.csv",header=TRUE)

ALL$repository[ALL$repository=='ML']='AEEEM'
ALL$repository[ALL$repository=='LC']='AEEEM'
ALL$repository[ALL$repository=='PDE']='AEEEM'
ALL$repository[ALL$repository=='PC1']='NASA'
ALL$repository[ALL$repository=='CM1']='NASA'
ALL$repository[ALL$repository=='KC3']='NASA'
ALL$repository[ALL$repository=='PC3']='NASA'
ALL$repository[ALL$repository=='PC2']='NASA'
ALL$repository[ALL$repository=='PC4']='NASA'
ALL$repository[ALL$repository=='MC1']='NASA'
ALL$repository[ALL$repository=='MW1']='NASA'
ALL$repository[ALL$repository=='Jedit 4.3']='Promise'
ALL$repository[ALL$repository=='Ivy 1.4']='Promise'
ALL$repository[ALL$repository=='Redaktor']='Promise'
ALL$repository[ALL$repository=='E-learning']='Promise'
ALL$repository[ALL$repository=='Xerces 1.3']='Promise'
ALL$repository[ALL$repository=='Ant 1.5']='Promise'
ALL$repository[ALL$repository=='Camel 1.6']='Promise'
ALL$repository[ALL$repository=='Ant 1.3']='Promise'
ALL$repository[ALL$repository=='Tomcat']='Promise'
ALL$repository[ALL$repository=='Poi 2.0']='Promise'
ALL$repository[ALL$repository=='Camel 1.0']='Promise'
ALL$repository[ALL$repository=='Pbeans 2']='Promise'
ALL$repository[ALL$repository=='Xerces 1.2']='Promise'
ALL$repository[ALL$repository=='Ivy 2.0']='Promise'
ALL$repository[ALL$repository=='Synapse 1.0']='Promise'
ALL$repository[ALL$repository=='Jedit 4.2']='Promise'
ALL$repository[ALL$repository=='Camel 1.4']='Promise'
ALL$repository[ALL$repository=='Forrest 0.7']='Promise'
ALL$repository[ALL$repository=='Arc']='Promise'
ALL$repository[ALL$repository=='Xalan 2.4']='Promise'
ALL$repository[ALL$repository=='Systemdata']='Promise'
ALL$repository[ALL$repository=='lucene-3.1']='Rnalytica'
ALL$repository[ALL$repository=='camel-2.10.0']='Rnalytica'
ALL$repository[ALL$repository=='wicket-1.3.0-beta2']='Rnalytica'
ALL$repository[ALL$repository=='activemq-5.3.0']='Rnalytica'
ALL$repository[ALL$repository=='jruby-1.7.0.preview1']='Rnalytica'
ALL$repository[ALL$repository=='lucene-3.0.0']='Rnalytica'
ALL$repository[ALL$repository=='camel-2.11.0']='Rnalytica'
ALL$repository[ALL$repository=='wicket-1.3.0-incubating-beta-1']='Rnalytica'
ALL$repository[ALL$repository=='hive-0.10.0']='Rnalytica'
ALL$repository[ALL$repository=='groovy-1_6_BETA_2']='Rnalytica'
ALL$repository[ALL$repository=='wicket-1.5.3']='Rnalytica'
ALL$repository[ALL$repository=='derby-10.5.1.1']='Rnalytica'
ALL$repository[ALL$repository=='hive-0.9.0']='Rnalytica'
ALL$repository[ALL$repository=='activemq-5.1.0']='Rnalytica'
ALL$repository[ALL$repository=='lucene-2.9.0']='Rnalytica'
ALL$repository[ALL$repository=='hive-0.12.0']='Rnalytica'
ALL$repository[ALL$repository=='jruby-1.4.0']='Rnalytica'
ALL$repository[ALL$repository=='camel-2.9.0']='Rnalytica'
ALL$repository[ALL$repository=='jruby-1.5.0']='Rnalytica'
ALL$repository[ALL$repository=='activemq-5.8.0']='Rnalytica'
ALL$repository[ALL$repository=='activemq-5.0.0']='Rnalytica'
ALL$repository[ALL$repository=='groovy-1_6_BETA_1']='Rnalytica'
ALL$repository[ALL$repository=='groovy-1_5_7']='Rnalytica'
ALL$repository[ALL$repository=='activemq-5.2.0']='Rnalytica'
ALL$repository[ALL$repository=='camel-1.4.0']='Rnalytica'
ALL$repository[ALL$repository=='jruby-1.1']='Rnalytica'
ALL$measure='MCC'


ALL$threshold[ALL$threshold==0]='DV'
ALL$threshold[ALL$threshold==1]='BV'
ALL$threshold[ALL$threshold==2]='AV'
ALL$threshold[ALL$threshold==3]='GV'
ALL$threshold[ALL$threshold==4]='DR'
ALL$threshold[ALL$threshold==5]='BR'
ALL$threshold[ALL$threshold==6]='AR'
ALL$threshold[ALL$threshold==7]='GR'
ALL$threshold[ALL$threshold==8]='↑F1'
ALL$threshold[ALL$threshold==9]='↑MCC'
ALL$threshold[ALL$threshold==10]='↑Gmn'
ALL$threshold[ALL$threshold==11]='↑Gms'
ALL$threshold[ALL$threshold==12]='vD2h'
#print(str(ALL))
RQ1<-ALL[ALL$ensemble=='ORIGIN'&ALL$subsample=='ORIGIN',]
RQ1<-RQ1[RQ1$repository!='SOFTLAB',]
RQ1<-RQ1[RQ1$classifier!='KNN',]
RQ1_f<-RQ1
RQ1_f$repository='ALL'
RQ1<-rbind(RQ1,RQ1_f)

for (r in unique(RQ1$repository)){
  CA<-RQ1$mcc[RQ1$threshold=="DV"&RQ1$repository==r]
  CB<-RQ1$mcc[RQ1$threshold=="BV"&RQ1$repository==r]
  CC<-RQ1$mcc[RQ1$threshold=="AV"&RQ1$repository==r]
  CD<-RQ1$mcc[RQ1$threshold=="GV"&RQ1$repository==r]
  CE<-RQ1$mcc[RQ1$threshold=="DR"&RQ1$repository==r]
  CF<-RQ1$mcc[RQ1$threshold=="BR"&RQ1$repository==r]
  CG<-RQ1$mcc[RQ1$threshold=="AR"&RQ1$repository==r]
  CH<-RQ1$mcc[RQ1$threshold=="GR"&RQ1$repository==r]
  CI<-RQ1$mcc[RQ1$threshold=="↑F1"&RQ1$repository==r]
  CJ<-RQ1$mcc[RQ1$threshold=="↑MCC"&RQ1$repository==r]
  CK<-RQ1$mcc[RQ1$threshold=="↑Gmn"&RQ1$repository==r]
  CL<-RQ1$mcc[RQ1$threshold=="↑Gms"&RQ1$repository==r]
  CM<-RQ1$mcc[RQ1$threshold=="vD2h"&RQ1$repository==r]
  
  
  RQ1$PValue[RQ1$threshold=="DV"&RQ1$repository==r]<-1
  RQ1$CliffDelta[RQ1$threshold=="DV"&RQ1$repository==r]<-cliff.delta(CA,CA)$estimate
  RQ1$PValue[RQ1$threshold=="BV"&RQ1$repository==r]<-wilcox.test(CB,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="BV"&RQ1$repository==r]<-cliff.delta(CB,CA)$estimate
  RQ1$PValue[RQ1$threshold=="AV"&RQ1$repository==r]<-wilcox.test(CC,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="AV"&RQ1$repository==r]<-cliff.delta(CC,CA)$estimate
  RQ1$PValue[RQ1$threshold=="GV"&RQ1$repository==r]<-wilcox.test(CD,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="GV"&RQ1$repository==r]<-cliff.delta(CD,CA)$estimate
  RQ1$PValue[RQ1$threshold=="DR"&RQ1$repository==r]<-wilcox.test(CE,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="DR"&RQ1$repository==r]<-cliff.delta(CE,CA)$estimate
  RQ1$PValue[RQ1$threshold=="BR"&RQ1$repository==r]<-wilcox.test(CF,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="BR"&RQ1$repository==r]<-cliff.delta(CF,CA)$estimate
  RQ1$PValue[RQ1$threshold=="AR"&RQ1$repository==r]<-wilcox.test(CG,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="AR"&RQ1$repository==r]<-cliff.delta(CG,CA)$estimate
  RQ1$PValue[RQ1$threshold=="GR"&RQ1$repository==r]<-wilcox.test(CH,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="GR"&RQ1$repository==r]<-cliff.delta(CH,CA)$estimate
  RQ1$PValue[RQ1$threshold=="↑F1"&RQ1$repository==r]<-wilcox.test(CI,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="↑F1"&RQ1$repository==r]<-cliff.delta(CI,CA)$estimate
  RQ1$PValue[RQ1$threshold=="↑MCC"&RQ1$repository==r]<-wilcox.test(CJ,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="↑MCC"&RQ1$repository==r]<-cliff.delta(CJ,CA)$estimate
  RQ1$PValue[RQ1$threshold=="↑Gmn"&RQ1$repository==r]<-wilcox.test(CK,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="↑Gmn"&RQ1$repository==r]<-cliff.delta(CK,CA)$estimate
  RQ1$PValue[RQ1$threshold=="↑Gms"&RQ1$repository==r]<-wilcox.test(CL,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="↑Gms"&RQ1$repository==r]<-cliff.delta(CL,CA)$estimate
  RQ1$PValue[RQ1$threshold=="vD2h"&RQ1$repository==r]<-wilcox.test(CM,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="vD2h"&RQ1$repository==r]<-cliff.delta(CM,CA)$estimate
}

for(i in(1:length(RQ1$repository))){
  if(RQ1$PValue[i]<0.05&RQ1$CliffDelta[i]>=0){
    RQ1$fill_p[i]="3"
  }
  else if(RQ1$PValue[i]<0.05&RQ1$CliffDelta[i]<0){
    RQ1$fill_p[i]="1"
  }
  else{
    RQ1$fill_p[i]="2"
  }
  
  
  if(abs(RQ1$CliffDelta[i])<0.147){
    RQ1$fill_c[i]="1"
  }
  else if(abs(RQ1$CliffDelta[i])<0.33){
    RQ1$fill_c[i]="2"
  }
  else if(abs(RQ1$CliffDelta[i])<0.474){
    RQ1$fill_c[i]="3"
  }
  else{
    RQ1$fill_c[i]="4"
  }
}

result<-RQ1[,-7][,-7][,-7][,-8][,-8][,-8]
colnames(result)[7] <- 'value'

bar<-rbind(bar,result)


ALL <- read.csv("average0.csv",header=TRUE,stringsAsFactors=FALSE)
#ALL <- read.csv("C:\\Users\\wu\\Desktop\\dataset\\result.csv",header=TRUE)

ALL$repository[ALL$repository=='ML']='AEEEM'
ALL$repository[ALL$repository=='LC']='AEEEM'
ALL$repository[ALL$repository=='PDE']='AEEEM'
ALL$repository[ALL$repository=='PC1']='NASA'
ALL$repository[ALL$repository=='CM1']='NASA'
ALL$repository[ALL$repository=='KC3']='NASA'
ALL$repository[ALL$repository=='PC3']='NASA'
ALL$repository[ALL$repository=='PC2']='NASA'
ALL$repository[ALL$repository=='PC4']='NASA'
ALL$repository[ALL$repository=='MC1']='NASA'
ALL$repository[ALL$repository=='MW1']='NASA'
ALL$repository[ALL$repository=='Jedit 4.3']='Promise'
ALL$repository[ALL$repository=='Ivy 1.4']='Promise'
ALL$repository[ALL$repository=='Redaktor']='Promise'
ALL$repository[ALL$repository=='E-learning']='Promise'
ALL$repository[ALL$repository=='Xerces 1.3']='Promise'
ALL$repository[ALL$repository=='Ant 1.5']='Promise'
ALL$repository[ALL$repository=='Camel 1.6']='Promise'
ALL$repository[ALL$repository=='Ant 1.3']='Promise'
ALL$repository[ALL$repository=='Tomcat']='Promise'
ALL$repository[ALL$repository=='Poi 2.0']='Promise'
ALL$repository[ALL$repository=='Camel 1.0']='Promise'
ALL$repository[ALL$repository=='Pbeans 2']='Promise'
ALL$repository[ALL$repository=='Xerces 1.2']='Promise'
ALL$repository[ALL$repository=='Ivy 2.0']='Promise'
ALL$repository[ALL$repository=='Synapse 1.0']='Promise'
ALL$repository[ALL$repository=='Jedit 4.2']='Promise'
ALL$repository[ALL$repository=='Camel 1.4']='Promise'
ALL$repository[ALL$repository=='Forrest 0.7']='Promise'
ALL$repository[ALL$repository=='Arc']='Promise'
ALL$repository[ALL$repository=='Xalan 2.4']='Promise'
ALL$repository[ALL$repository=='Systemdata']='Promise'
ALL$repository[ALL$repository=='lucene-3.1']='Rnalytica'
ALL$repository[ALL$repository=='camel-2.10.0']='Rnalytica'
ALL$repository[ALL$repository=='wicket-1.3.0-beta2']='Rnalytica'
ALL$repository[ALL$repository=='activemq-5.3.0']='Rnalytica'
ALL$repository[ALL$repository=='jruby-1.7.0.preview1']='Rnalytica'
ALL$repository[ALL$repository=='lucene-3.0.0']='Rnalytica'
ALL$repository[ALL$repository=='camel-2.11.0']='Rnalytica'
ALL$repository[ALL$repository=='wicket-1.3.0-incubating-beta-1']='Rnalytica'
ALL$repository[ALL$repository=='hive-0.10.0']='Rnalytica'
ALL$repository[ALL$repository=='groovy-1_6_BETA_2']='Rnalytica'
ALL$repository[ALL$repository=='wicket-1.5.3']='Rnalytica'
ALL$repository[ALL$repository=='derby-10.5.1.1']='Rnalytica'
ALL$repository[ALL$repository=='hive-0.9.0']='Rnalytica'
ALL$repository[ALL$repository=='activemq-5.1.0']='Rnalytica'
ALL$repository[ALL$repository=='lucene-2.9.0']='Rnalytica'
ALL$repository[ALL$repository=='hive-0.12.0']='Rnalytica'
ALL$repository[ALL$repository=='jruby-1.4.0']='Rnalytica'
ALL$repository[ALL$repository=='camel-2.9.0']='Rnalytica'
ALL$repository[ALL$repository=='jruby-1.5.0']='Rnalytica'
ALL$repository[ALL$repository=='activemq-5.8.0']='Rnalytica'
ALL$repository[ALL$repository=='activemq-5.0.0']='Rnalytica'
ALL$repository[ALL$repository=='groovy-1_6_BETA_1']='Rnalytica'
ALL$repository[ALL$repository=='groovy-1_5_7']='Rnalytica'
ALL$repository[ALL$repository=='activemq-5.2.0']='Rnalytica'
ALL$repository[ALL$repository=='camel-1.4.0']='Rnalytica'
ALL$repository[ALL$repository=='jruby-1.1']='Rnalytica'
ALL$measure='F1'


ALL$threshold[ALL$threshold==0]='DV'
ALL$threshold[ALL$threshold==1]='BV'
ALL$threshold[ALL$threshold==2]='AV'
ALL$threshold[ALL$threshold==3]='GV'
ALL$threshold[ALL$threshold==4]='DR'
ALL$threshold[ALL$threshold==5]='BR'
ALL$threshold[ALL$threshold==6]='AR'
ALL$threshold[ALL$threshold==7]='GR'
ALL$threshold[ALL$threshold==8]='↑F1'
ALL$threshold[ALL$threshold==9]='↑MCC'
ALL$threshold[ALL$threshold==10]='↑Gmn'
ALL$threshold[ALL$threshold==11]='↑Gms'
ALL$threshold[ALL$threshold==12]='vD2h'
#print(str(ALL))
RQ1<-ALL[ALL$ensemble=='ORIGIN'&ALL$subsample=='ORIGIN',]
RQ1<-RQ1[RQ1$repository!='SOFTLAB',]
RQ1<-RQ1[RQ1$classifier!='KNN',]
RQ1_f<-RQ1
RQ1_f$repository='ALL'
RQ1<-rbind(RQ1,RQ1_f)

for (r in unique(RQ1$repository)){
  CA<-RQ1$f1[RQ1$threshold=="DV"&RQ1$repository==r]
  CB<-RQ1$f1[RQ1$threshold=="BV"&RQ1$repository==r]
  CC<-RQ1$f1[RQ1$threshold=="AV"&RQ1$repository==r]
  CD<-RQ1$f1[RQ1$threshold=="GV"&RQ1$repository==r]
  CE<-RQ1$f1[RQ1$threshold=="DR"&RQ1$repository==r]
  CF<-RQ1$f1[RQ1$threshold=="BR"&RQ1$repository==r]
  CG<-RQ1$f1[RQ1$threshold=="AR"&RQ1$repository==r]
  CH<-RQ1$f1[RQ1$threshold=="GR"&RQ1$repository==r]
  CI<-RQ1$f1[RQ1$threshold=="↑F1"&RQ1$repository==r]
  CJ<-RQ1$f1[RQ1$threshold=="↑MCC"&RQ1$repository==r]
  CK<-RQ1$f1[RQ1$threshold=="↑Gmn"&RQ1$repository==r]
  CL<-RQ1$f1[RQ1$threshold=="↑Gms"&RQ1$repository==r]
  CM<-RQ1$f1[RQ1$threshold=="vD2h"&RQ1$repository==r]
  
  
  RQ1$PValue[RQ1$threshold=="DV"&RQ1$repository==r]<-1
  RQ1$CliffDelta[RQ1$threshold=="DV"&RQ1$repository==r]<-cliff.delta(CA,CA)$estimate
  RQ1$PValue[RQ1$threshold=="BV"&RQ1$repository==r]<-wilcox.test(CB,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="BV"&RQ1$repository==r]<-cliff.delta(CB,CA)$estimate
  RQ1$PValue[RQ1$threshold=="AV"&RQ1$repository==r]<-wilcox.test(CC,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="AV"&RQ1$repository==r]<-cliff.delta(CC,CA)$estimate
  RQ1$PValue[RQ1$threshold=="GV"&RQ1$repository==r]<-wilcox.test(CD,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="GV"&RQ1$repository==r]<-cliff.delta(CD,CA)$estimate
  RQ1$PValue[RQ1$threshold=="DR"&RQ1$repository==r]<-wilcox.test(CE,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="DR"&RQ1$repository==r]<-cliff.delta(CE,CA)$estimate
  RQ1$PValue[RQ1$threshold=="BR"&RQ1$repository==r]<-wilcox.test(CF,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="BR"&RQ1$repository==r]<-cliff.delta(CF,CA)$estimate
  RQ1$PValue[RQ1$threshold=="AR"&RQ1$repository==r]<-wilcox.test(CG,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="AR"&RQ1$repository==r]<-cliff.delta(CG,CA)$estimate
  RQ1$PValue[RQ1$threshold=="GR"&RQ1$repository==r]<-wilcox.test(CH,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="GR"&RQ1$repository==r]<-cliff.delta(CH,CA)$estimate
  RQ1$PValue[RQ1$threshold=="↑F1"&RQ1$repository==r]<-wilcox.test(CI,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="↑F1"&RQ1$repository==r]<-cliff.delta(CI,CA)$estimate
  RQ1$PValue[RQ1$threshold=="↑MCC"&RQ1$repository==r]<-wilcox.test(CJ,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="↑MCC"&RQ1$repository==r]<-cliff.delta(CJ,CA)$estimate
  RQ1$PValue[RQ1$threshold=="↑Gmn"&RQ1$repository==r]<-wilcox.test(CK,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="↑Gmn"&RQ1$repository==r]<-cliff.delta(CK,CA)$estimate
  RQ1$PValue[RQ1$threshold=="↑Gms"&RQ1$repository==r]<-wilcox.test(CL,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="↑Gms"&RQ1$repository==r]<-cliff.delta(CL,CA)$estimate
  RQ1$PValue[RQ1$threshold=="vD2h"&RQ1$repository==r]<-wilcox.test(CM,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="vD2h"&RQ1$repository==r]<-cliff.delta(CM,CA)$estimate
}

for(i in(1:length(RQ1$repository))){
  if(RQ1$PValue[i]<0.05&RQ1$CliffDelta[i]>=0){
    RQ1$fill_p[i]="3"
  }
  else if(RQ1$PValue[i]<0.05&RQ1$CliffDelta[i]<0){
    RQ1$fill_p[i]="1"
  }
  else{
    RQ1$fill_p[i]="2"
  }
  
  
  if(abs(RQ1$CliffDelta[i])<0.147){
    RQ1$fill_c[i]="1"
  }
  else if(abs(RQ1$CliffDelta[i])<0.33){
    RQ1$fill_c[i]="2"
  }
  else if(abs(RQ1$CliffDelta[i])<0.474){
    RQ1$fill_c[i]="3"
  }
  else{
    RQ1$fill_c[i]="4"
  }
}

result<-RQ1[,-7][,-7][,-8][,-8][,-8][,-8]
colnames(result)[7] <- 'value'

bar<-rbind(bar,result)

ALL <- read.csv("average0.csv",header=TRUE,stringsAsFactors=FALSE)
#ALL <- read.csv("C:\\Users\\wu\\Desktop\\dataset\\result.csv",header=TRUE)

ALL$repository[ALL$repository=='ML']='AEEEM'
ALL$repository[ALL$repository=='LC']='AEEEM'
ALL$repository[ALL$repository=='PDE']='AEEEM'
ALL$repository[ALL$repository=='PC1']='NASA'
ALL$repository[ALL$repository=='CM1']='NASA'
ALL$repository[ALL$repository=='KC3']='NASA'
ALL$repository[ALL$repository=='PC3']='NASA'
ALL$repository[ALL$repository=='PC2']='NASA'
ALL$repository[ALL$repository=='PC4']='NASA'
ALL$repository[ALL$repository=='MC1']='NASA'
ALL$repository[ALL$repository=='MW1']='NASA'
ALL$repository[ALL$repository=='Jedit 4.3']='Promise'
ALL$repository[ALL$repository=='Ivy 1.4']='Promise'
ALL$repository[ALL$repository=='Redaktor']='Promise'
ALL$repository[ALL$repository=='E-learning']='Promise'
ALL$repository[ALL$repository=='Xerces 1.3']='Promise'
ALL$repository[ALL$repository=='Ant 1.5']='Promise'
ALL$repository[ALL$repository=='Camel 1.6']='Promise'
ALL$repository[ALL$repository=='Ant 1.3']='Promise'
ALL$repository[ALL$repository=='Tomcat']='Promise'
ALL$repository[ALL$repository=='Poi 2.0']='Promise'
ALL$repository[ALL$repository=='Camel 1.0']='Promise'
ALL$repository[ALL$repository=='Pbeans 2']='Promise'
ALL$repository[ALL$repository=='Xerces 1.2']='Promise'
ALL$repository[ALL$repository=='Ivy 2.0']='Promise'
ALL$repository[ALL$repository=='Synapse 1.0']='Promise'
ALL$repository[ALL$repository=='Jedit 4.2']='Promise'
ALL$repository[ALL$repository=='Camel 1.4']='Promise'
ALL$repository[ALL$repository=='Forrest 0.7']='Promise'
ALL$repository[ALL$repository=='Arc']='Promise'
ALL$repository[ALL$repository=='Xalan 2.4']='Promise'
ALL$repository[ALL$repository=='Systemdata']='Promise'
ALL$repository[ALL$repository=='lucene-3.1']='Rnalytica'
ALL$repository[ALL$repository=='camel-2.10.0']='Rnalytica'
ALL$repository[ALL$repository=='wicket-1.3.0-beta2']='Rnalytica'
ALL$repository[ALL$repository=='activemq-5.3.0']='Rnalytica'
ALL$repository[ALL$repository=='jruby-1.7.0.preview1']='Rnalytica'
ALL$repository[ALL$repository=='lucene-3.0.0']='Rnalytica'
ALL$repository[ALL$repository=='camel-2.11.0']='Rnalytica'
ALL$repository[ALL$repository=='wicket-1.3.0-incubating-beta-1']='Rnalytica'
ALL$repository[ALL$repository=='hive-0.10.0']='Rnalytica'
ALL$repository[ALL$repository=='groovy-1_6_BETA_2']='Rnalytica'
ALL$repository[ALL$repository=='wicket-1.5.3']='Rnalytica'
ALL$repository[ALL$repository=='derby-10.5.1.1']='Rnalytica'
ALL$repository[ALL$repository=='hive-0.9.0']='Rnalytica'
ALL$repository[ALL$repository=='activemq-5.1.0']='Rnalytica'
ALL$repository[ALL$repository=='lucene-2.9.0']='Rnalytica'
ALL$repository[ALL$repository=='hive-0.12.0']='Rnalytica'
ALL$repository[ALL$repository=='jruby-1.4.0']='Rnalytica'
ALL$repository[ALL$repository=='camel-2.9.0']='Rnalytica'
ALL$repository[ALL$repository=='jruby-1.5.0']='Rnalytica'
ALL$repository[ALL$repository=='activemq-5.8.0']='Rnalytica'
ALL$repository[ALL$repository=='activemq-5.0.0']='Rnalytica'
ALL$repository[ALL$repository=='groovy-1_6_BETA_1']='Rnalytica'
ALL$repository[ALL$repository=='groovy-1_5_7']='Rnalytica'
ALL$repository[ALL$repository=='activemq-5.2.0']='Rnalytica'
ALL$repository[ALL$repository=='camel-1.4.0']='Rnalytica'
ALL$repository[ALL$repository=='jruby-1.1']='Rnalytica'
ALL$measure='Recall'


ALL$threshold[ALL$threshold==0]='DV'
ALL$threshold[ALL$threshold==1]='BV'
ALL$threshold[ALL$threshold==2]='AV'
ALL$threshold[ALL$threshold==3]='GV'
ALL$threshold[ALL$threshold==4]='DR'
ALL$threshold[ALL$threshold==5]='BR'
ALL$threshold[ALL$threshold==6]='AR'
ALL$threshold[ALL$threshold==7]='GR'
ALL$threshold[ALL$threshold==8]='↑F1'
ALL$threshold[ALL$threshold==9]='↑MCC'
ALL$threshold[ALL$threshold==10]='↑Gmn'
ALL$threshold[ALL$threshold==11]='↑Gms'
ALL$threshold[ALL$threshold==12]='vD2h'
#print(str(ALL))
RQ1<-ALL[ALL$ensemble=='ORIGIN'&ALL$subsample=='ORIGIN',]
RQ1<-RQ1[RQ1$repository!='SOFTLAB',]
RQ1<-RQ1[RQ1$classifier!='KNN',]
RQ1_f<-RQ1
RQ1_f$repository='ALL'
RQ1<-rbind(RQ1,RQ1_f)

for (r in unique(RQ1$repository)){
  CA<-RQ1$recall[RQ1$threshold=="DV"&RQ1$repository==r]
  CB<-RQ1$recall[RQ1$threshold=="BV"&RQ1$repository==r]
  CC<-RQ1$recall[RQ1$threshold=="AV"&RQ1$repository==r]
  CD<-RQ1$recall[RQ1$threshold=="GV"&RQ1$repository==r]
  CE<-RQ1$recall[RQ1$threshold=="DR"&RQ1$repository==r]
  CF<-RQ1$recall[RQ1$threshold=="BR"&RQ1$repository==r]
  CG<-RQ1$recall[RQ1$threshold=="AR"&RQ1$repository==r]
  CH<-RQ1$recall[RQ1$threshold=="GR"&RQ1$repository==r]
  CI<-RQ1$recall[RQ1$threshold=="↑F1"&RQ1$repository==r]
  CJ<-RQ1$recall[RQ1$threshold=="↑MCC"&RQ1$repository==r]
  CK<-RQ1$recall[RQ1$threshold=="↑Gmn"&RQ1$repository==r]
  CL<-RQ1$recall[RQ1$threshold=="↑Gms"&RQ1$repository==r]
  CM<-RQ1$recall[RQ1$threshold=="vD2h"&RQ1$repository==r]
  
  
  RQ1$PValue[RQ1$threshold=="DV"&RQ1$repository==r]<-1
  RQ1$CliffDelta[RQ1$threshold=="DV"&RQ1$repository==r]<-cliff.delta(CA,CA)$estimate
  RQ1$PValue[RQ1$threshold=="BV"&RQ1$repository==r]<-wilcox.test(CB,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="BV"&RQ1$repository==r]<-cliff.delta(CB,CA)$estimate
  RQ1$PValue[RQ1$threshold=="AV"&RQ1$repository==r]<-wilcox.test(CC,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="AV"&RQ1$repository==r]<-cliff.delta(CC,CA)$estimate
  RQ1$PValue[RQ1$threshold=="GV"&RQ1$repository==r]<-wilcox.test(CD,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="GV"&RQ1$repository==r]<-cliff.delta(CD,CA)$estimate
  RQ1$PValue[RQ1$threshold=="DR"&RQ1$repository==r]<-wilcox.test(CE,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="DR"&RQ1$repository==r]<-cliff.delta(CE,CA)$estimate
  RQ1$PValue[RQ1$threshold=="BR"&RQ1$repository==r]<-wilcox.test(CF,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="BR"&RQ1$repository==r]<-cliff.delta(CF,CA)$estimate
  RQ1$PValue[RQ1$threshold=="AR"&RQ1$repository==r]<-wilcox.test(CG,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="AR"&RQ1$repository==r]<-cliff.delta(CG,CA)$estimate
  RQ1$PValue[RQ1$threshold=="GR"&RQ1$repository==r]<-wilcox.test(CH,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="GR"&RQ1$repository==r]<-cliff.delta(CH,CA)$estimate
  RQ1$PValue[RQ1$threshold=="↑F1"&RQ1$repository==r]<-wilcox.test(CI,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="↑F1"&RQ1$repository==r]<-cliff.delta(CI,CA)$estimate
  RQ1$PValue[RQ1$threshold=="↑MCC"&RQ1$repository==r]<-wilcox.test(CJ,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="↑MCC"&RQ1$repository==r]<-cliff.delta(CJ,CA)$estimate
  RQ1$PValue[RQ1$threshold=="↑Gmn"&RQ1$repository==r]<-wilcox.test(CK,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="↑Gmn"&RQ1$repository==r]<-cliff.delta(CK,CA)$estimate
  RQ1$PValue[RQ1$threshold=="↑Gms"&RQ1$repository==r]<-wilcox.test(CL,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="↑Gms"&RQ1$repository==r]<-cliff.delta(CL,CA)$estimate
  RQ1$PValue[RQ1$threshold=="vD2h"&RQ1$repository==r]<-wilcox.test(CM,CA, paired=TRUE)$p.value
  RQ1$CliffDelta[RQ1$threshold=="vD2h"&RQ1$repository==r]<-cliff.delta(CM,CA)$estimate
}

for(i in(1:length(RQ1$repository))){
  if(RQ1$PValue[i]<0.05&RQ1$CliffDelta[i]>=0){
    RQ1$fill_p[i]="3"
  }
  else if(RQ1$PValue[i]<0.05&RQ1$CliffDelta[i]<0){
    RQ1$fill_p[i]="1"
  }
  else{
    RQ1$fill_p[i]="2"
  }
  
  
  if(abs(RQ1$CliffDelta[i])<0.147){
    RQ1$fill_c[i]="1"
  }
  else if(abs(RQ1$CliffDelta[i])<0.33){
    RQ1$fill_c[i]="2"
  }
  else if(abs(RQ1$CliffDelta[i])<0.474){
    RQ1$fill_c[i]="3"
  }
  else{
    RQ1$fill_c[i]="4"
  }
}

result<-RQ1[,-8][,-8][,-8][,-8][,-8][,-8]
colnames(result)[7] <- 'value'
bar<-rbind(bar,result)


bar<-data.frame(bar,stringsAsFactors=FALSE)
#bar$measure[is.na(bar$measure)]="F1"
bar$repository = factor(bar$repository, levels=c("ALL","AEEEM","NASA","Promise","Rnalytica"))
bar$threshold = factor(bar$threshold, levels=c("DV","BV","AV","GV","DR","BR","AR","GR","↑F1","↑MCC","↑Gmn","↑Gms","vD2h"))
bar$measure = factor(bar$measure, levels=c("Recall","F1","MCC","Gmean","Gmeasure"))
#p<-ggplot(data=RQ1, aes(x=threshold,y=gmeasure,fill=fill_c,colour=fill_p))+geom_boxplot()+ stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "green")+ theme(legend.position = 'none',strip.text = element_text(size=15),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),axis.title  = element_blank() ,axis.text.x = element_text(face="bold",size=16) )+ scale_fill_manual(breaks = c("1", "2", "3", "4"),values=c( "#ffffff", "#00fff9", "#00ffc5", "#00ff85"))+scale_color_manual(breaks = c("1", "2","3"),values=c( "#ff0000","#000000","#0011ff"))+ scale_y_continuous(limits = c(-0.05,0.6))+ facet_wrap(~ repository, scales="free")
bar_RQ1<-bar
p<-ggplot(data=bar_RQ1, aes(x=threshold,y=value,fill=fill_c,colour=fill_p))+
  geom_boxplot()+ 
  stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 1.5, fill = "green")+ 
  theme(text = element_text(family = "serif"),legend.position = 'none',plot.margin=unit(c(1,3,1,1),'lines'),plot.title=element_text(size=20,hjust=0.5),strip.text = element_text(size=15),panel.grid.major =element_line(colour = "grey95",size = 0.2), panel.grid.minor = element_blank(),panel.background = element_blank(),panel.border=element_rect(fill='transparent', color='black'),axis.line = element_line(colour = "black"),axis.title  = element_blank() ,axis.text.x = element_text(face="bold",size=12,angle=60,hjust = 1,vjust = 1), axis.text.y = element_text(face="bold",size=10))+
  
  scale_fill_manual(breaks = c("1", "2", "3", "4"),values=c( "#ffffff", "#E5E7E9", "#BDC3C7", "#797D7F"))+
  scale_color_manual(breaks = c("1", "2","3"),values=c("#ff0000","#000000", "#0011ff"))+ 
  scale_y_continuous()+ 
  facet_grid(measure~ repository, scales="free")

p

# showtext_auto(enable=T)
# font_add("MicrosoftYaHei","/System/Library/Fonts/msyh.ttf")    #微软雅黑字体#
# 
# font_import() # Prepare for this to take several minutes
# loadfonts(device = "win")

ggsave(p, file='RQ1_fig2.pdf', width=12, height=12) # 可以指定大小，如宽为12cm，高为10cm，需要指定保存路径
#ALL_average$gmeasure[ALL_average$dataset=='JURECZKO-pbeans2'&ALL_average$subsample=='ORIGIN'&ALL_average$ensemble=='ORIGIN']

#multiplot(p1, p2, layout = layout)
