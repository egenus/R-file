library(tidyverse)

setwd("C:/R/random")

rawdata<-read.csv("大麦ミネラル.csv")

rawdata<-rawdata %>% 
  pivot_longer(3:15,names_to = "element",values_to = "value")

line<-rawdata %>% 
  mutate(Exp.=fct_inorder(Exp.)) %>% 
  pivot_wider(names_from = n,values_from = value) %>% 
  rename("x"="1","y"="2")

line1<-line%>% 
  mutate(hikizan=line$x-line$y) %>% 
  select(1,2,5) %>% 
  na.omit()
line1$hikizan<-abs(line1$hikizan)

line1$hikizan<-line1$hikizan/2

line2<-line %>% 
  mutate(Exp.=fct_inorder(Exp.)) %>% 
  mutate(hikizan=abs(line$x-line$y)) %>% 
  select(1,2,5) %>%
  filter(Exp.=="キラリモチ")

line<-rbind(line1,line2) %>% 
  select(3)

data<-rawdata%>% 
  mutate(element=fct_inorder(element)) %>%
  mutate(Exp.=fct_inorder(Exp.)) %>% 
  group_by(Exp.,element) %>% 
  summarise(mean=mean(value)) 

data<-cbind(data,line) 
  
 
pdf("element.pdf",family="Japan1GothicBBB")

ggplot(data,aes(x=Exp.,y=mean))+
  geom_bar(stat="identity", width = 0.5,size=0.5,color="black",fill="#FFD700")+
  geom_errorbar(aes(ymax=mean+hikizan , ymin = mean - hikizan), width=0.1, size=0.1)+
  geom_point(data=rawdata,aes(x=Exp.,y=value),size=0.8,shape=1)+
  theme_bw()+
  theme(axis.text = element_text(color = "black"),axis.text.x = element_text(size=4,angle = 90,vjust = 0.5),aspect.ratio=1)+
  facet_wrap(~element ,scales = "free")

dev.off()  


