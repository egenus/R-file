library(tidyverse)

setwd("C:/R/2022/field")

rawdata<-read_csv("Fujieda9_28.csv") %>% 
  na.omit()

data_C<-rawdata %>% 
  filter(treat=="Control") %>% 
  group_by(ID) %>% 
  summarise(mean=mean(Stem_length),sd=sd(Stem_length))

data_A<-rawdata %>% 
  filter(treat=="AOH")%>% 
  group_by(ID) %>% 
  summarise(mean=mean(Stem_length),sd=sd(Stem_length))

data<-inner_join(data_C,data_A,by="ID") %>% 
  na.omit()#共通のデータを取ってくる

R<-select(data,1) %>%
  mutate(Relative=data$mean.y/data$mean.x) %>% 
  arrange(Relative) #低い順に並べる

#プロット用
wp<-slice(R,1:10) %>% 
  inner_join(rawdata,w.r,by="ID") %>% 
  mutate(treat=fct_inorder(treat),ID=fct_inorder(ID))
bp<-slice(R,95:104) %>% 
  inner_join(rawdata,b.r,by="ID") %>% 
  mutate(treat=fct_inorder(treat),ID=fct_inorder(ID))

#下位10品種
w.r<-slice(R,1:10) %>% 
  inner_join(rawdata,w.r,by="ID") %>%
  mutate(treat=fct_inorder(treat),ID=fct_inorder(ID)) %>% 
  group_by(treat,ID) %>% 
  summarise(mean=mean(Stem_length),sd=sd(Stem_length))
#上位10品種
b.r<-slice(R,95:104) %>% 
  inner_join(rawdata,b.r,by="ID")%>%
  mutate(treat=fct_inorder(treat),ID=fct_inorder(ID)) %>% 
  group_by(treat,ID) %>% 
  summarise(mean=mean(Stem_length),sd=sd(Stem_length))

ID.w<-w.r$ID
ID.w

p_array <- c()
for (i in ID.w) {
  print(i)
  data.test <- wp %>% 
    filter(ID == i)
  #T検定，結果をp_arrayに収納
  t.res <- t.test( Stem_length~ treat, data = data.test, var.equal=F)
  print(paste("ttest:",t.res$p.value))
  p_array <- append(p_array,as.numeric(t.res$p.value)) #p_arrayに対して新しく計算したp値を代入
  rm(t.res) #変数を初期化
}

#NAを0にする
w.r[is.na(w.r)] <- 0
b.r[is.na(b.r)] <- 0


#下位10品種作図----------------------------------------------------------------------------------
ylim<-max(w.r$mean + w.r$sd)*1.1

pdf("worst10_Fujieda2.pdf")

ggplot(w.r,aes(x=treat,y=mean,fill= treat))+
  geom_bar(stat="identity", width = 0.6,color="black",position = position_dodge(0.6))+
  geom_errorbar(aes(ymax =mean+ sd, ymin = mean - sd), width=0.2, size=0.5,position = position_dodge(0.6))+
  geom_point(data=wp,aes(x=treat,y=Stem_length))+
  scale_y_continuous(expand = c(0,0), limits = c(0,ylim))+
  scale_fill_manual(values = c("#C0C0C0","#FFD700"))+
  #stat_summary(geom = 'text',aes(x=2,y=ylim*0.95), label = p_array3,fun = max, size=4)+
  #stat_summary(geom = 'text',aes(x=2,y=ylim*0.7), label = p_array4,fun = max, size=7)+
  #theme_cowplot(font_size = 24, line_size = 1.25)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0, size = 28),aspect.ratio = 1,legend.position ="none")+
  theme(axis.text.x = element_text(size = 10,color = "black"), axis.text.y = element_text(size = 12,color = "black"),aspect.ratio=1)+
  theme(strip.text.x = element_text(size = 12,color = "black"))+
  facet_wrap(~ID,nrow =2)

dev.off()

#上位10品種作図-----------------------------------------------------------------------------------
ylim<-max(b.r$mean + b.r$sd)*1.1

pdf("best10_Fujieda2.pdf")

ggplot(b.r,aes(x=treat,y=mean,fill= treat))+
  geom_bar(stat="identity", width = 0.6,color="black",position = position_dodge(0.6))+
  geom_errorbar(aes(ymax =mean+ sd, ymin = mean - sd), width=0.2, size=0.5,position = position_dodge(0.6))+
  geom_point(data=bp,aes(x=treat,y=Stem_length))+
  scale_y_continuous(expand = c(0,0), limits = c(0,ylim))+
  scale_fill_manual(values = c("#C0C0C0","#FFD700"))+
  #stat_summary(geom = 'text',aes(x=2,y=ylim*0.95), label = p_array3,fun = max, size=4)+
  #stat_summary(geom = 'text',aes(x=2,y=ylim*0.7), label = p_array4,fun = max, size=7)+
  #theme_cowplot(font_size = 24, line_size = 1.25)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0, size = 28),aspect.ratio = 1,legend.position ="none")+
  theme(axis.text.x = element_text(size = 10,color = "black"), axis.text.y = element_text(size = 12,color = "black"),aspect.ratio=1)+
  theme(strip.text.x = element_text(size = 12,color = "black"))+
  facet_wrap(~ID,nrow =2)

dev.off()

ID.b<-b.r$ID
ID.b

p_array <- c()
for (i in ID.b) {
  print(i)
  data.test <- bp %>% 
    filter(ID == i)
  #T検定，結果をp_arrayに収納
  t.res <- t.test( Stem_length~ treat, data = data.test, var.equal=F)
  print(paste("ttest:",t.res$p.value))
  p_array <- append(p_array,as.numeric(t.res$p.value)) #p_arrayに対して新しく計算したp値を代入
  rm(t.res) #変数を初期化
}
