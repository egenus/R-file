library(tidyverse)
library(cowplot)
library(reshape2)

setwd("C:/R/2022/pot")

rawdata<-read_csv("Pot_test_yield.csv") %>% 
  na.omit()

#length()列数取得，ncol()のほうが感覚的にわかりやすいnrow()で行数．dim()で行列数
element_list=colnames(rawdata[3:ncol(rawdata)])


for (z in element_list) {
  
  data<-rawdata %>% 
    select(1,z)
  
  data<-data %>% 
    select("v"=2,everything()) %>% 
    mutate(treat = fct_inorder(treat)) %>% 
    group_by(treat) %>% 
    summarise(mean = mean(v),sd = sd(v))
  
  point<-rawdata %>% 
    select("P"=z,1)
  
  dx <-rawdata %>%
    mutate(treat=fct_inorder(treat)) %>% 
    select(1,z)
  
  dx <- dplyr::select(dx, "fx" = 1,"vx"=2, dplyr::everything())
  
  fx=as.factor(dx$fx)
  vx=as.numeric(dx$vx)
  
  Dunnett <- aov(vx ~ fx) %>% 
    multcomp::glht(linfct = multcomp::mcp(fx = "Dunnett")) %>% 
    summary()
  Dunnett.pvalues <- data.frame(Dunnett$test$pvalues) %>% 
    mutate(asterisk = case_when(
      Dunnett.test.pvalues < 0.001 ~ "***",
      Dunnett.test.pvalues < 0.01 ~ "**",
      Dunnett.test.pvalues < 0.05 ~ "*",
      Dunnett.test.pvalues >= 0.05 ~ " "))
  
  Dunnett.pvalues$asterisk
  dense_rank(Dunnett.pvalues$asterisk) 
  isTRUE(max(dense_rank(Dunnett.pvalues$asterisk)) == 1)
  
  ylim = max(data$mean + data$sd)*1.2  
  
  png_name=paste("pot_test_",z,".png")
  #正規表現削除して使う
  png(gsub(" ","",png_name))
  
  g<-ggplot(data,aes(x=treat, y=mean, fill = treat))+
    geom_bar(stat="identity", width = 0.7,color="black",size=0.5,fill= c("#C0C0C0", "#FF7F50","#FFD700","#00CED1"))+
    geom_errorbar(aes(ymax =mean+ sd, ymin = mean - sd), width=0.2, size=0.5)+
    scale_y_continuous(expand = c(0,0), limits = c(0,ylim))+
    {if  (max(dense_rank(Dunnett.pvalues$asterisk)) >= 2) {
      stat_summary(aes(x=treat,y=(mean+sd)*1.02),geom = 'text', label = c(" ",Dunnett.pvalues$asterisk) ,fun = max,size = 12)}}+
    {if  (max(dense_rank(Dunnett.pvalues$asterisk)) == 1) {
      annotate("text", x = 2.5 ,y = ylim/1.05,  colour = c("#000000"), label = "N.S.", parse = TRUE, size = 8)}}+
    geom_point(data=point,aes(x=treat,y=P),size=6,shape=1)+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0, size = 20),aspect.ratio = 1,legend.position = "none",axis.text = element_text(size = 15,color = "black"))+
    labs(title=png_name)
  
  print(g)
  
  dev.off()
}

