library(tidyverse)
library(cowplot)

setwd("C:/R/Experiment_TA")

rawdata<-read_csv("komatsuna_ionome_2.csv") %>% 
  na.omit()


element_list=colnames(rawdata[2:length(rawdata)])



for (z in element_list) {
  
  data<- rawdata
  
  point<-rawdata %>% 
    select("Exp"=1,"value"=z) %>% 
    group_by(Exp)
  
  data<-data %>%  
    select(1,z) 
  
  data<-select(data,"variety" = 2,everything()) %>% 
    mutate(Exp = fct_inorder(Exp)) %>% 
    group_by(Exp) %>% 
    summarise(mean = mean(variety),sd = sd(variety))
  
  dx <-rawdata %>%
    mutate(Exp = fct_inorder(Exp))%>% 
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
      Dunnett.test.pvalues >= 0.05 ~ " ")) # 5%以上なら
  Dunnett.pvalues
  # Dunnettの検定結果
  Dunnett.pvalues$asterisk
  dense_rank(Dunnett.pvalues$asterisk) # 要素ごとに1, 2, 3...をつける
  isTRUE(max(dense_rank(Dunnett.pvalues$asterisk)) == 1) # TRUEなら全部N.S.
  
  ylim = max(data$mean + data$sd)*1.3
  
  png_name=paste("ion_",z,".png")
  #正規表現削除して使う
  png(gsub(" ","",png_name))
  
  
  #aesで使用するデータと位置を指定，ggplot()内でx,yを指定すると省略可能
  g<-ggplot(data,aes(x=Exp, y=mean))+
    geom_bar(stat="identity", width = 0.7,color="black")+
    {if  (max(dense_rank(Dunnett.pvalues$asterisk)) >= 2) {stat_summary(aes(y=mean*1.15),geom = 'text', label = c(" ",Dunnett.pvalues$asterisk) ,fun = max, vjust = -1,size = 5)}}+
    {if  (max(dense_rank(Dunnett.pvalues$asterisk)) == 1) {annotate("text", x = 8 ,y = ylim/1.05,  colour = c("#000000"), label = "N.S.", parse = TRUE, size = 6)}}+
    geom_errorbar(aes(ymax =mean+ sd, ymin = mean - sd), width=0.5, size=1)+
    geom_jitter(data = point,aes(x=Exp,y=value),stat ="identity",size=2,pch=1,position=position_jitter(0))+
    scale_y_continuous(expand = c(0,0), limits = c(0,ylim))+
    theme_cowplot(font_size = 24, line_size = 1.25)+
    theme(plot.title = element_text(hjust = 0, size = 28),legend.position = "none")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5), axis.text.y = element_text(size = 18),aspect.ratio=1/3)
  
  print(g)
  
  
  
  dev.off()  
}

#Error: Discrete value supplied to continuous scale   scale_y_continuousここがおかしい
 