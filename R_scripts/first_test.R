source("/Users/mattmettler/Desktop/Proj_morality/R_scripts/Data_cleaning.R")


library(tidyverse)
library(stargazer)
library(psych)
library(estimatr)
library(marginaleffects)
dt1<-dt%>%
  filter(rep==1)
  



ddf<-dt1%>%
  lm(imm~age+female+educ1+year+bri+did_bri,data=.)


ddy<-rm%>%
  lm(imm~age+female+educ1+year+bri+did_bri,data=.)

dd<-rm1%>%
lm(imm~age+female+educ1+year+bri+did_bri,data=.)





stargazer(ddf,ddy,
          type="latex",
          style="ajps",
          digits=2,
          float=FALSE,
          model.numbers = FALSE,
          column.sep.width = "3pt",
          se = starprep(ddf,ddy,stat="std.error", se_type = "HC2"),
          dep.var.labels = c("","",""),
          dep.var.caption=c(""),
          column.labels = c("Full","Matched "),
          covariate.labels = c("Age","Female","Education","Year","Evangelical","Year x Evangelical"))


            
  



fig1<-marginaleffects::predictions(
  ddy,
  newdata=datagrid(year=c(0,1),bri=c(0,1),did_bri=c(0,1)),
  vcov = TRUE,
  conf_level = 0.95)%>%
.[c(1,3,5,8),c(2,7,8,12,13,14,15)]%>%
  dplyr::mutate(Group=ifelse(bri==0,"Non-evangelical","Evangelical"),Year=ifelse(year==0,"2011","2016"))%>%
  ggplot2::ggplot(aes(x = Year, y = estimate,color=Group, group = Group)) +
  #geom_hline(yintercept = 0, linetype = 2) +
  geom_point(position = position_dodge(width = 0.15),size=3)+ 
  geom_line(aes(linetype=Group),position = position_dodge(width = 0.15),size=1)+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),position = position_dodge(width = 0.15),width=.1,size=1) + 
  ylab("")+
  xlab("")+
  ylim(.2,1)+
  theme_bw()
  # scale_x_discrete(labels = c("")

ggsave("fig1.tiff",path="/Users/mattmettler/Desktop/Proj_morality/figures/", width = 6, height = 6, device='tiff', dpi=200)




