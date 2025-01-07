source("/Users/mattmettler/Desktop/Proj_morality/R_scripts/Data_cleaning.R")


# requires estimatr, marginaleffects,and tidyverse packages
# using rm dataset from mtaching to run diff in diff
dd<-estimatr::lm_robust(imm~age+female+educ1+year+br+did_br,data=rm)



marginaleffects::predictions(
  dd,
  newdata=datagrid(year=c(0,1),br=c(0,1),did_br=c(0,1)),
  vcov = TRUE,
  conf_level = 0.95)%>%
.[c(1,3,5,8),c(2,7,8,13,14,15)]%>%
  dplyr::mutate(Group=ifelse(br==0,"Non-evangelical","Evangelical"),Year=ifelse(year==0,"2011","2016"))%>%
  ggplot2::ggplot(aes(x = Year, y = estimate,color=Group, group = Group)) +
  #geom_hline(yintercept = 0, linetype = 2) +
  geom_point(position = position_dodge(width = 0.15),size=3)+ 
  geom_line(aes(linetype=Group),position = position_dodge(width = 0.15),size=1)+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),position = position_dodge(width = 0.15),width=.1,size=1) + 
  ylab("")+
  xlab("")+
  ylim(.2,1)
  # scale_x_discrete(labels = c("")

#ggsave()
