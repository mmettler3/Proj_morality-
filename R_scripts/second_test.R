source("/Users/mattmettler/Desktop/Proj_morality/R_scripts/Data_cleaning.R")


t2<-rb%>%
  filter(rep==1)%>%
estimatr::lm_robust(imm~educ+age+female+bgain+part_prime+bgain:part_prime,data=.)%>%
  ggpredict(., terms = c("part_prime[Clinton prime,Without Priming,Trump prime]","bgain[0,1]"))%>%
  as_tibble(.)%>%
  mutate(Group=c("Nonevangelical","Evangelical","Nonevangelical","Evangelical","Nonevangelical","Evangelical"))
 
t2%>%
ggplot2::ggplot(aes(x = x, y = predicted,group=Group,color=Group)) +
  #geom_hline(yintercept = 0, linetype = 2) +
  geom_point(aes(shape=Group),position = position_dodge(width = 0.15),size=5)+ 
  geom_line(aes(linetype=Group),position = position_dodge(width = 0.15),size=1)+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),position = position_dodge(width = 0.15),width=.1,size=1) + 
  ylab("")+
  xlab("")+
  ylim(.2,1)+
  theme_bw()


ggsave("fig2.tiff",path="/Users/mattmettler/Desktop/Proj_morality/figures/", width = 6, height = 6, device='tiff', dpi=200)

