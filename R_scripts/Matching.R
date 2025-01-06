source("/Users/mattmettler/Desktop/Proj_morality/R_scripts/Data_cleaning.R")

# requires tidyverse and MatchIt packages


r<-dt%>%
  drop_na()%>%
  filter(rep==1)



rm<-MatchIt::matchit(bgain~age+female+educ1,
        data=r,
        method="nearest",
        estimand="ATT",
        distance = "mahalanobis", 
        ratio = 1,
        exact=~year,
        replace = FALSE)
plot(summary(rm,interactions=TRUE))
rm <- match.data(rm)


rm_y<-MatchIt::matchit(year~age+female+educ1,
                     data=r,
                     method="nearest",
                     estimand="ATT",
                     distance = "mahalanobis", 
                     ratio = 1,
                     exact=~bgain,
                     replace = FALSE)
plot(summary(rm_y,interactions=TRUE))
rm_y <- match.data(rm_y)



a<-list(
  rm%>%
    filter(year==0)%>%
    lm_robust(imm ~ bgain, data = .)%>%
    broom::tidy(.)%>%
    mutate(year="2011"),
  rm%>%
    filter(year==1)%>%
    lm_robust(imm ~ bgain, data = .)%>%
    broom::tidy(.)%>%
    mutate(year="2016"))%>%
    bind_rows(.)%>%
   .[c(2,4),c(1,2,6,7,8,10)]



b<-list(
rm_y%>%
  filter(bgain==1)%>%
    lm_robust(imm ~ year, data = .)%>%
  broom::tidy(.)%>%
  mutate(group="Evangelical"),
rm_y%>%
  filter(bgain==0)%>%
  lm_robust(imm ~ year, data = .)%>%
  broom::tidy(.)%>%
  mutate(group="Non evangelical"))%>%
  bind_rows(.)%>%
  .[c(2,4),c(1,2,6,7,8,10)]
  


f1<-a%>%
  ggplot(aes(x=year,y=estimate))+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low,ymax=conf.high),width=.25)+
  geom_hline(yintercept=0,linetype=2)+
  labs(title = "Estimate:Evangelicals compared to non evangelicals",x="",y="")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_classic()
  
f2<-b%>%
  ggplot(aes(x=group,y=estimate))+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low,ymax=conf.high),width=.25)+
  geom_hline(yintercept=0,linetype=2)+
  labs(title = "Estimate:Increase 2011 to 2016",x="",y="")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_classic()  

ggarrange(f1,f2)
