source("/Users/mattmettler/Desktop/Proj_morality/R_scripts/Data_cleaning.R")

# requires tidyverse and MatchIt packages
library(psych)
library(MatchIt)
r<-dt%>%
  drop_na()%>%
  filter(rep==1)%>%
  mutate(Education=educ1,Year=year,Age=age,Female=female,Evangelical=bgain)




rm<-MatchIt::matchit(bgain~Age+Female+Education,
                     data=r,
                     method="nearest",
                     estimand="ATT",
                     distance = "mahalanobis", 
                     ratio = 1,
                     exact=~Year,
                     replace = FALSE)
plot(summary(rm,interactions=TRUE))
rm <- match.data(rm)

x<-MatchIt::matchit(bgain~age+female+Education+year,
                 data=rm,
                 method=NULL,
                 ratio = 1,
                 replace = FALSE)
summary(x,interactions=TRUE)


rm1<-MatchIt::matchit(year~age+female+Education,
        data=rm,
        method="nearest",
        estimand="ATT",
        distance = "mahalanobis", 
        ratio = 1,
        exact=~bgain,
        replace = FALSE)
plot(summary(rm1,interactions=TRUE))
rm1 <- match.data(rm1,weights = "weights1",subclass = "subclass1")

x<-MatchIt::matchit(bgain~age+female+educ1,
                    data=rm1,
                    method=NULL,
                    ratio = 1,
                    replace = FALSE)
summary(x,interactions=TRUE)




r%>%
  filter(year==0& bgain==0)%>%
   dplyr::select(imm,age,female,educ1)%>%
    psych::describe(.,fast=TRUE)  %>%
      .[,c(2,3,4,5,6,7)]%>%
  `rownames<-`(c("Immorality Tolerance","Age","Female","Education"))%>%
  kable(.,digits = 2,
        format="latex",
        booktabs=TRUE,
        linesep = "",
        caption="2011 nonevangelicals")%>%
  kable_styling(latex_options = c("striped","hold_position"))


r%>%
  filter(year==1& bgain==0)%>%
  dplyr::select(imm,age,female,educ1)%>%
  psych::describe(.,fast=TRUE) %>%
  .[,c(2,3,4,5,6,7)]%>%
  `rownames<-`(c("Immorality Tolerance","Age","Female","Education"))%>%
  kable(.,digits = 2,
        format="latex",
        booktabs=TRUE,
        linesep = "",
        caption="2016 nonevangelicals")%>%
  kable_styling(latex_options = c("striped","hold_position")) 

r%>%
  filter(year==0& bgain==1)%>%
  dplyr::select(imm,age,female,educ1)%>%
  psych::describe(.,fast=TRUE) %>%
  .[,c(2,3,4,5,6,7)]%>%
  `rownames<-`(c("Immorality Tolerance","Age","Female","Education"))%>%
  kable(.,digits = 2,
        format="latex",
        booktabs=TRUE,
        linesep = "",
        caption="2011 evangelicals")%>%
  kable_styling(latex_options = c("striped","hold_position")) 



r%>%
  filter(year==1& bgain==1)%>%
  dplyr::select(imm,age,female,educ1)%>%
  psych::describe(.,fast=TRUE) %>%
  .[,c(2,3,4,5,6,7)]%>%
  `rownames<-`(c("Immorality Tolerance","Age","Female","Education"))%>%
  kable(.,digits = 2,
        format="latex",
        booktabs=TRUE,
        linesep = "",
        caption="2016 evangelicals")%>%
  kable_styling(latex_options = c("striped","hold_position"))









