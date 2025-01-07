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


