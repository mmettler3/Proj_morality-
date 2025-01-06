rm(list=ls())

# requires haven and tidyverse package


dt11<-haven::read_dta("/Users/mattmettler/Desktop/Data/2011-2020_Dataset/2011_PRRI/prri_2011.DTA") # 2011 prri dataset
dt16<-haven::read_dta("/Users/mattmettler/Desktop/Data/2011-2020_Dataset/2016_PRRI/prri_2016.DTA") # 2016 prri dataset




dt11$rep<- ifelse(dt11$partyful<=2,1,0) # rep if party=1 (republican) or party lean=1 (lean republican)
dt11$dem<- ifelse(dt11$partyful>3 & dt11$partyful<6,1,0) # dem if party=2 (democrat) or party lean=2 (lean democrat)
dt11$ind<-ifelse( dt11$rep==0 &   dt11$dem==0 & dt11$party==3,1,0)   # independent if not rep or dem            

dt16$rep<- ifelse(dt16$party==1 | dt16$partyln==1,1,0)
dt16$rep<-ifelse(is.na(dt16$rep),0,dt16$rep)

dt16$dem<- ifelse(dt16$party==2 | dt16$partyln==2,1,0)
dt16$dem<-ifelse(is.na(dt16$dem),0,dt16$dem)

dt16$ind<-ifelse(dt16$rep==0 & dt16$dem==0 & dt16$party==3,1,0)              



dt11$imm<-ifelse(dt11$privpub==1,1,
                 ifelse(dt11$privpub==2,0,
                        ifelse(dt11$privpub==98,.5,NA))) # immoral act 1=yes, .5=depends, 0=no

dt16$imm<-ifelse(dt16$Q8==1,1,
                 ifelse(dt16$Q8==2,0,
                        ifelse(dt16$Q8==3,.5,NA))) # immoral act 1=yes, .5=depends, 0=no




dt11$bgain<-ifelse(is.na(dt11$born),0,
                   ifelse(dt11$born==1,1,0)) #Would you describe yourself as 'born again' or evangelical Christian, or not?



dt16$bgain<-ifelse(is.na(dt16$born),0,
                   ifelse(dt16$born==1,1,0)) #Would you describe yourself as 'born again' or evangelical Christian, or not?
 

dt11$age<-ifelse(dt11$age>13,NA,dt11$age)

dt16$age1<-dt16$age
dt16<-dt16%>%
  mutate(age=case_when(
    age>=18 & age<=20~1,
    age>=21 & age<=24~2,
    age>=25 & age<=29~3,
    age>=30 & age<=34~4,
    age>=35 & age<=39~5,
    age>=40 & age<=44~6,
    age>=45 & age<=49~7,
    age>=50 & age<=54~8,
    age>=55 & age<=59~9,
    age>=60 & age<=64~10,
    age>=65 & age<=69~11,
    age>=70 & age<=74~12,
    age>=75~13,
    TRUE ~ NA_real_)) # change age from numeric to categories to match 2011 data









dt11$female<-ifelse(dt11$sex==2,1,0) # indicator for female
dt16$female<-ifelse(dt16$sex==2,1,0)# indicator for female



# Reconcile education variables to match
# categories are:
# 1 = less than high school diploma
# 2 = high school diploma
# 3 = some college/associates degree/technical degree
# 4 = College degree and beyond


dt11$educ1<-ifelse(dt11$educ<=2,1,
                   ifelse(dt11$educ==3,2,
                          ifelse(dt11$educ==4,3,
                                 ifelse(dt11$educ==5,3,
                                        ifelse(dt11$educ>=6 &dt11$educ<8 ,4,NA)))))                  

dt16$educ1<-ifelse(dt16$educ==1,1,
                   ifelse(dt16$educ==2,2,
                          ifelse(dt16$educ==3,3,
                                 ifelse(dt16$educ==4,4,
                                        ifelse(dt16$educ==6,3,
                                               ifelse(dt16$educ==5,4,NA)))))) 


dt11$year<-0 # before Trump
dt16$year<-1 # during Trump


list<-list(dt11[,c("educ1","age","year","female","rep","ind","dem","bgain","imm")],
           dt16[,c("educ1","age","year","female","rep","ind","dem","bgain","imm")])



dt<-rbind(list[[1]],list[[2]])
rm(list)

dt$bri<-ifelse(dt$rep==1 & dt$bgain==1 |dt$ind==1 & dt$bgain==1,1,0) # republican and independents evangelicals
dt$br<-ifelse(dt$rep==1 & dt$bgain==1 & dt$bgain==1,1,0) # republican evangelicals

dt$did_bri<-dt$year*dt$bri # difference in difference interaction of year and indicator for evangelical republican/independent
dt$did_br<-dt$year*dt$br   # difference in difference interaction of year and indicator for evangelical republican


################################################################################
#data cleaning for part two
df<-haven::read_sav("/Users/mattmettler/Desktop/Data/2011-2020_Dataset/2018_CCES/TM/cces18.sav") # 2018 cces dataset


df$bagain<-ifelse(df$religpew==1 & df$pew_bornagain==1| df$religpew==12 & df$pew_bornagain==1,1,0) # evangelical = protestant or something else


df$rep_ind<-ifelse(df$pid7>3 & df$pid7<8,1,0)
df$rep<-ifelse(df$pid7>4 & df$pid7<8,1,0)


rb<-df%>%
  filter(rep_ind==1)# filter to republican or independent 


rb$imm<-ifelse(rb$UND475==1,1,
               ifelse(rb$UND475==3,.5,
                      ifelse(rb$UND475==2,0,NA)))


rb$no_p<-ifelse(rb$UND475_picker==1,1,0) # experimental indicator-control
rb$pb<-ifelse(rb$UND475_picker==2,1,0)   # experimental indicator-Bill Clinton
rb$pt<-ifelse(rb$UND475_picker==3,1,0)   # experimental indicator-Donald Trump

rb<-rb%>%
mutate(part_prime = factor(UND475_picker, 
                              levels = c("1", "2", "3"),
                              labels = c("Without Priming", "Primed with Bill Clinton", "Primed with Trump")))


