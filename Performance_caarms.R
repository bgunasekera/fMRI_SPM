library(foreign)
library(plyr)
library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(MuMIn)

df <- read.spss("C:/Users/bjgun/OneDrive - King's College London/CANTOP/Linear_regression/Performance_analysis_CAARMS/CANTOP_Megafile_COPY.sav", to.data.frame=TRUE)
dfc <- read.csv ("C:/Users/bjgun/OneDrive - King's College London/CANTOP/Linear_regression/Performance_analysis_CAARMS/caarms_data.csv")


### CAARMS data ###

caarmsdf <- dfc [,c("Drug", "id", "total_1m_21", "pos_1m_21", "neg_1m_21", "valence")] 
caarmsdf <- caarmsdf[order(caarmsdf$id),]



### RT calculation ### 


RTtotal<- df[which(df$RT >= 100),] %>%
  group_by(Subject.ID, Cue) %>%
  summarise(RT = mean(RT))

RTneutral3<- RTtotal[which(RTtotal$Cue== "Control"),]
RTws3<- RTtotal[which(RTtotal$Cue== "WinSmall"),]  
RTwl3<- RTtotal[which(RTtotal$Cue== "WinBig"),]  
RTal3<- RTtotal[which(RTtotal$Cue== "Lose"),] 

RTnmws<- RTws3$RT - RTneutral3$RT
RTnmwl<- RTwl3$RT - RTneutral3$RT
RTnmal<- RTal3$RT - RTneutral3$RT

RTlist <- do.call(c, list(RTnmws, RTnmwl, RTnmal))
RTnmwall <- as.data.frame(RTlist)

### combine caarms data and rt and regression analysis ###

caarmsdf_RT <- cbind(caarmsdf, RTnmwall)
head (caarmsdf_RT)


RTlmer <- lmer(RTlist~ Drug*total_1m_21  + (id |valence), data=caarmsdf_RT)
summary(RTlmer)
confint(RTlmer,method="Wald")
r.squaredGLMM(RTlmer) 


### False start calculation ### 

FScalc<- df %>%
  group_by(Subject.ID, Cue, FalseStart) %>%
  filter(FalseStart %in% c("legitimate", "false")) %>% #Please not that here where particiants did not respond and entry is "NA"- these were filtered out and excluded from analysis
  dplyr::summarise(mean = n())


FSneutral<- FScalc[which(FScalc$Cue== "Control"),]
FSneutralY<- FSneutral[which(FSneutral$FalseStart== "legitimate"),]
FSneutralN<- FSneutral[which(FSneutral$FalseStart== "false"),]

FSws<- FScalc[which(FScalc$Cue== "WinSmall"),]
FSwsY<- FSws[which(FSws$FalseStart== "legitimate"),]
FSwsN<- FSws[which(FSws$FalseStart== "false"),]
FSwl<- FScalc[which(FScalc$Cue== "WinBig"),]  
FSwlY<- FSwl[which(FSwl$FalseStart== "legitimate"),]
FSwlN<- FSwl[which(FSwl$FalseStart== "false"),]
FSal<- FScalc[which(FScalc$Cue== "Lose"),]  
FSalY<- FSal[which(FSal$FalseStart== "legitimate"),]
FSalN<- FSal[which(FSal$FalseStart== "false"),]

FSPercentneutralN<- FSneutralN$mean/ (FSneutralY$mean + FSneutralN$mean) * 100
FSPercentwsN<- FSwsN$mean/ (FSwsY$mean + FSwsN$mean) * 100
FSPercentwlN<- FSwlN$mean/ (FSwlY$mean + FSwlN$mean) * 100
FSPercentalN<- FSwlN$mean/ (FSwlY$mean + FSwlN$mean) * 100


FSnmws<- FSPercentneutralN - FSPercentwsN
FSnmwl<- FSPercentneutralN - FSPercentwlN
FSnmal<- FSPercentneutralN - FSPercentalN


FSnmwall <- do.call(c, list(FSnmws, FSnmwl, FSnmal))
FSnmwall <- as.data.frame(FSnmwall)


### combine caarms data and false start and regression analysis ###

caarmsdf_FS <- cbind(caarmsdf, FSnmwall) %>%
  na.omit(caarmsdf_FS)
head(caarmsdf_FS)


FSlmer <- lmer(FSnmwall~ Drug*pos_1m_21  + (id |valence), data=caarmsdf_FS)
summary(FSlmer)
confint(FSlmer,method="Wald")
r.squaredGLMM(FSlmer)

### Money calculation ### 

Moneycalc<- df %>%
  group_by(Subject.ID, Cue) %>%
  summarise(Money = (mean(Reward)))


Moneyws<- Moneycalc[which(Moneycalc$Cue== "WinSmall"),]  
Moneywl<- Moneycalc[which(Moneycalc$Cue== "WinBig"),]  
Moneyal<- Moneycalc[which(Moneycalc$Cue== "Lose"),]  

Moneytotal<- (Moneyws$Money + Moneywl$Money + Moneyal$Money) /3

Moneytotal <- as.data.frame(Moneytotal)


### combine caarms data and monetary and regression analysis ###
caarmsdf2 <- dfc [,c("Drug", "id", "total_1m_21", "pos_1m_21", "neg_1m_21", "valence")] 
caarmsdf2 <- caarmsdf[order(caarmsdf$id),]
caarmsdf2 <- caarmsdf[caarmsdf$valence != 2 & caarmsdf$valence != "3", ] 

caarmsdf_money <- cbind(caarmsdf2, Moneytotal)


ggplot(caarmsdf_money, aes(x=total_1m_21, y=Moneytotal, shape= Drug, color=Drug)) +
  geom_point()+
  geom_smooth(method=lm, aes(fill=Drug))+
  theme_light()+
  labs(tag= "B", x = "Total CAARMS score", y = "Total monetary reward", color = "Drug")


summary(m1 <- lm(pos_1m_21~ Drug*Moneytotal, data=caarmsdf_money)) 
confint(m1,method="Wald")
r.squaredGLMM(m1) 


#Individual correlations

ConPLB<- caarmsdf_money[which(caarmsdf_money$Drug == 'Placebo'),]
ConCBD<- caarmsdf_money[which(caarmsdf_money$Drug == 'CBD'),]


summary(m2 <- lm(pos_1m_21~ Moneytotal, data=ConCBD)) 
summary(m3 <- lm(pos_1m_21~ Moneytotal, data=ConPLB)) 











################################## 

#DELETED CODE 
#USES 1 value per subject
#Didnt want to use as wanted to be consistent with caarms x brain signal regressions
#pasted here incase important later






df <- read.spss("C:/Users/bjgun/OneDrive - King's College London/CANTOP/Linear_regression/Performance_analysis_CAARMS/CANTOP_Megafile_COPY.sav", to.data.frame=TRUE)
dfc <- read.csv ("C:/Users/bjgun/OneDrive - King's College London/CANTOP/Linear_regression/Performance_analysis_CAARMS/caarms_data.csv")


### CAARMS data ###

caarmsdf <- dfc [,c("Drug", "id", "total_1m_21", "pos_1m_21", "neg_1m_21", "valence")] 
caarmsdf <- caarmsdf[order(caarmsdf$id),]
caarmsdf <- caarmsdf[caarmsdf$valence != 2 & caarmsdf$valence != "3", ] 


### RT calculation ### 


RTtotal<- df[which(df$RT >= 100),] %>%
  group_by(Subject.ID, Cue) %>%
  summarise(RT = mean(RT))

RTneutral3<- RTtotal[which(RTtotal$Cue== "Control"),]
RTws3<- RTtotal[which(RTtotal$Cue== "WinSmall"),]  
RTwl3<- RTtotal[which(RTtotal$Cue== "WinBig"),]  
RTal3<- RTtotal[which(RTtotal$Cue== "Lose"),] 

RTnmwall<- RTneutral3$RT - ((RTws3$RT + RTwl3$RT + RTal3$RT) /3)

RTnmwall <- as.data.frame(RTnmwall)

### combine caarms data and rt and regression analysis ###

caarmsdf_RT <- cbind(caarmsdf, RTnmwall)


ggplot(caarmsdf_RT, aes(x=RTnmwall, y=total_1m_21, shape= Drug, color=Drug)) +
  geom_point()+
  geom_smooth(method=lm, aes(fill=Drug))+
  theme_light()+
  labs(tag= "A", y = "Total CAARMS score", x = "Reaction time to salient stimuli", color = "Drug")


summary(m <- lm(total_1m_21~ Drug*RTnmwall, data=caarmsdf_RT)) 
# I chose not to use the lmer for this
# See performance analysis con estimate script -- money calc at bottom --where I got the error
# error arrises from 1 observation per id
# there is a link to a replicated problem and solution 
confint(m,method="Wald")
r.squaredGLMM(m) 

### False start calculation ### 

FScalc<- df %>%
  group_by(Subject.ID, Cue, FalseStart) %>%
  filter(FalseStart %in% c("legitimate", "false")) %>% #Please not that here where particiants did not respond and entry is "NA"- these were filtered out and excluded from analysis
  dplyr::summarise(mean = n())


FSneutral<- FScalc[which(FScalc$Cue== "Control"),]
FSneutralY<- FSneutral[which(FSneutral$FalseStart== "legitimate"),]
FSneutralN<- FSneutral[which(FSneutral$FalseStart== "false"),]

FSws<- FScalc[which(FScalc$Cue== "WinSmall"),]
FSwsY<- FSws[which(FSws$FalseStart== "legitimate"),]
FSwsN<- FSws[which(FSws$FalseStart== "false"),]
FSwl<- FScalc[which(FScalc$Cue== "WinBig"),]  
FSwlY<- FSwl[which(FSwl$FalseStart== "legitimate"),]
FSwlN<- FSwl[which(FSwl$FalseStart== "false"),]
FSal<- FScalc[which(FScalc$Cue== "Lose"),]  
FSalY<- FSal[which(FSal$FalseStart== "legitimate"),]
FSalN<- FSal[which(FSal$FalseStart== "false"),]

FSPercentneutralN<- FSneutralN$mean/ (FSneutralY$mean + FSneutralN$mean) * 100
FSPercentwsN<- FSwsN$mean/ (FSwsY$mean + FSwsN$mean) * 100
FSPercentwlN<- FSwlN$mean/ (FSwlY$mean + FSwlN$mean) * 100
FSPercentalN<- FSwlN$mean/ (FSwlY$mean + FSwlN$mean) * 100


FSnmwall<- FSPercentneutralN - ((FSPercentwsN + FSPercentwlN + FSPercentalN) /3)
FSnmwall <- as.data.frame(FSnmwall)


### combine caarms data and false start and regression analysis ###

caarmsdf_FS <- cbind(caarmsdf, FSnmwall) %>%
  na.omit(caarmsdf_FS)


ggplot(caarmsdf_FS, aes(x=total_1m_21, y=FSnmwall, shape= Drug, color=Drug)) +
  geom_point()+
  geom_smooth(method=lm, aes(fill=Drug))+
  theme_light()+
  labs(tag= "D", y = "Total CAARMS score", x = "False starts", color = "Drug")


summary(m <- lm(total_1m_21~ Drug*FSnmwall, data=caarmsdf_FS)) 
confint(m,method="Wald")
r.squaredGLMM(m)  



  