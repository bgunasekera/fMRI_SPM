library(foreign)
library(plyr)
library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(MuMIn)





df <- read.spss("C:/Users/bjgun/OneDrive - King's College London/CANTOP/Linear_regression/Performance_analysis_con_estimates/CANTOP_Megafile_COPY.sav", to.data.frame=TRUE)
df2 <- read.csv ("C:/Users/bjgun/OneDrive - King's College London/CANTOP/Linear_regression/Performance_analysis_con_estimates/beta_caarms_data_COPY.csv")


### brain data ###

braindf <- df2 [,c("Drug", "id", "pos_1m_21", "neg_1m_21", "valence", "L_insula", "R_insula")] 
braindf<- braindf[order(braindf$id),] 
braindf<- braindf[order(braindf$valence),]



### RT calculation ###
RTcalc2<- df[which(df$RT >= 100),] %>%
  group_by(Subject.ID, Cue) %>%
  summarise(RT = mean(RT))

RTneutral2<- RTcalc2[which(RTcalc2$Cue== "Control"),]
RTws<- RTcalc2[which(RTcalc2$Cue== "WinSmall"),]  
RTwl<- RTcalc2[which(RTcalc2$Cue== "WinBig"),]  
RTal<- RTcalc2[which(RTcalc2$Cue== "Lose"),]  

RTnmws<- RTws$RT - RTneutral2$RT
RTnmwl<- RTwl$RT - RTneutral2$RT
RTnmal<- RTal$RT - RTneutral2$RT

RTnmws
RTnmwl
RTnmal

RTlist <- do.call(c, list(RTnmws, RTnmwl, RTnmal))
RTlist <- as.data.frame(RTlist)


### combine brain data and rt and regression analysis ###

braindf_RT <- cbind(braindf, RTlist)

A <- ggplot(braindf_RT, aes(x=L_insula, y=RTlist, shape= Drug, color=Drug)) +
  geom_point()+
  geom_smooth(method=lm, aes(fill=Drug))+
  theme_light()+
  labs(x = "Contrast estimates from Left insula ROI", y = "Reaction time difference to valenced stimuli", color = "Drug")

RTlmer <- lmer(RTlist~ Drug*L_insula + (id |valence), data=braindf_RT)
summary(RTlmer)
confint(RTlmer,method="Wald")
r.squaredGLMM(RTlmer)

RTlmer <- lmer(RTlist~ Drug*R_insula + (id |valence), data=braindf_RT)
summary(RTlmer)
confint(RTlmer,method="Wald")
r.squaredGLMM(RTlmer)

D <- ggplot(braindf_RT, aes(x=R_insula, y=RTlist, shape= Drug, color=Drug)) +
  geom_point()+
  geom_smooth(method=lm, aes(fill=Drug))+
  theme_light()+
  labs(x = "Contrast estimates from Right insula ROI", y = "Reaction time difference to valenced stimuli", color = "Drug")



#RT outlier analysis (Cooks D)
cooksdRT <- cooks.distance(RTlmer)

sample_size <- nrow(braindf_RT)
plot(cooksdRT, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksdRT)+1, y=cooksdRT, labels=ifelse(cooksdRT>4/sample_size, names(cooksdRT),""), col="red")  # add labels

# Removing Outliers
# influential row numbers
influential <- as.numeric(names(cooksdRT)[(cooksdRT > (4/sample_size))])

braindf_RT_nooutliers <- braindf_RT[-influential, ]

#Plot and lmer RT no outliers
ggplot(braindf_RT_nooutliers, aes(x=L_insula, y=RTlist, shape= Drug, color=Drug)) +
  geom_point()+
  geom_smooth(method=lm, aes(fill=Drug))+
  theme_light()+
  labs(x = "Contrast estimates from insula ROI", y = "Reaction time to salient stimuli", color = "Drug")

RTlmer_nooutliers <- lmer(RTlist~ Drug*L_insula + (id |valence), data=braindf_RT_nooutliers)
summary(RTlmer_nooutliers)
confint(RTlmer_nooutliers,method="Wald")
r.squaredGLMM(RTlmer_nooutliers)

#Individual RT correlations
ConPLB<- braindf_RT_nooutliers[which(braindf_RT_nooutliers$Drug == 'Placebo'),]
ConCBD<- braindf_RT_nooutliers[which(braindf_RT_nooutliers$Drug == 'CBD'),]

cbdcorr <- lmer(RTlist~ L_insula + (id |valence), data=ConCBD)
summary(cbdcorr)
confint(cbdcorr,method="Wald")

plbcorr <- lmer(RTlist~ L_insula + (id |valence), data=ConPLB)
summary(plbcorr)
confint(plbcorr,method="Wald")



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


FSlist <- do.call(c, list(FSnmws, FSnmwl, FSnmal))
FSlist <- as.data.frame(FSlist)


### combine brain data and accuracy and regression analysis ###

braindf_FS <- cbind(braindf, FSlist) %>%
  na.omit(braindf_FS)


C<- ggplot(braindf_FS, aes(x=L_insula, y=FSlist, shape= Drug, color=Drug)) +
  geom_point()+
  geom_smooth(method=lm, aes(fill=Drug))+
  theme_light()+
  labs(x = "Contrast estimates from Left insula ROI", y = "False starts to valenced stimuli", color = "Drug")

FSlmer <- lmer(FSlist~ Drug*L_insula + (id |valence), data=braindf_FS)
summary(FSlmer)
confint(FSlmer,method="Wald")
r.squaredGLMM(FSlmer)

FSlmer <- lmer(FSlist~ Drug*R_insula + (id |valence), data=braindf_FS)
summary(FSlmer)
confint(FSlmer,method="Wald")
r.squaredGLMM(FSlmer)



#FS outlier analysis (Cooks D)
cooksdFS <- cooks.distance(FSlmer)

sample_size <- nrow(braindf_FS)
plot(cooksdFS, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksdFS)+1, y=cooksdFS, labels=ifelse(cooksdFS>4/sample_size, names(cooksdFS),""), col="red")  # add labels


# Removing Outliers
# influential row numbers
influential <- as.numeric(names(cooksdFS)[(cooksdFS > (4/sample_size))])

braindf_FS_nooutliers <- braindf_FS[-influential, ]

FSlmer_nooutliers <- lmer(FSlist~ Drug*L_insula + (id |valence), data=braindf_FS_nooutliers)
summary(FSlmer_nooutliers)
confint(FSlmer_nooutliers,method="Wald")
r.squaredGLMM(FSlmer_nooutliers)





################################### 
#NEW MONEY CALC BUT UNSURE IF CORRECT
#CALCULATED mean OF MONETRAY GAINS WITH mean OF CON ESTIMATES
#GAVE 1 OBSERVATION PER ID THUS CANNOT USE LMER IN STANDARD WAY
#SEE HERE 
#https://stackoverflow.com/questions/19713228/lmer-error-grouping-factor-must-be-number-of-observations 


Moneycalc<- df %>%
  group_by(Subject.ID, Cue) %>%
  summarise(Money = (mean(Reward)))


Moneyws<- Moneycalc[which(Moneycalc$Cue== "WinSmall"),]  
Moneywl<- Moneycalc[which(Moneycalc$Cue== "WinBig"),]  
Moneyal<- Moneycalc[which(Moneycalc$Cue== "Lose"),]  

Moneytotal<- (Moneyws$Money + Moneywl$Money + Moneyal$Money) /3 

Moneytotal

Moneytotal <- as.data.frame(Moneytotal)


### divide brain data into a single valence per subject per drug ###

brainval<- braindf %>%
  group_by(id, Drug) %>%
  summarise(L_insula = (mean(L_insula)))

brainval_R<- braindf %>%
  group_by(id, Drug) %>%
  summarise(R_insula = (mean(R_insula)))


### combine brain data and money and regression analysis ###

braindf_Money <- cbind(brainval, Moneytotal, brainval_R) %>%
  na.omit(braindf_Money)


braindf_Money<- braindf_Money %>%
  rename(
    id = id...1,
    Drug = Drug...2
  )

B<- ggplot(braindf_Money, aes(x=L_insula, y=Moneytotal, shape= Drug, color=Drug)) +
  geom_point()+
  geom_smooth(method=lm, aes(fill=Drug))+
  theme_light()+
  labs(x = "Contrast estimates from Left insula ROI", y = "Monetary reward to salient stimuli", color = "Drug")


summary(m <- lm(Moneytotal~ Drug*R_insula, data=braindf_Money)) 
confint(m,method="Wald")
r.squaredGLMM(m) 



#Money outlier analysis (Cooks D)
cooksdM <- cooks.distance(m)

sample_size <- nrow(braindf_Money)
plot(cooksdM, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksdM)+1, y=cooksdM, labels=ifelse(cooksdM>4/sample_size, names(cooksdM),""), col="red")  # add labels

# Removing Outliers
# influential row numbers
influential <- as.numeric(names(cooksdM)[(cooksdM > (4/sample_size))])

braindf_M_nooutliers <- braindf_Money[-influential, ]


#Plot and lmer RT no outliers
ggplot(braindf_M_nooutliers, aes(x=L_insula, y=Moneytotal, shape= Drug, color=Drug)) +
  geom_point()+
  geom_smooth(method=lm, aes(fill=Drug))+
  theme_light()+
  labs(tag= "A", x = "Contrast estimates from insula ROI", y = "Monetary reward to salient stimuli", color = "Drug")

summary(m_nooutliers <- lm(Moneytotal~ Drug*L_insula, data=braindf_M_nooutliers)) 
confint(m_nooutliers,method="Wald")
r.squaredGLMM(m_nooutliers) 

library (ggplot2)
library (ggpubr)
theme_set(theme_pubr())

### Figure 

figure <- ggarrange(A, B, C, D,
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow =2,
                    common.legend = TRUE, legend = "bottom"
)

figure

