# Use this script for the Exp 2a Tseltal Dataset
library(dplyr)
library(ggpubr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(emmeans)
library(tidyverse)
library(sjPlot)
library(gridExtra)
theme_set(theme_pubr())

# call in the data
all.data.raw=read.csv(file = 'clean_data/Exp2b_Tseltal_add_clean.csv',header=T)
all.data.raw2=read.csv(file = 'clean_data/Exp2b_Tseltal_affect_clean.csv',header=T)

all.data <- merge(x = all.data.raw, y = all.data.raw2,
                  by = c("ID", "recording", "part")) 

# export the data file as a single csv               
#write.csv(all.data1, "clean_data/all.data1.csv", row.names = FALSE)

# sets variables as factors
all.data$recording<-factor(all.data$recording)
all.data$participant<-factor(all.data$participant)
all.data$participant2<-factor(all.data$participant2)
all.data$ID<-factor(all.data$ID)
all.data$AddresseeT<-factor(all.data$nat_inf_label)
all.data$rater_label<-factor(all.data$rater_label)

# Label accuracy
all.data$accuracy[all.data$rater_label=="ads" &
                    all.data$AddresseeT=="A"] <- 1
all.data$accuracy[all.data$rater_label=="cds" &
                    all.data$AddresseeT=="T"] <- 1
all.data$accuracy[all.data$rater_label=="cds" &
                    all.data$AddresseeT=="C"] <- 1
all.data$accuracy[all.data$rater_label=="cds" &
                    all.data$AddresseeT=="A"] <- 0
all.data$accuracy[all.data$rater_label=="ads" &
                    all.data$AddresseeT=="T"] <- 0
all.data$accuracy[all.data$rater_label=="ads" &
                    all.data$AddresseeT=="C"] <- 0

# collapse C and T in Native Informer label
all.data$Addressee[all.data$AddresseeT=="A"] <- "A"
all.data$Addressee[all.data$AddresseeT=="C"] <- "C"
all.data$Addressee[all.data$AddresseeT=="T"] <- "C"
all.data$Addressee <- factor(c(all.data$Addressee), labels = c("ads","cds"))

#makes numeric Addressee variable (cds= 0 and ads = 1)
all.data$Addressee.num[all.data$Addressee=="ads"] <- 1
all.data$Addressee.num[all.data$Addressee=="cds"] <- 0


# build affect variables 
# happy factor for model
all.data$Happy[all.data$happy=="0"] <- 'Neutral'
all.data$Happy[all.data$happy=="1"] <- 'Little'
all.data$Happy[all.data$happy=="2"] <- 'Some'
all.data$Happy[all.data$happy=="3"] <- 'More'
all.data$Happy[all.data$happy=="4"] <- 'Extremely'
all.data$Happy<-factor(c(all.data$Happy), levels = c("Neutral", "Little","Some","More","Extremely"))
#all.data$Happy<-relevel(all.data$Happy, "Some")

# sooth factor for model
all.data$Sooth[all.data$sooth=="0"] <- 'Neutral'
all.data$Sooth[all.data$sooth=="1"] <- 'Little'
all.data$Sooth[all.data$sooth=="2"] <- 'Some'
all.data$Sooth[all.data$sooth=="3"] <- 'More'
all.data$Sooth[all.data$sooth=="4"] <- 'Extremely'
all.data$Sooth<-factor(c(all.data$Sooth), levels = c("Neutral", "Little","Some","More","Extremely"))
#all.data$Sooth<-relevel(all.data$Sooth, "Some")

# love factor for model
all.data$Love[all.data$love=="0"] <- 'Neutral'
all.data$Love[all.data$love=="1"] <- 'Little'
all.data$Love[all.data$love=="2"] <- 'Some'
all.data$Love[all.data$love=="3"] <- 'More'
all.data$Love[all.data$love=="4"] <- 'Extremely'
all.data$Love<-factor(c(all.data$Love), levels = c("Neutral", "Little","Some","More","Extremely"))
#all.data$Love<-relevel(all.data$Love, "Some")

# Excited factor for model
all.data$Excited[all.data$exaggerate=="0"] <- 'Neutral'
all.data$Excited[all.data$exaggerate=="1"] <- 'Little'
all.data$Excited[all.data$exaggerate=="2"] <- 'Some'
all.data$Excited[all.data$exaggerate=="3"] <- 'More'
all.data$Excited[all.data$exaggerate=="4"] <- 'Extremely'
all.data$Excited<-factor(c(all.data$Excited), levels = c("Neutral", "Little","Some","More","Extremely"))
#all.data$Excited<-relevel(all.data$Excited, "Some")

# confidence variable 
con.count<-ggplot(all.data, aes(confidence.x)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()
con.count

con.count.num <- all.data %>%
  group_by(confidence.x) %>%
  summarise(counts = n())
con.count.num

############demographics and analyses
# Accuracy means by group
mean(all.data$accuracy)
median(all.data$accuracy)
range(all.data$accuracy)
sd(all.data$accuracy)

# C and T are collapsed 
acc.means.TisC<-all.data %>%
  group_by(Addressee) %>%
  summarise_at(vars(accuracy), list(name = mean))
acc.means.TisC

acc.sd.TisC<-all.data %>%
  group_by(Addressee) %>%
  summarise_at(vars(accuracy), list(name = sd))
acc.sd.TisC

acc.count.TisC<-all.data %>%
  group_by(Addressee) %>%
  summarise(counts = n())
acc.count.TisC

con.means<-all.data %>%
  group_by(Addressee) %>%
  summarise_at(vars(confidence.x), list(name = mean))
con.means

# Breaks out C and T
acc.means<-all.data %>%
  group_by(AddresseeT) %>%
  summarise_at(vars(accuracy), list(name = mean))
acc.means

con.means<-all.data %>%
  group_by(AddresseeT) %>%
  summarise_at(vars(confidence.x), list(name = mean))
con.means



#########MODELS
###accuracy model T is C
#accuracy model with positive affect interactions
accuracy2.model<-glmer(accuracy~1 + Addressee +
                         confidence.x +
                         Happy +
                         Sooth +
                         Love +
                         Excited +
                         Addressee*Happy +
                         Addressee*Sooth +
                         Addressee*Love +
                         Addressee*Excited +
                         #(1|participantF)+
                         (1|recording),
                       data = all.data,
                       family = binomial (link = 'logit'))

summary(accuracy2.model)

tab_model(accuracy2.model, show.se = TRUE)


#Happy Contrasts
emmeans_results_happy <- emmeans(accuracy2.model, ~ Addressee*Happy)
#emmeans_results_happy

#pairs(emmeans_results)

#contrast(emmeans_results, "revpairwise", by="nat_inf_labelF",adjust="bonferroni") 

contrast(emmeans_results_happy, "revpairwise", by="Happy",adjust="bonferroni") 

p1<-emmip(accuracy2.model, Addressee ~ Happy, CIs=TRUE, plotit=T)+theme_bw()

#Love Contrasts
emmeans_results_love <- emmeans(accuracy2.model, ~ Addressee*Love)
#emmeans_results_love

#pairs(emmeans_results)

#contrast(emmeans_results, "revpairwise", by="nat_inf_labelF",adjust="bonferroni") 

contrast(emmeans_results_love, "revpairwise", by="Love",adjust="bonferroni") 

p2<-emmip(accuracy2.model, Addressee ~ Love, CIs=TRUE, plotit=T)+theme_bw()

#Sooth Contrasts
emmeans_results_sooth <- emmeans(accuracy2.model, ~ Addressee*Sooth)
#emmeans_results_sooth

#pairs(emmeans_results)

#contrast(emmeans_results, "revpairwise", by="nat_inf_labelF",adjust="bonferroni") 


contrast(emmeans_results_sooth, "revpairwise", by="Sooth",adjust="bonferroni") 

p3<-emmip(accuracy2.model, Addressee ~ Sooth, CIs=TRUE, plotit=T)+theme_bw()

#Excited contrasts
emmeans_results_exag <- emmeans(accuracy2.model, ~ Addressee*Excited)
#emmeans_results_exag

#pairs(emmeans_results)

#contrast(emmeans_results, "revpairwise", by="nat_inf_labelF",adjust="bonferroni") 


contrast(emmeans_results_exag, "revpairwise", by="Excited",adjust="bonferroni") 

p4<-emmip(accuracy2.model, Addressee ~ Excited, CIs=TRUE, plotit=T)+theme_bw()

#puts all the interaction plots together
grid.arrange(p1,p2,p3,p4, nrow = 2)


#exploratory model with addressee with positive affect predictors
addressee.model<-glmer(Addressee.num ~ 1 +
                         #confidence.x +
                         Happy +
                         Sooth +
                         Love +
                         Excited +
                         (1|recording),
                       data = all.data,
                       family = binomial (link = 'logit'))

summary(addressee.model)

tab_model(addressee.model, show.se = TRUE)

#same exploratory model but treats the affect as contagious
addressee.model.con<-glmer(Addressee.num ~ 1 +
                         confidence.x +
                         happy +
                         sooth +
                         love +
                         exaggerate +
                         (1|recording),
                       data = all.data,
                       family = binomial (link = 'logit'))

summary(addressee.model.con)

###accuracy model T is not C
#accuracy model with positive affect interactions
accuracy3.model<-glmer(accuracy~1 + AddresseeT +
                         #(1|participantF)+
                         (1|recording),
                       data = all.data,
                       family = binomial (link = 'logit'))

summary(accuracy3.model)

tab_model(accuracy3.model, show.se = TRUE)


all.data$AddresseeT2<-relevel(all.data$AddresseeT, "T")
accuracy4.model<-glmer(accuracy~1 + AddresseeT2 +
                         #(1|participantF)+
                         (1|recording),
                       data = all.data,
                       family = binomial (link = 'logit'))

summary(accuracy4.model)

tab_model(accuracy4.model, show.se = TRUE)

# bar plots to look at distributions
# sounded happy variable 
happy.count<-ggplot(all.data, aes(happy)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()
happy.count

happy.count.num <- all.data %>%
  group_by(happy) %>%
  summarise(counts = n())
happy.count.num

happy.CI<- all.data %>% 
  group_by(Addressee) %>% 
  summarise(ci = list(mean_cl_normal(happy) %>% 
                        rename(mean=y, lwr=ymin, upr=ymax))) %>% 
  unnest
happy.CI

# sounded soothing
sooth.count<-ggplot(all.data, aes(sooth)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()
sooth.count

sooth.count.num <- all.data %>%
  group_by(sooth) %>%
  summarise(counts = n())

sooth.CI<- all.data %>% 
  group_by(Addressee) %>% 
  summarise(ci = list(mean_cl_normal(sooth) %>% 
                        rename(mean=y, lwr=ymin, upr=ymax))) %>% 
  unnest
sooth.CI

# sounded loving
love.count<-ggplot(all.data, aes(love)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()
love.count

love.count.num <- all.data %>%
  group_by(love) %>%
  summarise(counts = n())
love.count.num

love.CI<- all.data %>% 
  group_by(Addressee) %>% 
  summarise(ci = list(mean_cl_normal(love) %>% 
                        rename(mean=y, lwr=ymin, upr=ymax))) %>% 
  unnest
love.CI

# sounded Excited
Excited.count<-ggplot(all.data, aes(exaggerate)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()
Excited.count

Excited.count.num <- all.data %>%
  group_by(exaggerate) %>%
  summarise(counts = n())
Excited.count.num


con.count<-ggplot(all.data, aes(exaggerate)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()
Excited.count

Excited.count.num <- all.data %>%
  group_by(exaggerate) %>%
  summarise(counts = n())
Excited.count.num

Excited.CI<- all.data %>% 
  group_by(Addressee) %>% 
  summarise(ci = list(mean_cl_normal(exaggerate) %>% 
                        rename(mean=y, lwr=ymin, upr=ymax))) %>% 
  unnest
Excited.CI

###Accuracy bar plot
AccPlot <- data.frame(Addressee = factor(c(1,2,3)),
                      Affect = c( .82, .84, .79),
                      CI=c( .03,.03, .03),
                      Salience = factor(c(1,2,3)))

Acc.Bar<-ggplot(AccPlot, aes(x=Salience, y=Affect))+ 
  geom_bar(stat="identity", position="dodge")+
  geom_bar(stat="identity", colour="black", position="dodge",show.legend=FALSE)+xlab("")+
  ylab("Mean Accuracy \n")+
  geom_hline(yintercept=.5, linetype='dotted', col = 'black')+
  geom_errorbar(aes(ymin=Affect-CI, ymax=Affect+CI),width=.2,position=position_dodge(.9))+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line.x = element_line(color="black", size = .5),
        axis.line.y = element_line(color="black", size = .5))+
  scale_x_discrete(breaks=c("1", "2", "3"), labels=c("Overall", "CDS","ADS")) 

Acc.Bar+coord_cartesian(ylim=c(0, 1))+scale_fill_manual(values=c("#808080"),guide = guide_legend(title = ""),labels=c("cds", "ads"))+
  scale_y_continuous(breaks=c(0,.1,.2,.3,.4, .5, .6, .7, .8, .9, 1.0), expand = c(0,0))


##Affect bar plots
AffectPlot <- data.frame(Addressee = factor(c(1,2,1,2,1,2,1,2)),
                         Affect = c( 2.01, 2.06, 2.13, 2.12, 1.97, 1.93, 2.13, 2.12),
                         CI=c( .05,.09, .05, .09, .05, .09, .05, .09),
                         Salience = factor(c(1,1,2,2,3,3,4,4)))

Affect.Bar<-ggplot(AffectPlot, aes(x=Salience, y=Affect, fill=Addressee))+ 
  geom_bar(aes(fill=Addressee),stat="identity", position="dodge")+
  geom_bar(aes(fill=Addressee),stat="identity", colour="black", position="dodge",show.legend=FALSE)+xlab("")+
  ylab("Average Emotional Rating \n")+
  geom_errorbar(aes(ymin=Affect-CI, ymax=Affect+CI),width=.2,position=position_dodge(.9))+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line.x = element_line(color="black", size = .5),
        axis.line.y = element_line(color="black", size = .5))+
  scale_x_discrete(breaks=c("1", "2", "3", "4"), labels=c("Happy", "Love","Sooth", "Excited")) 

Affect.Bar+coord_cartesian(ylim=c(0, 4))+scale_fill_manual(values=c("#696969", "#808080"),guide = guide_legend(title = ""),labels=c("cds", "ads"))+
  scale_y_continuous(breaks=c(0,.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0), expand = c(0,0))









##############################STOP!!!!
###########Old Stuff
###accuracy model
accuracy.model<-glmer(accuracy~1+nat_inf_labelF+
                        confidence +
                        happyF +
                        soothF +
                        loveF +
                        exaggeratedF +
                        (1|participantF)+(1|recorded_childF),
                      data = all.data,
                      family = binomial (link = 'logit'))

summary(accuracy.model)

###accuracy model cds
all.data.cds<-subset(all.data, nat_inf_labelF == 'I')
accuracy.model.cds<-glmer(accuracy~confidence +
                            happyF +
                            angryF +
                            sadF +
                            soothF +
                            loveF +
                            exaggeratedF +
                            #(1|participantF)+
                            (1|recorded_childF),
                          data = all.data.cds,
                          family = binomial (link = 'logit'))

summary(accuracy.model.cds)

# cds affect means
happy.means.cds<-all.data.cds %>%
  group_by(happyF) %>%
  summarise_at(vars(accuracy), list(name = mean))
happy.means.cds

angry.means.cds<-all.data.cds %>%
  group_by(angryF) %>%
  summarise_at(vars(accuracy), list(name = mean))
angry.means.cds

sad.means.cds<-all.data.cds %>%
  group_by(sadF) %>%
  summarise_at(vars(accuracy), list(name = mean))
sad.means.cds

sooth.means.cds<-all.data.cds %>%
  group_by(soothF) %>%
  summarise_at(vars(accuracy), list(name = mean))
sooth.means.cds

love.means.cds<-all.data.cds %>%
  group_by(loveF) %>%
  summarise_at(vars(accuracy), list(name = mean))
love.means.cds

exaggerated.means.cds<-all.data.cds %>%
  group_by(exaggeratedF) %>%
  summarise_at(vars(accuracy), list(name = mean))
exaggerated.means.cds

###accuracy model ads
all.data.ads<-subset(all.data, nat_inf_labelF == 'A')
accuracy.model.ads<-glmer(accuracy~1+
                            confidence +
                            happyF +
                            angryF +
                            sadF +
                            soothF +
                            loveF +
                            exaggeratedF +
                            (1|participantF)+(1|recorded_childF),
                          data = all.data.ads,
                          family = binomial (link = 'logit'))

summary(accuracy.model.ads)

happy.means.ads<-all.data.ads %>%
  group_by(happyF) %>%
  summarise_at(vars(accuracy), list(name = mean))
happy.means.ads

angry.means.ads<-all.data.ads %>%
  group_by(angryF) %>%
  summarise_at(vars(accuracy), list(name = mean))
angry.means.ads

sad.means.ads<-all.data.ads %>%
  group_by(sadF) %>%
  summarise_at(vars(accuracy), list(name = mean))
sad.means.ads

sooth.means.ads<-all.data.ads %>%
  group_by(soothF) %>%
  summarise_at(vars(accuracy), list(name = mean))
sooth.means.ads

love.means.ads<-all.data.ads %>%
  group_by(loveF) %>%
  summarise_at(vars(accuracy), list(name = mean))
love.means.ads

exaggerated.means.ads<-all.data.ads %>%
  group_by(exaggeratedF) %>%
  summarise_at(vars(accuracy), list(name = mean))
exaggerated.means.ads

# interactions tons of convergence issues regardless
# of which interactions remain in the model
accuracy2.model<-glmer(accuracy~1+nat_inf_labelF+
                         confidence +
                         happyF +
                         angryF +
                         sadF +
                         soothF +
                         loveF +
                         exaggeratedF +
                         #nat_inf_labelF*confidence +
                         AddresseeT*happyF +
                         #AddresseeT*sadF +
                         AddresseeT*soothF +
                         AddresseeT*loveF +
                         AddresseeT*exaggeratedF +
                         (1|participantF)+(1|recorded_childF),
                       data = all.data,
                       family = binomial (link = 'logit'))

summary(accuracy2.model)


accuracyz.model<-glmer(accuracy~1 + Addressee +
                         confidence.x +
                         happy +
                         sooth +
                         love +
                         exaggerate +
                         Addressee*happy +
                         Addressee*sooth +
                         Addressee*love +
                         Addressee*exaggerate +
                         #(1|participantF)+
                         (1|recording),
                       data = all.data,
                       family = binomial (link = 'logit'))

summary(accuracyz.model)

emmip(accuracyz.model, happy ~ Addressee, CIs=TRUE, plotit=T)+theme_bw()

