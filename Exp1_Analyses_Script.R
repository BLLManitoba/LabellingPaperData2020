# Use this script for the Farci Dataset
library(dplyr)
library(ggpubr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(emmeans)
library(sjPlot)
library(gridExtra)
theme_set(theme_pubr())

# call in the data
all.data.raw=read.csv(file = 'clean_data/Farci_Clean.csv',header=T)
all.data<-subset(all.data.raw, participant != 'SUB15')

# sets variables as factors
all.data$recorded_childF<-factor(all.data$recorded_child)
all.data$participantID<-factor(all.data$participant)
all.data$Addressee<-factor(c(all.data$nat_inf_labeln), labels = c("cds","ads"))
all.data$Addressee<-relevel(all.data$Addressee, "ads")

# Label accuracy
all.data$accuracy[all.data$rater_labeln=="1" &
                    all.data$nat_inf_labeln=="1"] <- 1
all.data$accuracy[all.data$rater_labeln=="0" &
                    all.data$nat_inf_labeln=="0"] <- 1
all.data$accuracy[all.data$rater_labeln=="1" &
                    all.data$nat_inf_labeln=="0"] <- 0
all.data$accuracy[all.data$rater_labeln=="0" &
                    all.data$nat_inf_labeln=="1"] <- 0

# constant to compare to chance
all.data$constant <- 1

# build affect variables
# happy factor for model
all.data$Happy[all.data$happy=="0"] <- 'Extremely Not'
all.data$Happy[all.data$happy=="1"] <- 'Somewhat Not'
all.data$Happy[all.data$happy=="2"] <- 'Neutral'
all.data$Happy[all.data$happy=="3"] <- 'Somewhat'
all.data$Happy[all.data$happy=="4"] <- 'Extremely'
all.data$Happy<-factor(c(all.data$Happy), levels = c("Neutral", "Extremely Not", "Somewhat Not","Somewhat","Extremely"))
#all.data$Happy<-relevel(all.data$Happy, "Neutral", "Extremely Not", "Somewhat Not", "Somewhat", "Extremely")

# angry factor for model
all.data$Angry[all.data$angry=="0"] <- 'Extremely Not'
all.data$Angry[all.data$angry=="1"] <- 'Somewhat Not'
all.data$Angry[all.data$angry=="2"] <- 'Neutral'
all.data$Angry[all.data$angry=="3"] <- 'Somewhat'
all.data$Angry[all.data$angry=="4"] <- 'Extremely'
all.data$Angry<-factor(c(all.data$Angry), levels = c("Neutral", "Extremely Not", "Somewhat Not","Somewhat","Extremely"))
#all.data$Angry<-relevel(all.data$Angry, "Neutral")

# sad factor for model
all.data$Sad[all.data$sad=="0"] <- 'Extremely Not'
all.data$Sad[all.data$sad=="1"] <- 'Somewhat Not'
all.data$Sad[all.data$sad=="2"] <- 'Neutral'
all.data$Sad[all.data$sad=="3"] <- 'Somewhat'
all.data$Sad[all.data$sad=="4"] <- 'Extremely'
all.data$Sad<-factor(c(all.data$Sad), levels = c("Neutral", "Extremely Not", "Somewhat Not","Somewhat","Extremely"))
#all.data$Sad<-relevel(all.data$Sad, "Neutral")

sooth.count.num
# sooth factor for model
all.data$Sooth[all.data$sooth=="0"] <- 'Extremely Not'
all.data$Sooth[all.data$sooth=="1"] <- 'Somewhat Not'
all.data$Sooth[all.data$sooth=="2"] <- 'Neutral'
all.data$Sooth[all.data$sooth=="3"] <- 'Somewhat'
all.data$Sooth[all.data$sooth=="4"] <- 'Extremely'
all.data$Sooth<-factor(c(all.data$Sooth), levels = c("Neutral", "Extremely Not", "Somewhat Not","Somewhat","Extremely"))
#all.data$Sooth<-relevel(all.data$Sooth, "Neutral")

# love factor for model
all.data$Love[all.data$love=="0"] <- 'Extremely Not'
all.data$Love[all.data$love=="1"] <- 'Somewhat Not'
all.data$Love[all.data$love=="2"] <- 'Neutral'
all.data$Love[all.data$love=="3"] <- 'Somewhat'
all.data$Love[all.data$love=="4"] <- 'Extremely'
all.data$Love<-factor(c(all.data$Love), levels = c("Neutral", "Extremely Not", "Somewhat Not","Somewhat","Extremely"))
#all.data$Love<-relevel(all.data$Love, "Neutral")

# exaggerated factor for model
all.data$Exaggerated[all.data$exaggerated=="0"] <- 'Extremely Not'
all.data$Exaggerated[all.data$exaggerated=="1"] <- 'Somewhat Not'
all.data$Exaggerated[all.data$exaggerated=="2"] <- 'Neutral'
all.data$Exaggerated[all.data$exaggerated=="3"] <- 'Somewhat'
all.data$Exaggerated[all.data$exaggerated=="4"] <- 'Extremely'
all.data$Exaggerated<-factor(c(all.data$Exaggerated), levels = c("Neutral", "Extremely Not", "Somewhat Not","Somewhat","Extremely"))
#all.data$Exaggerated<-relevel(all.data$Exaggerated, "Neutral")

# confidence variable 
con.count<-ggplot(all.data, aes(confidence)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()
con.count

con.count.num <- all.data %>%
  group_by(confidence) %>%
  summarise(counts = n())
con.count.num

############demographics and analyses
# Accuracy means by group
mean(all.data$accuracy)
median(all.data$accuracy)
range(all.data$accuracy)
sd(all.data$accuracy)

acc.means<-all.data %>%
  group_by(nat_inf_label) %>%
  summarise_at(vars(accuracy), list(name = mean))
acc.means

acc.sd.TisC<-all.data %>%
  group_by(nat_inf_label) %>%
  summarise_at(vars(accuracy), list(name = sd))
acc.sd.TisC

acc.count.TisC<-all.data %>%
  group_by(nat_inf_label) %>%
  summarise(counts = n())
acc.count.TisC

con.means<-all.data %>%
  group_by(nat_inf_label) %>%
  summarise_at(vars(confidence), list(name = mean))
con.means

#df_summary <- reduce(list(acc.means, con.means), 
                     #left_join, by = "nat_inf_label")

#accuracy model compared to a constant of 1 to compare to chance
#accuracy.chance.model<-glmer(accuracy~1+constant + (1|recorded_childF),
                             #data = all.data,
                             #family = binomial (link = 'logit')) 
#summary(accuracy.chance.model)


#accuracy model with positive affect interactions
accuracy2.model<-glmer(accuracy~1+Addressee+
                         confidence +
                         Happy +
                         #Sad +
                         Sooth +
                         Love +
                         Exaggerated +
                         Addressee*Happy +
                         Addressee*Sooth +
                         Addressee*Love +
                         Addressee*Exaggerated +
                         #(1|participantF)+
                         (1|recorded_childF),
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

p1<-emmip(accuracy2.model, Addressee ~ Happy, at = all.data$happy , CIs=TRUE, plotit=T)+theme_bw()

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

#exaggerated contrasts
emmeans_results_exag <- emmeans(accuracy2.model, ~ Addressee*Exaggerated)
#emmeans_results_exag

#pairs(emmeans_results)

#contrast(emmeans_results, "revpairwise", by="nat_inf_labelF",adjust="bonferroni") 


contrast(emmeans_results_exag, "revpairwise", by="Exaggerated",adjust="bonferroni") 

p4<-emmip(accuracy2.model, Addressee ~ Exaggerated, CIs=TRUE, plotit=T)+theme_bw()

#puts all the interaction plots together
grid.arrange(p1,p2,p3,p4, nrow = 2)

#exploratory model with addressee with positive affect predictors
addressee.model<-glmer(nat_inf_labeln ~ 1 +
                        confidence +
                        Happy +
                        #Angry +
                        #Sad +
                        Sooth +
                        Love +
                        Exaggerated +
                        (1|recorded_childF),
                      data = all.data,
                      family = binomial (link = 'logit'))

summary(addressee.model)

tab_model(addressee.model, show.se = TRUE)



# bar plots to look at distributions of affect variables
# sounded happy variable 
happy.count<-ggplot(all.data, aes(happy)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()
happy.count

happy.count.num <- all.data %>%
  group_by(Addressee, happy) %>%
  summarise(counts = n())
happy.count.num

happy.means<-all.data %>%
  group_by(Addressee) %>%
  summarise_at(vars(happy), list(name = mean))
happy.means

happy.sd<-all.data %>%
  group_by(Addressee) %>%
  summarise_at(vars(happy), list(name = sd))
happy.sd

happy.CI<- all.data %>% 
  group_by(Addressee) %>% 
  summarise(ci = list(mean_cl_normal(happy) %>% 
                        rename(mean=y, lwr=ymin, upr=ymax))) %>% 
  unnest
happy.CI

# Sounded Angry
angry.count<-ggplot(all.data, aes(angry)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()
angry.count

angry.count.num <- all.data %>%
  group_by(angry) %>%
  summarise(counts = n())
angry.count.num

# sounded sad
sad.count<-ggplot(all.data, aes(sad)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()
sad.count

sad.count.num <- all.data %>%
  group_by(sad) %>%
  summarise(counts = n())
sad.count.num

# sounded soothing
sooth.count<-ggplot(all.data, aes(sooth)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()
sooth.count

sooth.count.num <- all.data %>%
  group_by(Addressee, sooth) %>%
  summarise(counts = n())
sooth.count.num

sooth.means<-all.data %>%
  group_by(Addressee) %>%
  summarise_at(vars(sooth), list(name = mean))
sooth.means

sooth.sd<-all.data %>%
  group_by(Addressee) %>%
  summarise_at(vars(sooth), list(name = sd))
sooth.sd

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
  group_by(Addressee,love) %>%
  summarise(counts = n())
love.count.num

love.means<-all.data %>%
  group_by(Addressee) %>%
  summarise_at(vars(love), list(name = mean))
love.means

love.sd<-all.data %>%
  group_by(Addressee) %>%
  summarise_at(vars(love), list(name = sd))
love.sd

love.CI<- all.data %>% 
  group_by(Addressee) %>% 
  summarise(ci = list(mean_cl_normal(love) %>% 
                        rename(mean=y, lwr=ymin, upr=ymax))) %>% 
  unnest
love.CI

# sounded exaggerated
exaggerated.count<-ggplot(all.data, aes(exaggerated)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()
exaggerated.count

exaggerated.count.num <- all.data %>%
  group_by(Addressee,exaggerated) %>%
  summarise(counts = n())
exaggerated.count.num

exaggerated.means<-all.data %>%
  group_by(Addressee) %>%
  summarise_at(vars(exaggerated), list(name = mean))
exaggerated.means

exaggerated.sd<-all.data %>%
  group_by(Addressee) %>%
  summarise_at(vars(exaggerated), list(name = sd))
exaggerated.sd

exaggerated.CI<- all.data %>% 
  group_by(Addressee) %>% 
  summarise(ci = list(mean_cl_normal(exaggerated) %>% 
                        rename(mean=y, lwr=ymin, upr=ymax))) %>% 
  unnest
exaggerated.CI

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
                         Affect = c( 3.80, 2.96, 3.86, 2.69, 3.61, 2.56, 3.75, 2.58),
                         CI=c( .03,.03, .03, .03, .03, .03, .03, .03),
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
  scale_x_discrete(breaks=c("1", "2", "3", "4"), labels=c("Happy", "Love","Sooth", "Exaggerated")) 

Affect.Bar+coord_cartesian(ylim=c(0, 5))+scale_fill_manual(values=c("#696969", "#808080"),guide = guide_legend(title = ""),labels=c("cds", "ads"))+
  scale_y_continuous(breaks=c(0,.5,1.0,1.5,2.0, 2.5, 3.0, 3.5, 4.0,4.5, 5.0), expand = c(0,0))









##########STOP HERE!!!!
###accuracy model (Not used)
accuracy.model<-glmer(accuracy~1+Addressee+
                      confidence +
                      happyF +
                      #angryF +
                      sadF +
                      soothF +
                      loveF +
                      exaggeratedF +
                      (1|participantF)+(1|recorded_childF),
                    data = all.data,
                    family = binomial (link = 'logit'))

summary(accuracy.model)

###accuracy model cds
all.data.cds<-subset(all.data, Addressee == 'I')
accuracy.model.cds<-glmer(accuracy~confidence +
                        happyF +
                        #angryF +
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
all.data.ads<-subset(all.data, Addressee == 'A')
accuracy.model.ads<-glmer(accuracy~1+
                        confidence +
                        happyF +
                        #angryF +
                        sadF +
                        soothF +
                        loveF +
                        exaggeratedF +
                        #(1|participantF)+
                          (1|recorded_childF),
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

### useful code

all.data <- all.data %>%
  #select(val, corp) %>%
  mutate(addressee_CnotT = case_when(
    val == "T" & grepl("xds@", tier) & (corp == "CAS" | corp == "ROS") ~ "C",
    grepl("xds@", tier) ~ val),
    addressee_CisT = case_when(
      val == "C" & grepl("xds@", tier) & (corp != "CAS" & corp != "ROS") ~ "T",
      grepl("xds@", tier) ~ val)
  )