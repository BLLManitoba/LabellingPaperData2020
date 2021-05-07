# Use this script for the Exp 2a Tseltal Dataset
library(dplyr)
library(ggpubr)
library(ggplot2)
library(lme4)
library(tidyverse)
library(lmerTest)
library(emmeans)
library(sjPlot)
theme_set(theme_pubr())

# call in the data
all.data.raw=read.csv(file = 'clean_data/Exp2a_Tseltal_clean.csv',header=T)
# Removes any responses tagged jumk
all.data.all<-subset(all.data.raw, rater_label != 'junk')

#remove all non-Female adult speakers
all.data.all$speaker <- 0
fa.samples <- which(grepl('^FA', all.data.all$block))
all.data.all$speaker[fa.samples] <- 1

all.data<-subset(all.data.all, speaker == 1)

# sets variables as factors
all.data$recording<-factor(all.data$recording)
all.data$participant<-factor(all.data$participant)
all.data$nat_inf_label<-factor(all.data$nat_inf_label)
all.data$rater_label<-factor(all.data$rater_label)

# Label accuracy
all.data$accuracy[all.data$rater_label=="ads" &
                    all.data$nat_inf_label=="A"] <- 1
all.data$accuracy[all.data$rater_label=="cds" &
                    all.data$nat_inf_label=="T"] <- 1
all.data$accuracy[all.data$rater_label=="cds" &
                    all.data$nat_inf_label=="C"] <- 1
all.data$accuracy[all.data$rater_label=="cds" &
                    all.data$nat_inf_label=="A"] <- 0
all.data$accuracy[all.data$rater_label=="ads" &
                    all.data$nat_inf_label=="T"] <- 0
all.data$accuracy[all.data$rater_label=="ads" &
                    all.data$nat_inf_label=="C"] <- 0

# removes the 2 cases missing a response
all.data <- all.data %>% drop_na() 

# sanity check
acc.count.num <- all.data %>%
  group_by(accuracy) %>%
  summarise(counts = n())
acc.count.num

# collapse C and T in Native Informer label
all.data$nat_inf_label_TisC[all.data$nat_inf_label=="A"] <- "A"
all.data$nat_inf_label_TisC[all.data$nat_inf_label=="C"] <- "C"
all.data$nat_inf_label_TisC[all.data$nat_inf_label=="T"] <- "C"
all.data$nat_inf_label_TisC <- factor(all.data$nat_inf_label_TisC)

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

# C and T are collapsed 
acc.means.TisC<-all.data %>%
  group_by(nat_inf_label_TisC) %>%
  summarise_at(vars(accuracy), list(name = mean))
acc.means.TisC

acc.sd.TisC<-all.data %>%
  group_by(nat_inf_label_TisC) %>%
  summarise_at(vars(accuracy), list(name = sd))
acc.sd.TisC

acc.count.TisC<-all.data %>%
  group_by(nat_inf_label_TisC) %>%
  summarise(counts = n())
acc.count.TisC

con.means<-all.data %>%
  group_by(nat_inf_label_TisC) %>%
  summarise_at(vars(confidence), list(name = mean))
con.means

# Breaks out C and T
acc.means<-all.data %>%
  group_by(nat_inf_label) %>%
  summarise_at(vars(accuracy), list(name = mean))
acc.means

con.means<-all.data %>%
  group_by(nat_inf_label) %>%
  summarise_at(vars(confidence), list(name = mean))
con.means

###accuracy model T is C
accuracy.TisC.model<-glmer(accuracy~1+nat_inf_label_TisC+
                             confidence +
                        (1|participant)+(1|recording),
                      data = all.data,
                      family = binomial (link = 'logit'))

summary(accuracy.TisC.model)

accuracy.TisC.model.simple<-glmer(accuracy~1+nat_inf_label_TisC+
                             confidence +
                             #(1|participant)+
                               (1|recording),
                           data = all.data,
                           family = binomial (link = 'logit'))

summary(accuracy.TisC.model.simple)

#anova(accuracy.TisC.model,accuracy.TisC.model.simple)
emmeans_results <- emmeans(accuracy.TisC.model.simple, ~ nat_inf_label_TisC)
emmeans_results
tab_model(accuracy.TisC.model.simple)

###accuracy model T & C separated
accuracy.model<-glmer(accuracy~1+nat_inf_label+
                        confidence +
                        (1|participant)+(1|recording),
                      data = all.data,
                      family = binomial (link = 'logit'))

summary(accuracy.model)

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

