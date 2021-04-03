# Use this script for the Farci Dataset
library(dplyr)
library(ggpubr)
library(ggplot2)
library(lme4)
library(lmerTest)
theme_set(theme_pubr())

# call in the data
all.data.raw=read.csv(file = 'clean_data/Farci_Clean.csv',header=T)

all.data<-subset(all.data.raw, participant != 'SUB15')

# sets variables as factors
all.data$recorded_childF<-factor(all.data$recorded_child)
all.data$participantF<-factor(all.data$participant)
all.data$nat_inf_labelF<-factor(all.data$nat_inf_label)

# Label accuracy
all.data$accuracy[all.data$rater_labeln=="1" &
                    all.data$nat_inf_labeln=="1"] <- 1
all.data$accuracy[all.data$rater_labeln=="0" &
                    all.data$nat_inf_labeln=="0"] <- 1
all.data$accuracy[all.data$rater_labeln=="1" &
                    all.data$nat_inf_labeln=="0"] <- 0
all.data$accuracy[all.data$rater_labeln=="0" &
                    all.data$nat_inf_labeln=="1"] <- 0

# build affect variables
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
  group_by(sooth) %>%
  summarise(counts = n())

# sounded loving
love.count<-ggplot(all.data, aes(love)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()
love.count

love.count.num <- all.data %>%
  group_by(love) %>%
  summarise(counts = n())
love.count.num

# sounded exaggerated
exaggerated.count<-ggplot(all.data, aes(exaggerated)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()
exaggerated.count

exaggerated.count.num <- all.data %>%
  group_by(exaggerated) %>%
  summarise(counts = n())
exaggerated.count.num

# happy factor for model
all.data$happyF[all.data$happy=="0"] <- 'ENot'
all.data$happyF[all.data$happy=="1"] <- 'SNot'
all.data$happyF[all.data$happy=="2"] <- 'Neut'
all.data$happyF[all.data$happy=="3"] <- 'Some'
all.data$happyF[all.data$happy=="4"] <- 'Extr'
all.data$happyF<-factor(all.data$happyF)
all.data$happyF<-relevel(all.data$happyF, "Neut")

# angry factor for model
all.data$angryF[all.data$angry=="0"] <- 'ENot'
all.data$angryF[all.data$angry=="1"] <- 'SNot'
all.data$angryF[all.data$angry=="2"] <- 'Neut'
all.data$angryF[all.data$angry=="3"] <- 'Some'
all.data$angryF[all.data$angry=="4"] <- 'Extr'
all.data$angryF<-factor(all.data$angryF)
all.data$angryF<-relevel(all.data$angryF, "Neut")

# sad factor for model
all.data$sadF[all.data$sad=="0"] <- 'ENot'
all.data$sadF[all.data$sad=="1"] <- 'SNot'
all.data$sadF[all.data$sad=="2"] <- 'Neut'
all.data$sadF[all.data$sad=="3"] <- 'Some'
all.data$sadF[all.data$sad=="4"] <- 'Extr'
all.data$sadF<-factor(all.data$sadF)
all.data$sadF<-relevel(all.data$sadF, "Neut")

sooth.count.num
# sooth factor for model
all.data$soothF[all.data$sooth=="0"] <- 'ENot'
all.data$soothF[all.data$sooth=="1"] <- 'SNot'
all.data$soothF[all.data$sooth=="2"] <- 'Neut'
all.data$soothF[all.data$sooth=="3"] <- 'Some'
all.data$soothF[all.data$sooth=="4"] <- 'Extr'
all.data$soothF<-factor(all.data$soothF)
all.data$soothF<-relevel(all.data$soothF, "Neut")

# love factor for model
all.data$loveF[all.data$love=="0"] <- 'ENot'
all.data$loveF[all.data$love=="1"] <- 'SNot'
all.data$loveF[all.data$love=="2"] <- 'Neut'
all.data$loveF[all.data$love=="3"] <- 'Some'
all.data$loveF[all.data$love=="4"] <- 'Extr'
all.data$loveF<-factor(all.data$loveF)
all.data$loveF<-relevel(all.data$loveF, "Neut")

# exaggerated factor for model
all.data$exaggeratedF[all.data$exaggerated=="0"] <- 'ENot'
all.data$exaggeratedF[all.data$exaggerated=="1"] <- 'SNot'
all.data$exaggeratedF[all.data$exaggerated=="2"] <- 'Neut'
all.data$exaggeratedF[all.data$exaggerated=="3"] <- 'Some'
all.data$exaggeratedF[all.data$exaggerated=="4"] <- 'Extr'
all.data$exaggeratedF<-factor(all.data$exaggeratedF)
all.data$exaggeratedF<-relevel(all.data$exaggeratedF, "Neut")

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

acc.means<-all.data %>%
  group_by(nat_inf_label) %>%
  summarise_at(vars(accuracy), list(name = mean))
acc.means

con.means<-all.data %>%
  group_by(nat_inf_label) %>%
  summarise_at(vars(confidence), list(name = mean))
con.means

#df_summary <- reduce(list(acc.means, con.means), 
                     #left_join, by = "nat_inf_label")

###accuracy model
accuracy.model<-glmer(accuracy~1+nat_inf_labelF+
                      confidence +
                      happyF +
                      angryF +
                      sadF +
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
                         nat_inf_label*happyF +
                         #nat_inf_label*sadF +
                         nat_inf_label*soothF +
                         nat_inf_label*loveF +
                         nat_inf_label*exaggeratedF +
                        (1|participantF)+(1|recorded_childF),
                      data = all.data,
                      family = binomial (link = 'logit'))

summary(accuracy2.model)





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