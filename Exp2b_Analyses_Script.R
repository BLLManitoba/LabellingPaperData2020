# Use this script for the Exp 2a Tseltal Dataset
library(dplyr)
library(tidyr)
library(ggpubr)
library(ggplot2)
library(lme4)
library(lmerTest)
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
all.data$nat_inf_label<-factor(all.data$nat_inf_label)
all.data$rater_label<-factor(all.data$rater_label)

# Label accuracy
all.data$accuracy[all.data$rater_label=="ads" &
                    all.data$nat_inf_label=="A"] <- 1
all.data$accuracy[all.data$rater_label=="cds" &
                    all.data$nat_inf_label=="T" |
                    all.data$nat_inf_label=="C"] <- 1
all.data$accuracy[all.data$rater_label=="cds" &
                    all.data$nat_inf_label=="A"] <- 0
all.data$accuracy[all.data$rater_label=="ads" &
                    all.data$nat_inf_label=="T"|
                    all.data$nat_inf_label=="C"] <- 0

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
exaggerated.count<-ggplot(all.data, aes(exaggerate)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()
exaggerated.count

exaggerated.count.num <- all.data %>%
  group_by(exaggerate) %>%
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
all.data %>% 
  rename(confidence = confidence.x)

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
