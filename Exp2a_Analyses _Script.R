# Use this script for the Farci Dataset
library(dplyr)
library(tidyr)
library(ggpubr)
library(ggplot2)
library(lme4)
library(lmerTest)
theme_set(theme_pubr())

# call in the data
all.data.raw=read.csv(file = 'clean_data/Exp2a_Tseltal_clean.csv',header=T)
all.data<-subset(all.data.raw, rater_label != 'junk')

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

# removes the 2 cases mmissing a response
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

# C and T are collapsed 
acc.means.TisC<-all.data %>%
  group_by(nat_inf_label_TisC) %>%
  summarise_at(vars(accuracy), list(name = mean))
acc.means.TisC

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
                        (1|participant)+(1|recording),
                      data = all.data,
                      family = binomial (link = 'logit'))

summary(accuracy.TisC.model)

###accuracy model
accuracy.model<-glmer(accuracy~1+nat_inf_label+
                        (1|participant)+(1|recording),
                      data = all.data,
                      family = binomial (link = 'logit'))

summary(accuracy.model)

