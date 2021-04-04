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

all.data1 <- merge(x = all.data.raw, y = all.data.raw2,
                  by = c("recording", "part"), all.y = FALSE) 
                

all.data1 <- merge(all.data.raw, all.data.raw2, all = FALSE)