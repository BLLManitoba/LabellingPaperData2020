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
                  by = c("ID", "recording", "part")) 
                
write.csv(all.data1, "clean_data/all.data1.csv", row.names = FALSE)