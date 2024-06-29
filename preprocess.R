# Load packages
library(corrplot)
library(tidyverse)
library(psych)
library(ggpubr)
library(ggplot2)
library(dplyr)
library(lm.beta)
library(sjPlot)
library(rstatix)
library(sjtable2df)
library("MatchIt")

# Load the CMI dataset
data <- read.csv('CMI_data.csv')

## Extract the Neurotypical group from the dataset
data$PreInt_TxHx.Past_DX <- replace(data$PreInt_TxHx.Past_DX,
                                    data$PreInt_TxHx.Past_DX == ".",
                                    0)

NT <- subset(data, data$PreInt_TxHx.Past_DX == 0)

## Remove the participant who has fat mass < 0 
NT_new <- subset(NT, NT$BIABIA_Fat > 0)

## Categorize into aftersport and non-aftersport group

# Remove the participants who did not answer aftersport questionnaire
df <- NT_new %>% filter(
  !is.na(NT_new$PreInt_EduHxyoga) &
    !is.na(NT_new$PreInt_EduHxmartial_arts) &
    !is.na(NT_new$PreInt_EduHxdance) &
    !is.na(NT_new$PreInt_EduHxschool_sports)
)

# create the aftersport variable that tells if the person did 
# any of the aftersport 
df_aftersport <- data.frame(df$PreInt_EduHxyoga, 
                            df$PreInt_EduHxmartial_arts,
                            df$PreInt_EduHxdance,
                            df$PreInt_EduHxschool_sports)

df$aftersport <- rowSums(df_aftersport)

# for the value that is above 0, replace into 1 (doing aftersport). 
df$aftersport <- replace(df$aftersport,df$aftersport != 0, 1)

## Create the PPS Score variable 
pps_score <- data.frame(as.numeric(df$PPS.PPS_F_Score),
                        as.numeric(df$PPS.PPS_M_Score))

df$pps_Score <- rowSums(pps_score, na.rm = TRUE)

## BMI Percentile that is <1 is equal to 1
df$bmi_percentile <- replace(df$bmi_percentile, df$bmi_percentile < 1, 1)


## Add the Fitness Z-score variables

# Endurance 
min = df$Fitness_EnduranceTime_Mins
sec = df$Fitness_EnduranceTime_Sec

min_new = replace(min, min == "." | min == " ", "0")
sec_new = replace(sec, sec == "." | sec == " ", "0")

endurance <- as.numeric(min_new) * 60 + as.numeric(sec_new)

# calculate the percentile of the endurance total
endurance_new <- replace(endurance,  endurance == "0", NA)
a = na.omit(endurance_new)
mean_a = mean(a)
sd_a = sd(a)
endurance_zscore <- (endurance_new - mean_a) / sd_a
df$endurance_zscore <- endurance_zscore 

# Aerobic
vo2max <- df$Fitness_AerobicV02Max
vo2max_new <- replace(vo2max, vo2max == "." | vo2max == " ", NA)
c = na.omit(vo2max_new)
mean_c = mean(as.numeric(c))
sd_c = sd(as.numeric(c))
vo2max_zscore <- (as.numeric(vo2max_new) - mean_c) / sd_c  
df$vo2max_zscore <- vo2max_zscore

# Merge them all together 
# Convert NA to 0 since there is no 0 in the dataset
df$vo2max_zscore <- replace(df$vo2max_zscore, is.na(df$vo2max_zscore), 0)
df$endurance_zscore <- replace(df$endurance_zscore, is.na(df$endurance_zscore), 0)

b <- data.frame(as.numeric(df$vo2max_zscore), as.numeric(df$endurance_zscore))

df$Fitness_zscore <- rowSums(b, na.rm = TRUE)
df$Fitness_zscore <- replace(df$Fitness_zscore, df$Fitness_zscore==0, NA)

# Merge the PAQA and PAQC as well
paq_a <- df$PAQ_APAQ_A_Total
paq_c <- df$PAQ_CPAQ_C_Total

paq_a <- replace(paq_a, paq_a=="." | paq_a==" ", 0)
paq_c <- replace(paq_c, paq_c=="." | paq_c==" ", 0)

d <- data.frame(as.numeric(paq_a), as.numeric(paq_c))

df$PAQ_Total <- rowSums(d, na.rm = TRUE)
df$PAQ_Total <- replace(df$PAQ_Total, df$PAQ_Total == 0, NA)

# Use Match it to create the matched sample
match_healthy <- matchit(aftersport ~ PhysicalAge +
                           FSQ.FSQ_04 + PhysicalSex + 
                           PreInt_Demos_Fam.Child_Ethnicity +
                           PreInt_Demos_Fam.Child_Race +
                           pps_Score + WISCWISC_FSIQ_Percentile,
                         data = df,
                         method = "nearest", distance = "glm") 

# Create it into the data frame
match_healthy_df <- match.data(match_healthy)
