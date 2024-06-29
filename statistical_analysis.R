options(scipen=999)

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
library(car)
library("MatchIt")
library(effectsize)

# Turn the chracter or integer variables into numeric variables for the statistical analysis
df <- df %>% mutate_if(is.character, as.numeric)
df <- df %>% mutate_if(is.integer, as.numeric)

## Categorize the data into aftersport and non-aftersport
healthy_sport <- subset(df, aftersport == 1 )
healthy_nonsport <- subset(df, aftersport == 0 )

## Find the mean and standard deviation for each variables from each group and whole samples
summary <- describe(df)
sport_summary <- describe(healthy_sport)
nonsport_summary <- describe(healthy_nonsport)

## ---------------------------------- Between-group analysis -------------------------------------------------
# Homogenity of variance
var.test(healthy_sport$new_data.PAQ_Total, healthy_nonsport$new_data.PAQ_Total)

# Two-tailed independent sample t-test
t.test(new_data.PAQ_Total ~ new_data.aftersports, data = df, var.equal = TRUE, alternative = "two.sided")

## ---------------------------------- Correlation Analysis -------------------------------------------------
# Calculate the correlation for all variables
tab_corr(df, p.numeric = TRUE, triangle = "lower",na.deletion = c("pairwise"))
tab_corr(healthy_nonsport, p.numeric = TRUE, triangle = "lower",na.deletion = c("pairwise")) # make sure to exclude the "aftersports" variable when running this line of code
