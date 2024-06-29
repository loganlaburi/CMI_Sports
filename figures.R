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

## ---------------------------------- Bar Graph -------------------------------------------------
# Figure 1 - 3
df$Level <- factor(ifelse(df$aftersport == 1, "Sports", "Non-Sports"))

df1 <- df %>% select(Level, CBCL_AP)
df2 <- na.omit(df1)

# Calculates mean, sd, se and IC
my_sum <- df2 %>%
  group_by(Level) %>%
  summarise( 
    n=n(),
    mean=mean(CBCL_AP),
    sd=sd(CBCL_AP)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

# bar plot with the error bar using the standard deviation
ggplot(my_sum, aes(x=as.factor(Level), y=mean, fill = as.factor(Level))) +
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.1,  color = "black", alpha = 0.9, linewidth = 0.5) + 
  # coord_cartesian(ylim = c(57, 63)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() + scale_fill_manual(values=c("#4C5762", "#B7BCC9")) +
   xlab(" ") +
  ylab("CBCL: Attention Problems")

## ---------------------------------- Correlation Heatmap -------------------------------------------------
# Figure 4 & 5

fitness = healthy_sport[,c(1:17)]
outcomes = healthy_sport[,c(19:59)]

M = cor(fitness, outcomes,use="pairwise.complete.obs")
testRes = cor.mtest(healthy_sport[,c(1:17, 19:59)])
pvalue = testRes$p
pvalues = pvalue[c(1:17), c(18:58)]

# use for loop to adjust p-values
pvalue_adj <- pvalues

# Bonferroni 
for (i in 1:ncol(fitness)) {
  # NIH 
  a <- pvalues[i, 1:4]
  a_new <- p.adjust(a, method = "bonferroni")
  pvalue_adj[i, 1:4] <- a_new
  
  # WISC 
  b <- pvalues[i, 5:8]
  b_new <- p.adjust(b, method = "bonferroni")
  pvalue_adj[i, 5:8] <- b_new
  
  # WIAT
  c <- pvalues[i, 9:17]
  c_new <- p.adjust(c, method = "bonferroni")
  pvalue_adj[i, 9:17] <- c_new
  
  # Mental health
  d <- pvalues[i, 18:25]
  d_new <- p.adjust(d, method = "bonferroni")
  pvalue_adj[i, 18:25] <- d_new
  
  # Conner
  e <- pvalues[i, 26:30]
  e_new <- p.adjust(e, method = "bonferroni")
  pvalue_adj[i, 26:30] <- e_new
  
  # CBCL
  f <- pvalues[i, 31:41]
  f_new <- p.adjust(f, method = "bonferroni")
  pvalue_adj[i, 31:41] <- f_new
}

# Round the p-value for better visualization 
round(pvalue_adj, 3)

# Save the figure as PDF
pdf(file = "healthysport.pdf", paper = 'a4')
# Generate the correlation heatmap
corrplot(M, p.mat = pvalue_adj, method = "color", sig.level = c(0.001, 0.01, 0.05), addCoef.col = 1,number.cex = 0.25, tl.cex = 0.25, insig = 'label_sig', 
         pch.col = 'grey20', pch.cex = 0.3, tl.col='black')
dev.off()
