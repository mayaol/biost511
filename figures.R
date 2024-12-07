# Author: Maya Oleynikova 
# Project: UW BIOST 511 (AUT 2024) Final Project - DEXASLIM
# Purpose: Perform descriptive and analytic analyses on DEXASLIM dataset for final report
# Date: 12/6/2024

# Set up ------------------------------------------------------
library(tableone)     
library(e1071) 
library(tidyverse)
library(gtsummary) 
library(systemfonts)
library(flextable)

# Data ------------------------------------------------------

# Loading in DEXASLIM data, removing rows with missing data
data <- read.csv("C:/Users/mayaol/biost511/final_project/dexaslim.csv")
data <- na.omit(data)

# Creating weightdiff variable (change from Week 0 to Week 6)
data$weightdiff <- data$week6weight - data$preweight

# Creating factor variables for sex and group
data$sex.f <- ifelse(data$sex == 1, "Male", "Female")
data$group.f <- ifelse(data$group == 1, "DEXASLIM", "Control")

# Analysis ------------------------------------------------------

# Table 1: Demographics of study participants by intervention status (n=52)
data %>% select(sex.f, preweight, week6weight, group.f, weightdiff) %>%
  tbl_summary(by=group.f,
              missing = "ifany",
              type = all_continuous() ~ "continuous2",
              statistic = all_continuous() ~ c("{mean} ({sd})",
                                               "{median} ({p25}, {p75})", 
                                               "{min}, {max}",
                                               "{skewness}"),
              digits = list(all_continuous() ~ 1),  
              label = list(sex.f ~ "Sex",
                           preweight  ~ "Weight at week 0 (kg)",
                           week6weight ~ "Weight at week 6 (kg)",
                           weightdiff ~ "Change in weight (kg)")
  ) %>%
  modify_header(label = "**Variable**") %>%
  as_flex_table()

# Figure 1: Boxplots of intervention change in weight by sex (n=27)
ggplot(data, aes(x = factor(sex.f), y = weightdiff)) +
  geom_boxplot(aes(fill = factor(sex.f)), 
               outlier.shape = 16, 
               outlier.size = 3,
               width = 0.4) +
  labs(title = "Boxplot of Weight Difference by Sex",
       x = "Sex", 
       y = "Change in Weight (kg)",
       fill = "Sex") +
  theme_minimal() +
  scale_fill_manual(values = c("coral1", "darkslategray3")) +
  theme(axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 18, margin = margin(r = 30)),
        legend.title = element_text(size = 16),   
        legend.text = element_text(size = 14))+
  scale_y_continuous(expand = c(0.05, 0), limits = c(min(data$weightdiff) - 2, max(data$weightdiff) + 2)) + 
  geom_hline(yintercept = 0, color = "antiquewhite4", size = 0.5)

# Figure 2: Bar chart of change in weight in DEXASLIM and control groups (n=52)
# First, adjusting data by changing the sign on weightdiff for readability
data$weightdiff_change <- data$weightdiff * (-1)

ggplot(data, aes(x = factor(group.f), y = weightdiff_change, fill = factor(group.f))) +
  stat_summary(fun = "mean", geom = "bar", width = 0.3, color = "black") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.15) +
  labs(title = "Bar chart of change in weight in DEXASLIM and control groups", 
       x = "Group", 
       y = "Reduction in weight (kg)",
       fill = "Group") +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        axis.title = element_text(size = 18),
        axis.title.y = element_text(size = 18, margin = margin(r = 30)),
        plot.title = element_text(size = 16, hjust = 0.5),
        legend.title = element_text(size = 16),   
        legend.text = element_text(size = 14))

# Hypothesis test ------------------------------------------------------
# Parametric hypothesis tests
# 1. To assess for a potential difference in weight change between the DEXASLIM 
# and control groups
# Two-sample t-test calculation
t.test(weightdiff ~ group.f, alternative = "two.sided",
       var.equal = TRUE, conf.level = 0.95, data=data)

# 2. To assess for a potential difference in weight change in the control group
# Paired t-test calculation
t.test(data$preweight[data$group.f == "Control"], 
       data$week6weight[data$group.f == "Control"], 
       alternative = "two.sided", paired = TRUE, conf.level = 0.95)

# -----------------------------------------------------------------------------

# Not included in report - used to check assumption of Normal distribution only
# Create an overlayed histogram of weightdiff by group
ggplot(data, aes(x = weightdiff, fill = factor(group.f), color = factor(group.f))) +
  geom_histogram(binwidth = 0.4, position = "identity", alpha = 0.5, 
                 aes(y = ..density..)) +  # Use density instead of counts
  labs(title = "Overlayed Histogram of Weight Difference by Group",
       x = "Change in Weight (kg)",
       y = "Density",
       fill = "Group", color = "Group") +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +  
  scale_color_manual(values = c("blue", "green")) +  
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, hjust = 0.5)) + 
  xlim(-15, 10) +  
  ylim(0, 1)  
