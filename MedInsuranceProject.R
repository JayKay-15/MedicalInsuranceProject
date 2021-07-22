##### working in R to analyze and predict medical insurance costs #####

if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, caTools, lubridate,
               ggthemes, janitor, corrplot, GGally, psych)

rm(list=ls())
setwd("/Users/Jesse/R/MedInsuranceProject/")

# read in movie industry data
df <- read_csv("/Users/Jesse/R/MedInsuranceProject/insurance.csv")

# checking out the data and structure
head(df)
str(df)
summary(df)

df$sex <- as.factor(df$sex)
df$smoker <- as.factor(df$smoker)
df$region <- as.factor(df$region)

# check for missing data
any(is.na(df)) # returns FALSE - no missing data

# histogram of charges
df %>% ggplot(aes(charges)) + geom_histogram(aes(y=stat(density)), fill = "light blue", color = "white", bins = 30) + 
    ggtitle("Distribution of Charges") + theme_bw() + theme(plot.title = element_text(hjust = .5)) +
    geom_density(col="dark blue")


# normal distribution
df %>% ggplot(aes(log10(charges))) + geom_histogram(aes(y=stat(density)), fill = "light blue", color = "white", bins = 30) + 
    ggtitle("Distribution of Charges") + theme_bw() + 
    theme(plot.title = element_text(hjust = .5)) + geom_density(col = "dark blue")


# charges by region and smoker status
df %>% ggplot(aes(region, charges)) + geom_boxplot(fill=c(3,4,6,7)) + 
    theme_bw() + ggtitle("Medical Insurance Charges by Region")

df %>% ggplot(aes(smoker, charges)) + geom_boxplot(fill=c(3,2)) + 
    theme_bw() + ggtitle("Medical Insurance Charges by Smoker Status")

df %>% ggplot(aes(sex, charges)) + geom_boxplot(fill=c(5,7)) + 
    theme_bw() + ggtitle("Medical Insurance Charges by Sex")

df %>% ggplot(aes(as.factor(children), charges)) + geom_boxplot(fill=c(2:7)) + 
    theme_bw() + ggtitle("Medical Insurance Charges by Number of Children")

df <- df %>%
    mutate(obese = as.factor(if_else(bmi >= 30, "yes", "no")))

df %>% ggplot(aes(obese, charges)) + geom_boxplot(fill=c(6,4)) + 
    theme_bw() + ggtitle("Medical Charges by Obesity status (BMI 30+)")

df %>% ggplot(aes(age, charges)) + geom_point(aes(color = smoker)) +
    theme_bw() + ggtitle("Medical Charges by Age and Smoker Status") + scale_color_manual(values=c("red", "blue"))

# correlation
num_cols <- sapply(df,is.numeric) # using only numeric features
ggpairs(df[,num_cols]) # matrix of plots
pairs.panels(df[,num_cols])

cor_mx <- cor(df[,num_cols]) # correlation matrix
cor_mx
corrplot(cor_mx, method = "color", title = "Correlation Matrix for Numeric Features", 
         mar=c(0,0,1,0))

# create dummy variables
df_dummy <- df %>%
    mutate(sex_dummy = if_else(sex == "male", 1, 0)) %>%
    mutate(smoker_dummy = if_else(smoker == "yes", 1, 0))

df_dummy <- df_dummy %>% 
    select(1,3,4,7,9,10)

# correlation all features
ggpairs(df_dummy)
pairs.panels(df_dummy)

dummy_cor_mx <- cor(df_dummy)
corrplot(dummy_cor_mx, method = "color", title = "Correlation Matrix for All Features", 
         mar=c(0,0,1,0))


# building a model - original features
med_model <- lm(charges ~ .-bmi, data = df)
summary(med_model)





