# 1 Read csv into df or tibble, make sure sex and site are factors
df_fit <- read.csv('DevilCondition.csv')
 
library(plyr)
library(tidyverse)

df_fit$Sex <- factor(df_fit$Sex)
df_fit$Site <- factor(df_fit$Site)

# 2 Figure that indicates whether or not BC is related to LTV, sex, and site
ggplot(df_fit, aes(x = LTV, y = BC, color = Sex)) +
  geom_point() +
  facet_wrap(~ Site) +
  labs(x = "Log10 of Tumour Volume", y = "Body Condition") +
  theme_bw()

# Alternative figure THIS COULD BE INTERESTING
# 2 Figure that indicates whether or not BC is related to LTV, sex, and site
ggplot(df_fit, aes(x = LTV, y = BC)) +
  geom_point() +
  facet_wrap(Site ~ Sex) +
  labs(x = "Log10 of Tumour Volume", y = "Body Condition") +
  theme_bw()

# Fit of full model with all interaction terms
fit_1 <- lm(BC ~ LTV*Sex*Site, data = df_fit)
drop1(fit_1, test='F')

# Drop 3-way interaction
fit_2 <- lm(BC ~ LTV + Sex + Site + LTV:Sex + LTV:Site + Sex:Site, data = df_fit )
drop1(fit_2, test = 'F')

# Drop LTV:Site
fit_3 <- lm(BC ~ LTV + Sex + Site + LTV:Sex + Sex:Site, data = df_fit )
drop1(fit_3, test = 'F')

# Drop LTV:Sex
fit_4 <- lm(BC ~ LTV + Sex + Site + Sex:Site, data = df_fit )
drop1(fit_4, test = 'F')

# Drop Sex:Site
fit_5 <- lm(BC ~ LTV + Sex + Site, data = df_fit )
drop1(fit_5, test = 'F')

# Drop Site
fit_6 <- lm(BC ~ LTV + Sex, data = df_fit )
drop1(fit_6, test='F')

# Drop Sex
fit_7 <- lm(BC ~ LTV, data = df_fit )
drop1(fit_7, test='F')
summary(fit_7)

# Left with just LTV as predictor for BC... R^2 is 0.0473 which is terrible
predicted <- predict.lm(fit_7, interval = 'confidence')
df_predicted <- as.data.frame(predicted)
df_fit$fit <- df_predicted$fit
df_fit$CIlower <- df_predicted$lwr
df_fit$CIupper <- df_predicted$upr

ggplot(df_fit, aes(x = LTV, y = BC)) +
  geom_point()+
  geom_line(aes(x = LTV, y= fit), color = 'red', size = 1) +
  geom_ribbon(data = df_fit, aes(ymin = CIlower, ymax = CIupper, alpha = 0.25)) +
  labs(x = "Total tumour volume (log10 mm^3)", y = "Body condition", 
       title = "Predicted devil body condition for given tumour volume",
       subtitle = "95% confidence interval included") +
  theme_bw() +
  theme(legend.position = "none")

plot(fit_7)


df_new <- filter(df_fit, LTV >= 2.5)
# FIt of full model with all interaction terms
fit_1.1 <- lm(BC ~ LTV*Sex*Site, data = df_new)
drop1(fit_1.1, test='F')

# Drop 3-way interaction
fit_2.1 <- lm(BC ~ LTV + Sex + Site + LTV:Sex + LTV:Site + Sex:Site, data = df_new )
drop1(fit_2.1, test = 'F')

# Drop LTV:Site
fit_3.1 <- lm(BC ~ LTV + Sex + Site + LTV:Sex + Sex:Site, data = df_new )
drop1(fit_3.1, test = 'F')

# Drop LTV:Sex
fit_4.1 <- lm(BC ~ LTV + Sex + Site + Sex:Site, data = df_new )
drop1(fit_4.1, test = 'F')

# Drop Sex:Site
fit_5.1 <- lm(BC ~ LTV + Sex + Site, data = df_new )
drop1(fit_5.1, test = 'F')

# Drop Site
fit_6.1 <- lm(BC ~ LTV + Sex, data = df_new )
drop1(fit_6.1, test='F')

# Drop Sex
fit_7.1 <- lm(BC ~ LTV, data = df_new )
drop1(fit_7.1, test='F')
summary(fit_7.1)

predicted.1 <- predict.lm(fit_7.1, interval = 'confidence')
df_predicted.1 <- as.data.frame(predicted.1)
df_new$fit <- df_predicted.1$fit
df_new$CIlower <- df_predicted.1$lwr
df_new$CIupper <- df_predicted.1$upr

ggplot(df_new, aes(x = LTV, y = BC)) +
  geom_point()+
  geom_line(aes(x = LTV, y= fit), color = 'red', size = 1) +
  geom_ribbon(data = df_new, aes(ymin = CIlower, ymax = CIupper, alpha = 0.25)) +
  labs(x = "Total tumour volume (log10 mm^3)", y = "Body condition", 
       title = "Predicted devil body condition for given tumour volume",
       subtitle = "95% confidence interval included") +
  theme_bw() +
  theme(legend.position = "none")

plot(fit_7.1)

