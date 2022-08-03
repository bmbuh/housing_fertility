#Coded by: Brian Buh
#Started on: 03.08.2022
#Last Updated:


library(tidyverse)
library(haven)
library(arsenal)
library(survival)
library(survminer)
library(texreg)
library(sjPlot)
library(lme4)
library(effects)
library(jtools) #summ function
library(broom.mixed) #plot_summs function
library(margins)
library(interactions) #for using cat_plot


#Load data cball2
cball2 <- file.choose()
cball2 <- readRDS(cball2)

#This is necessary until I am able to impute my missing data
cball2 <- cball2 %>% filter(!is.na(ratio))

cball2m <- cball2 %>% filter(sex == "Men")
cball2f <- cball2 %>% filter(sex == "Women")

cball2p1 <- cball2 %>% filter(parity == 1)
cball2p2 <- cball2 %>% filter(parity == 2)
cball2p3 <- cball2 %>% filter(parity == 3)

cball2m %>% count(event, parity)
cball2f %>% count(event, parity)



###########################################################################
# Variable correlation ----------------------------------------------------
###########################################################################

#Testing to see if sex and event are correlate
### Chi-squared test http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r
chisq <- chisq.test(table(cball2$sex, cball2$event))
chisq$observed
round(chisq$expected,3)
library(corrplot)
corrplot(chisq$residuals, is.cor = FALSE)
#This clearly shows that a "worse" future is strongly positive correlated with difficult and strongly negatively correlated with fine

# Contribution in percentage (%)
contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)
# Visualize the contribution
corrplot(contrib, is.cor = FALSE)
# printing the p-value
chisq$p.value
# printing the observed/expected numbers
chisq$observed
chisq$expected
# printing the mean
chisq$estimate

###########################################################################
# Model Fit Testing -------------------------------------------------------
###########################################################################


# --------------------------------------------------------------------------
# Testing Logit versus Cloglog Model ---------------------------------------
# --------------------------------------------------------------------------

#Creating a baseline to see the time since education and event
baselinelogit <- glm(formula = event ~ clock,
                     family = binomial(link = "logit"),
                     data = cball2)


baselinecloglog <- glm(formula = event ~ clock,
                       family = binomial(link = "cloglog"),
                       data = cball2)

summary(baselinelogit)
summary(baselinecloglog)
#There is no difference between logit and cloglog
summ(baselinelogit, exp = TRUE) #takes a minute to process
#The strong relationship between clock and event in this models
#signifies that the baseline hazard is the same for all individuals ( :-) )


testlogit <- glm(formula = event ~ clock + sex + ratio_cat2 + parity + period + age + agesq,
                 family = binomial(link = "logit"),
                 data = cball2)

summary(testlogit)
summ(testlogit, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(testlogit, exp = T)

#Does the test model vary significantly from the baseline?
##Likelihood Ratio Test
anova(baselinelogit, testlogit, test = "Chisq")
##AIC
baselinelogit$aic
testlogit$aic
#The answer appears to be yes


plot(allEffects(testlogit))
ggsave("paramater_effect_03-08-2022.png")


#Deviance Residuals
Data_DevResid <- tibble(Pred_Haz = predict(testlogit, type = "response"),
                        Event = pull(cball2, event),
                        ID = pull(cball2, pidp))%>%
  mutate(DevRes = if_else(Event == 0, 
                          -sqrt(-2*log(1-Pred_Haz)),
                          sqrt(-2*log(Pred_Haz))))

Data_DevResid %>%
  ggplot(aes(x = ID, y = DevRes)) +
  geom_point()

###!!!
##It is clear that logit models are the best fit for my data

# ----------------------------------------------------------------------
# Model for men --------------------------------------------------------
# ----------------------------------------------------------------------

baseline_men <- glm(formula = event ~ clock,
                    family = binomial(link = "logit"),
                    data = cball2m)
summ(baseline_men, exp = TRUE, scale = TRUE)
mlogit <- glm(formula = event ~ clock + ratio_cat2 + parity + period + age + agesq,
              family = binomial(link = "logit"),
              data = cball2m)
summary(mlogit)
summ(mlogit, exp = TRUE)
plot_summs(mlogit, exp = T)

#Likelihood Ratio Test
anova(baseline_men, mlogit, test = "Chisq")
#AIC
baseline_men$aic
mlogit$aic

#Deviance Residuals
Data_DevResid_m <- tibble(Pred_Haz = predict(mlogit, type = "response"),
                          Event = pull(cball2m, event),
                          ID = pull(cball2m, pidp))%>%
  mutate(DevRes = if_else(Event == 0, 
                          -sqrt(-2*log(1-Pred_Haz)),
                          sqrt(-2*log(Pred_Haz))))

Data_DevResid_m %>%
  ggplot(aes(x = ID, y = DevRes)) +
  geom_point() +
  ggtitle("Deviance Residuals Men - end of education") +
  ggsave("dev_resid_men_endedu_03-08-2022.png")



# ----------------------------------------------------------------------
# Model for women ------------------------------------------------------
# ----------------------------------------------------------------------

baseline_women <- glm(formula = event ~ clock,
                      family = binomial(link = "logit"),
                      data = cball2f)
summ(baseline_women, exp = TRUE, scale = TRUE)
flogit<- glm(formula = event ~ clock + ratio_cat2 + parity + period + age + agesq,
             family = binomial(link = "logit"),
             data = cball2f)
summary(flogit)
summ(flogit, exp = TRUE)

#Likelihood Ratio Test
anova(baseline_women, flogit, test = "Chisq")

#AIC
baseline_women$aic
flogit$aic

#Deviance Residuals
Data_DevResid_f <- tibble(Pred_Haz = predict(flogit, type = "response"),
                          Event = pull(cball2f, event),
                          ID = pull(cball2f, pidp))%>%
  mutate(DevRes = if_else(Event == 0, 
                          -sqrt(-2*log(1-Pred_Haz)),
                          sqrt(-2*log(Pred_Haz))))

Data_DevResid_f %>%
  ggplot(aes(x = ID, y = DevRes)) +
  geom_point() +
  ggtitle("Deviance Residuals Women - time since end of education") +
  ggsave("dev_resid_women_endedu_03-08-2022.png")

# --------------------------------------------------------------------------
# Covariate Testing -------------------------------------------------------
# --------------------------------------------------------------------------

###Goodness-of-Fit tests
#AIC Test (comparison)
testlogit$aic
mlogit$aic
flogit$aic
#The AIC improve significantly sex stratification


plot_models(mlogit, flogit, 
            title = "Odds Ratios",
            m.labels = c("Men", "Women"),
            legend.title = "Model",
            # axis.labels = c(
            #  "finfut.num:employed", "finnow.num:employed",
            # #   # "Married - unknown", "Married - non-employed","Married - employed",
            # #   # "Cohab - non-employed", "Cohab - employed","Single",
            #   "Edu. Low", "Edu. Medium", "Edu. High",
            #   # "Job security",
            #   "Future Financial Sit",
            #   "Employed",
            #   "Present Financial Sit",
            #   "PJI",
            #   "Age Squared", "Age, in months", "Time"),
            # axis.lim = c(0.5, 1.4),
            dot.size = 6,
            #colors  = c("#2E9FDF", "#E7B800"), #in case you wanna change to the gold blue set
            p.shape = TRUE,
            grid = TRUE)

ggsave("logit_timesinceedu_m_f_03-08-2022.png")


# --------------------------------------------------------------------------
# basic frailty and cox models --------------------------------------------
# --------------------------------------------------------------------------


#First step is to plot the Baseline Gompertz Regression model
cball2 %>%
  group_by(clock) %>%
  summarise(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = clock, 
             y = log(-log(1-hazard)))) +
  geom_point() +
  geom_smooth()

# ----------------------------------------------------------------------
# Basic Frailty Model --------------------------------------------------
# ----------------------------------------------------------------------

#A basic frailty model - has an added random individual effect
frailty_baseline <- glmer(formula = event ~ clock + (1|pidp),
                          family = binomial,
                          data = cball2,
                          control = glmerControl(optimizer = "bobyqa",
                                                 optCtrl = list(maxfun = 2e5)))

summary(frailty_baseline)
#AIC = 17120
baselinelogit$aic
#AIC = 19548
#The frailty baseline is a slight improvement


#The similarity between the AIC in this model and the above GLM model suggest this "Basic Frailty Model" is unnecessary 
#I cannot include age or agesq because there is a scaling issue between clock and those predictor variables - would need to be rescaled
frailtylogit <- glmer(formula = event ~ clock + sex + ratio_cat2 + parity + period + age + agesq + (1|pidp),
                      data = cball2,
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa",
                                             optCtrl = list(maxfun = 2e5))) #This is to control for the warning "Model is nearly unidentifiable"

summary(frailtylogit)


# ----------------------------------------------------------------------
# CoxPH Model ----------------------------------------------------------
# ----------------------------------------------------------------------

# 1. Kaplan-Meier Test
kmtest <- survfit(Surv(clock, clock2, event) ~ strata (parity), data = cball2, cluster = pidp)
summary(kmtest)
plot(kmtest)

ggsurvplot(kmtest, size = 1,   # change line size
           conf.int = TRUE,          # Add confidence interval
           risk.table = TRUE,        # Add risk table
           # risk.table.col = "strata",# Risk table color by groups
           legend.labs = c("Parity 1", "Parity 2", "Parity 3"),    # Change legend labels
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme_bw()      # Change ggplot2 theme
) 
ggsave("km_base_02-08-2022.png")

# 2. Cox Proportional Hazard Model
coxph <- coxph(formula = Surv(clock, clock2, event) ~ sex + age + agesq + parity + period + ratio*parity, data = cball2, cluster = pidp, method = "breslow")
summary(coxph)
testph <- cox.zph(coxph)
summary(testph)


########################################################################
# Basic Prediction Plots -----------------------------------------------
########################################################################

# using effect_plots https://cran.r-project.org/web/packages/jtools/vignettes/effect_plot.html


#Plotting "ratio" as a continuous variable
a1m0 <- glm(formula = event ~ clock + sex*ratio*parity + period + age + agesq,
            family = binomial(link = "logit"),
            data = cball2)
summary(margins(a1m0))

effect_plot(a1m0, 
            pred = ratio, 
            pred.values = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
            interval = TRUE, 
            y.label = "Experencing a Live Birth")


cball2 %>% count(ratio_cat3)

#The terms of interest MUST be interacted or the difference won't be plotted correct
a1m1 <- glm(formula = event ~ clock + sex + ratio_cat2*parity + period + age + agesq,
            family = binomial(link = "logit"),
            data = cball2)
summ(a1m1, exp = TRUE)
summary(margins(a1m1))

effect_plot(a1m1, 
            pred = ratio_cat2, 
            pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
            interval = TRUE, 
            cat.geom = "line",
            y.label = "Experencing a Live Birth")

cat_plot(a1m1, pred = parity, modx = ratio_cat2, mod2 = sex,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         # modx.values = c("0", "0.1-15", "15-30", "30-45", "45-60", "60-100"), #For ratio_cat3
         # pred.values = c("out of LF", "unemployment", "self-employed", "part time", "full time"),
         # pred.labels = c("Out of the LF", "Unemployment",  "Self-employed", "Part-time", "Full-time"),
         # modx.labels = c("Low", "Medium", "High"),
         mod2.values = c("Women", "Men"),
         mod2.labels = c("Women", "Men"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         legend.main = "Parity") +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
# ggsave("a1m1_int_empstat_edu_(color)S9_03-08-2022.png", dpi = 300)

#The terms of interest MUST be interacted or the difference won't be plotted correct
a1m2 <- glm(formula = event ~ clock + sex*ratio_cat2*period + parity + age + agesq,
            family = binomial(link = "logit"),
            data = cball2)
summary(margins(a1m2))

cat_plot(a1m2, pred = period, modx = ratio_cat2, mod2 = sex,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
         # pred.values = c("out of LF", "unemployment", "self-employed", "part time", "full time"),
         # pred.labels = c("Out of the LF", "Unemployment",  "Self-employed", "Part-time", "Full-time"),
         # modx.labels = c("Low", "Medium", "High"),
         # mod2.values = c("Women", "Men"),
         # mod2.labels = c("Women", "Men"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         legend.main = "Parity") +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
# ggsave("a1m1_int_empstat_edu_(color)S9_03-08-2022.png", dpi = 300)

#--------------------------------------------------------------------------
# Sex-specific models -----------------------------------------------------
#--------------------------------------------------------------------------

#Men
a1m1m <- glm(formula = event ~ clock + ratio_cat2*parity + period + age + agesq,
            family = binomial(link = "logit"),
            data = cball2m)
summary(margins(a1m1m))

cat_plot(a1m1m, pred = parity, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
         # pred.values = c("out of LF", "unemployment", "self-employed", "part time", "full time"),
         # pred.labels = c("Out of the LF", "Unemployment",  "Self-employed", "Part-time", "Full-time"),
         # modx.labels = c("Low", "Medium", "High"),
         # mod2.values = c("Women", "Men"),
         # mod2.labels = c("Women", "Men"),
         x.label = "Parity",
         y.label = "Pr(Experencing a Live Birth)",
         legend.main = "% household income spent on housing:") +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15), legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("a1m1m_parity_ratio_cat2_Smodel_fit_03-08-2022.png", dpi = 300)


#Women
a1m1f <- glm(formula = event ~ clock + ratio_cat2*parity*period + age + agesq,
             family = binomial(link = "logit"),
             data = cball2m)
summary(margins(a1m1f))

cat_plot(a1m1f, pred = parity, modx = ratio_cat2, mod2 = period,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
         # pred.values = c("out of LF", "unemployment", "self-employed", "part time", "full time"),
         # pred.labels = c("Out of the LF", "Unemployment",  "Self-employed", "Part-time", "Full-time"),
         # modx.labels = c("Low", "Medium", "High"),
         # mod2.values = c("Women", "Men"),
         # mod2.labels = c("Women", "Men"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         legend.main = "Parity") +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))

#--------------------------------------------------------------------------
# Parity-specific models --------------------------------------------------
#--------------------------------------------------------------------------

#Parity 1
a1m1p1 <- glm(formula = event ~ clock + sex*ratio_cat2 + period + age + agesq,
             family = binomial(link = "logit"),
             data = cball2p1)
summary(margins(a1m1m))

cat_plot(a1m1p1, pred = ratio_cat2, modx = sex,
         point.size = 2,
         point.shape = TRUE,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
         # pred.values = c("out of LF", "unemployment", "self-employed", "part time", "full time"),
         # pred.labels = c("Out of the LF", "Unemployment",  "Self-employed", "Part-time", "Full-time"),
         # modx.labels = c("Low", "Medium", "High"),
         # mod2.values = c("Women", "Men"),
         # mod2.labels = c("Women", "Men"),
         x.label = "% household income spent on housing",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Parity 1",
         legend.main = "Sex") +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15), legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("a1m1p1_parity_ratio_cat2_Smodel_fit_03-08-2022.png", dpi = 300)


#Parity 2
a1m1p2 <- glm(formula = event ~ clock + sex*ratio_cat2 + period + age + agesq,
              family = binomial(link = "logit"),
              data = cball2p2)
summary(margins(a1m1p2))

cat_plot(a1m1p2, pred = ratio_cat2, modx = sex,
         point.size = 2,
         point.shape = TRUE,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         # interval.geom = TRUE, #Removes confidence intervals
         pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
         # pred.values = c("out of LF", "unemployment", "self-employed", "part time", "full time"),
         # pred.labels = c("Out of the LF", "Unemployment",  "Self-employed", "Part-time", "Full-time"),
         # modx.labels = c("Low", "Medium", "High"),
         # mod2.values = c("Women", "Men"),
         # mod2.labels = c("Women", "Men"),
         x.label = "% household income spent on housing",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Parity 2",
         legend.main = "Sex") +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15), legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("a1m1p2_parity_ratio_cat2_Smodel_fit_03-08-2022.png", dpi = 300)


#Parity 3
a1m1p3 <- glm(formula = event ~ clock + sex*ratio_cat2 + period + age + agesq,
              family = binomial(link = "logit"),
              data = cball2p3)
summary(margins(a1m1p3))

cat_plot(a1m1p3, pred = ratio_cat2, modx = sex,
         point.size = 2,
         point.shape = TRUE,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
         # pred.values = c("out of LF", "unemployment", "self-employed", "part time", "full time"),
         # pred.labels = c("Out of the LF", "Unemployment",  "Self-employed", "Part-time", "Full-time"),
         # modx.labels = c("Low", "Medium", "High"),
         # mod2.values = c("Women", "Men"),
         # mod2.labels = c("Women", "Men"),
         x.label = "% household income spent on housing",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Parity 3",
         legend.main = "Sex") +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15), legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("a1m1p3_parity_ratio_cat2_Smodel_fit_03-08-2022.png", dpi = 300)


