library(dataverse)
library(tidyverse)
library(labelled)
library(modelsummary)
library(performance)
whan_data <- get_dataframe_by_name(
  filename = "final WP replication dataset.tab",
  dataset = "10.7910/DVN/JGE9OF",
  original=TRUE,
  .f = haven::read_dta,
  server = "dataverse.harvard.edu")
whan_data$warnumber<-factor(whan_data$warnumber)

whan_data$ghdumr<- factor(whan_data$ghdumr, levels=c(0, 1), labels=c("Non-signatory", "Signatory"))
whan_data$raceorrel <- factor(whan_data$raceorrel, levels=c(0, 1), labels=c("No", "Yes"))
whan_data$waraims2<-factor(whan_data$waraims2, levels=c(0, 1), labels=c("Other", "Conquest/Regime change"))
# first model
model1 <- lm(lnnoncom ~ ghdumr + polity + raceorrel + apc + waraims2 + duration + finalprop  + lnnewpop,
             data = whan_data
)
# second model
model2 <- lm(lnnoncom ~ brat + polity  + raceorrel + apc + waraims2 + duration + finalprop + lnnewpop,
             data = whan_data)
#third model
model3 <- lm(
  lnnoncom ~ ghdumr + demtreat + demdum + raceorrel + apc + waraims2 + duration + finalprop + lnnewpop,
  data = whan_data
)
#fourth model
model4 <- lm(
  lnnoncom ~ ghdumr + demdum + apcdem +  raceorrel + apcdum + waraims2 + duration + finalprop  + lnnewpop,
  data = whan_data
)



#Replicating the main table (more or less)-----

# A mapping of coefficient names

map<-c(
  lnnoncom = "Non-Combatant Deaths (logged)",
  # International Law: 
  ghdumrSignatory  = "Treaty Status",
  brat = "Mutual Treaty Ratification",
  demtreat = "Treaty * Regime Type",
  # Alternative Hypotheses
  polity = "Regime Type",
  demdum = "Regime Type",
  apcdem = "Regime Type * Strategy",
  raceorrel = 'Racial/Religious Conflict',
  # Strategic Hypotheses
  apc = "Attrition or Counterinsurgency Strategy",
  apcdum = "Attrition or Counterinsurgency Strategy",
  `waraims2Conquest/Regime change` = "War Aims",
  duration = "War Duration",
  # controls
  finalprop = "Relative Capabilities",
  lnnewpop = "Adversary Population Size (logged)",
  `(Intercept)` = "Constant"
)

model_list <- list("Model 1 (Baseline)" = model1, 
                   "Model 2 (Mutual Ratification)" = model2, 
                   "Model 3 (Regime Type * Treaty)" = model3, 
                   "Model 4 (Regime Type * Strategy)" = model4)


## Table ---

# 1. Cluster SEs on War number
# 2. Use Bootstrapped standard errors
set.seed(999)
library(gt)
ms<-modelsummary(
  model_list,
 # shape = term ~ statistic,
  estimate = "{estimate} ({statistic})",
  statistic = NULL,
  #statistic = c("std.error", "({p.value})"),
  coef_map = map,
  gof_map = c("nobs", "r.squared", "bic", 'aic'),
  vcov = "bootstrap",
  R = 1000,
  cluster = ~ warnumber,
  output= 'gt',
 notes = 'T-statistics in parentheses'
)

ms|>
  tab_row_group(label = html("<b></b>"),
                rows=c(12:16)
  )|>
  tab_row_group(label = html("<b>Controls</b>"),
                rows=c(10,11)
  )|>

  tab_row_group(label = html("<b>Strategic Hypotheses</b>"),
                rows=c(7,8, 9)
  )|>
  tab_row_group(label = html("<b>Alternative Hypotheses</b>"),
                rows=c(4,5, 6)
  )|>
  tab_row_group(label=html('<b>International Law</b>'),
                rows = c(1,2,3),
  )|>
  tab_options(row_group.as_column = TRUE)|>
  
  fmt_markdown() 


#Predictions and marginal effects------

library(marginaleffects)

# average marginal effects:
avg_comparisons(model1, type='response', vcov=~warnumber)


# plotting predictions for geneva vs. non-geneva
plot_predictions(model1, condition=list('ghdumr'=c("Non-signatory","Signatory")),
                 type='response'
) +
  theme_bw() +
  labs(y="Non-Combatant Deaths (logged)",
       x= "Treaty Status"
       )


# plotting predictions as function of duration
plot_predictions(model1,  condition=list('duration', 'ghdumr'),
                 type='response',
                 rug=TRUE
                 
) +
  theme_bw() +
  labs(y = "Non-Combatant Deaths (logged)",
       x = "Duration",
       color = 'Treaty Status',
       fill= 'Treaty Status',
       title ='Predicted non-combatant deaths by duration and treaty status'
       )


#Join significance of the alternative hypothesis coefficients----


model0 <- lm(lnnoncom ~ ghdumr  + apc + waraims2 + duration + finalprop  + lnnewpop,
             data = whan_data
)

compare_performance(model0, model1)
# f-test for nested models, the null hypothesis here is that model1 is no better than model0
anova(model0, model1)


# Prediction checks ----
# Note the posterior predictive check in particular: 
check_model(model1)



# Zero inflation and counts ----
# The authors argue that this isn't appropriate because the observation periods
# are different and the process of civilian killing varies across wars. Still:
# this looks *a lot* like zero inflation
plot(density(whan_data$lnnoncom))



# converting the logged civilian deaths back to counts. 
whan_data$counts <- round(exp(whan_data$lnnoncom)-1)

whan_data|>
  mutate(zeros = counts ==0 )|>
  mutate(ghdumr =as.numeric(ghdumr), 
         raceorrel = as.numeric(raceorrel),
         waraims2 = as.numeric(waraims2)
         )|>
  select(zeros, 
         ghdumr , 
         polity ,
         raceorrel ,
         apc ,
         waraims2  ,
         finalprop  , 
         lnnewpop )|>
  cor()


whan_data|>
  group_by(waraims2)|>
  summarise(props  = mean(counts==0))


library(glmmTMB)
library(parameters)


form<-formula(counts ~ ghdumr + polity + raceorrel + apc + waraims2  + finalprop  + 
                lnnewpop +offset(log(duration))) 

negbinmodel <- glmmTMB(
  form,
  ziformula = ~ 0,
  data = whan_data,
  family = nbinom1(link = 'log')
)

zero_inflated_model <- glmmTMB(form, 
                               ziformula = ~  waraims2 + raceorrel
                               ,  
                               data=whan_data, 
                               family=nbinom1(link='log')
                               
                               )
# in this model, the effect of signing the geneva conventions is negative and 
# significant.
summary(zero_inflated_model)


model_parameters(zero_inflated_model, robust = TRUE, vcov_estimation = "vcovCL", 
                 vcov_args = list(type = "HC1", cluster=whan_data$warnumber))

