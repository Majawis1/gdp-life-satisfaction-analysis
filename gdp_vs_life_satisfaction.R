rm(list=ls())
attach(project_data)


# descriptive analysis
  summary(project_data)
  plot(life_sat ~ gdp)
  plot(life_sat ~ log(gdp))
  
  hist(gdp)
  hist(log(gdp))
  hist(unempl)
  hist(log(unempl))
  hist(life_sat)
  
  install.packages("modelsummary")
  library(modelsummary)
  datasummary_skim(project_data)

# is gini missingness related to income?
  tapply(gdp, is.na(gini), mean, na.rm = TRUE)
  
  gini_missing <- lm(is.na(gini) ~ log(gdp))
  summary(gini_missing)
  # not statistically significant 

#base model
  base_model <- lm(life_sat~log(gdp))
  summary(base_model)
  
# model with log(gdp)
  model1 <- lm(life_sat ~ log(gdp)+educ+unempl+demo+gini)
  summary(model1)  
  
  plot(model1$model$life_sat,residuals(model1))

# Breusch-Pagan test on model 1 
  library(lmtest)
  bptest(model1)
  # p-value=0,02663 => H0 (Homoscedasticity) rejected => Heteroscedasticity 
  # because of this, all of the following models are computed with robust standard errors
  
# model 1 with heteroscedasticity robust standard errors
  install.packages("estimatr")
  library(estimatr)
  model1_robust <- lm_robust(life_sat ~ log(gdp)+educ+unempl+demo+gini)
  summary(model1_robust)  
  # adj R^2 = 0,6937
  
# model without education
  model2 <- lm_robust(life_sat ~ log(gdp)+unempl+demo+gini)
  summary(model2)  
  # adj R^2 = 0,6968

# models with interaction terms 
  # log(gdp)*demo
  model3 <- lm_robust(life_sat ~ log(gdp)*demo+educ+unempl+gini)
  summary(model3)  #adj R^2 = 0,6926
  # log(gdp)*gini
  model4 <- lm_robust(life_sat ~ log(gdp)*gini+educ+unempl+demo)
  summary(model4) #adj R^2 = 0,6907
  # log(gdp)*unempl
  model5 <- lm_robust(life_sat ~ log(gdp)*unempl+educ+demo+gini)
  summary(model5)  #adj R^2 = 0,6933
  
# models with variables in nonlinear form 
  plot(life_sat ~ unempl) # skewed 
  plot(life_sat ~ log(unempl))
  model6 <- lm_robust(life_sat ~ log(gdp)+educ+log(unempl)+demo+gini)
  summary(model6)
  #adj R^2 = 0,7006

  # gini squared 
  model7 <- lm_robust(life_sat ~ log(gdp)+educ+unempl+demo+gini+I(gini^2))
  summary(model7)  # adj R^2 = 0,692
  
# combination of best models
  model8 <- lm_robust(life_sat ~ log(gdp)*log(unempl)+educ+demo+gini)
  summary(model8)  # adj R^2 = 0,6985
  
  model9 <- lm_robust(life_sat ~ log(gdp)+log(unempl)+demo+gini)
  summary(model8)  # adj R^2 = 0,7031
  

  