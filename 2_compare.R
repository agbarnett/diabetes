# 2_compare.R
# Compare perinatal complications between 2020 group with had NO GDM based on a FBGL <4.7mmol/l 
# with 2019 group with NO GDM with an OGTT. 
# Control for confounding with data available. 
# July 2022
library(dplyr)
library(lme4) # for model with random intercept
library(stargazer) # for nice regression tables

## full data
load('data/raw_data.RData') # from 0_read_data.R

## get the mother data with new diagnosis
# 2019 group
load('data/1_diagnosis_2019.RData')
g2019 = filter(diagnosis_2019, overall == 'No') %>%
  select(-overall)
# 2020 group
load('data/1_diagnosis_2020.RData')
g2020 = filter(diagnosis_2020, gdm == 'No diagnosis based on fasting blood glucose only') %>%
  select(-gdm)
# combine and create 2020 cohort
mother = bind_rows(g2019, g2020, .id = 'group') %>%
  mutate(c2020 = case_when(
    group == 1 ~ 0,
    group == 2 ~ 1
  ),
  age = baby_birth_year - mother_birth_year, # mother's age
  age10 = (age - 30)/10, # scaling
  body_mass_index5 = (body_mass_index - 25)/5) %>%
  select(-group)

# function to run model
run_model = function(
    outcome ='',
    predictors = '',
    bdata # source of baby data
    ){
  
  # add data
  vars = c('mother_id','preg_seq_id', 'c2020', outcome, predictors)
  mother_slim = select(mother, any_of(vars)) # slim down
  bdata = select(bdata, any_of(vars))
  for_analysis = left_join(mother_slim, bdata, by=c('mother_id','preg_seq_id'))
  # run regression model
  formula = paste(outcome, ' ~ c2020 + ', paste(predictors, collapse = ' + '), sep='', collapse='')
  formula = paste(formula, ' + (1|mother_id)', collapse = '', sep='') # add random intercept
  model = lmer(as.formula(formula), data = for_analysis)
  # nice table
  tab = stargazer(model, type='html')
  
  # get confidence intervals
  ci = confint(model)
  
  # get within mother correlation
  
  return(model)
}

# compare outcomes
m1 = run_model(outcome = 'birth_weight',
      predictors = c('age10','body_mass_index5'),
      bdata = baby_data)
summary(m1)


# adjust using Nina's list
