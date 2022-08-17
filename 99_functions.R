# 99_functions.R
# July 2022

# function to round with trailing zeros
roundz  = function(x, digits=0){formatC( round( x, digits ), format='f', digits=digits)}

# function to replace missing
replace_99 = function(x){
  x = ifelse(x==99, NA, x)
  x = ifelse(x==999, NA, x)
  x = ifelse(x==9999, NA, x)
  x = ifelse(x==99.8, NA, x) # for length at birth
}

# function to replace * in text
replace_star = function(x){
  x = ifelse(x=='?', '', x)
  x = ifelse(x=='*', '', x)
  x = ifelse(x=='***', '', x)
  x = ifelse(x=='< 0.5', '0.5', x)
  x = ifelse(x=='>14.0', '14.0', x)
}

# add text description from ICD code
add_icd = function(indata, var){
  newvar = paste(var, '_text', sep='') # new variable name
  index = which(names(indata) == var)
  names(indata)[index] = 'var' # rename merging variable
  # merge with longest available code
  indata = left_join(indata, icd_codes, by = c('var' = 'code'))
  
  ## then merge with shorter code (three numbers) if missing
  # truncate to letter and three digit code, example change: T3100 to T310
  not_missing = filter(indata, !is.na(text))
  missing = filter(indata, is.na(text)) %>%
    select(-text) %>% # will over-write
    mutate(var_short = str_sub(var, 1, 4)) %>% # shorter code
    left_join(short_codes_3, by=c('var_short' = 'short_code')) %>%
    select(-var_short)
  #
  indata = bind_rows(missing, not_missing)
  
  ## then merge with shorter code (two numbers) if missing
  not_missing = filter(indata, !is.na(text))
  missing = filter(indata, is.na(text)) %>%
    select(-text) %>% # will over-write
    mutate(var_short = str_sub(var, 1, 3)) %>% # shorter code
    left_join(short_codes_2, by=c('var_short' = 'short_code')) %>%
    select(-var_short)
  #
  indata = bind_rows(missing, not_missing)
  
  ## then merge with even shorter codes if still missing
  not_missing = filter(indata, !is.na(text))
  missing = filter(indata, is.na(text)) %>%
    select(-text) %>%
    mutate(var_short = str_sub(var, 1, 4)) %>%
    left_join(very_short_codes, by=c('var_short' = 'code_short')) %>%
    select(-var_short)
  #
  indata = bind_rows(missing, not_missing)
  
  # check for missing
  for_missing_check = filter(indata, var != '') # remove missing codes
  missing_n = sum(is.na(for_missing_check$text))
  prop_missing = missing_n / nrow(for_missing_check)
  if(prop_missing > 0.01){warning(paste('Over 1% missing (percentage = ', round(prop_missing*100), ').\n', sep=''))}
  
  # tidy up
  index = which(names(indata) == 'var')
  names(indata)[index] = var # rename back
  index = which(names(indata) == 'text')
  names(indata)[index] = newvar # update new name
  indata = arrange(indata, indata[,1]) 
  
  #
  return(indata)
}

# function to run model to compare 2019 and 2020
run_model = function(
    blind_outcome = TRUE, # blind results for 2020 vs 2019
    check_clt = FALSE, # check the central limit theorem for binary variables
    n_boot = 1000, # number of bootstraps
    binomial_outcome = FALSE,
    outcome = '',
    predictors = '',
    dp = 0, # decimal place for rounding
    mdata, # source of mother data
    bdata # source of baby data
){
  
  # don't check CLT when using logistic model
  #if(binomial_outcome==TRUE){check_clt = FALSE}
  
  # add data
  vars = c('mother_id','preg_seq_id', 'group_2020', outcome, predictors)
  mother_slim = select(mdata, any_of(vars)) # slim down
  bdata = select(bdata, any_of(vars))
  for_analysis = left_join(mother_slim, bdata, by=c('mother_id','preg_seq_id'))
  
  # summarise missing data
  for_missing = select(for_analysis, -contains('_id')) 
  missing = sapply(for_missing, function(x) sum(is.na(x)))
  missing = as.data.frame(missing) %>%
    tibble::rownames_to_column()
  names(missing) = c('Variable','Number missing')
  missing = flextable(missing) %>%
    theme_alafoli() %>%
    autofit()
  
  # blind 2020 results by random scrambling
  if(blind_outcome == TRUE){
    TeachingDemos::char2seed('colchester')
    for_analysis = select(for_analysis, -group_2020) %>%
      mutate(group_2020 = sample(c(0,1), size=n(), replace=TRUE))
  }
  
  ## Summary table or stats
  var = select(for_analysis, all_of(outcome))[,1] # just pick out the outcome
  if(binomial_outcome == TRUE | class(var) == 'logical'){
    stab = janitor::tabyl(var) %>%
      mutate(percent = round(percent*100)) %>%
      select(var, n, percent) # remove valid_percent if it is there
    names(stab)[1] = outcome
    stab = flextable(stab) %>%
      theme_box() %>%
      autofit()
  }
  if(binomial_outcome == FALSE & class(var) != 'logical'){
    stab = data.frame(min = min(var, na.rm = TRUE),
                      q1 = quantile(var, 0.25, na.rm = TRUE),
                      median = median(var, na.rm = TRUE),
                      q3 = quantile(var, 0.75, na.rm = TRUE),
                      max = max(var, na.rm = TRUE)) %>%
      mutate(min = roundz(min, dp),
             q1 = roundz(q1, dp),
             median = roundz(median, dp),
             q3 = roundz(q3, dp),
             max = roundz(max, dp)) %>%
      flextable() %>%
      theme_box() %>%
      autofit()
  }
  
  # make the formula
  formula_no_mother = paste(outcome, ' ~ group_2020 + ', paste(predictors, collapse = ' + '), sep='', collapse='')
  formula = paste(formula_no_mother, ' + (1|mother_id)', collapse = '', sep='') # add random intercept
  
  source('99_non_bayes_model.R')

  # return
  to_return = list()
  to_return$model = result$model
  to_return$table = result$table
  to_return$g2020 = result$ests
  to_return$summary = stab
  to_return$missing = missing
  to_return$clt_plot = clt_plot
  return(to_return)
}

# make file names for results and check if file already exists
make_file = function(infile, only_consent){
  ret = paste(paste(infile[1:2], collapse='/'), '.RData', sep='', collapse='')
  ret_short = paste(infile[2], '.RData', sep='', collapse='')
  exists = length(dir(infile[1], pattern = ret_short)) > 0
  #
  to_return = list()
  to_return$file = ret
  to_return$exists = exists
  return(to_return)
}


# function to run Bayesian model to compare 2019 and 2020
run_model_bayes = function(
    blind_outcome = TRUE, # blind results for 2020 vs 2019
    outcome = '',
    predictors = '',
    dp = 0, # decimal place for rounding
    mdata, # source of mother data
    bdata # source of baby data
){
  
  # add data
  vars = c('mother_id','preg_seq_id', 'group_2020', outcome, predictors)
  mother_slim = select(mdata, any_of(vars)) # slim down
  bdata = select(bdata, any_of(vars))
  for_analysis = left_join(mother_slim, bdata, by=c('mother_id','preg_seq_id'))
  
  # summarise missing data
  for_missing = select(for_analysis, -contains('_id')) 
  missing = sapply(for_missing, function(x) sum(is.na(x)))
  missing = as.data.frame(missing) %>%
    tibble::rownames_to_column()
  names(missing) = c('Variable','Number missing')
  missing = flextable(missing) %>%
    theme_alafoli() %>%
    autofit()
  
  # blind 2020 results by random scrambling
  if(blind_outcome == TRUE){
    TeachingDemos::char2seed('colchester')
    for_analysis = select(for_analysis, -group_2020) %>%
      mutate(group_2020 = sample(c(0,1), size=n(), replace=TRUE))
  }
  
  ## Summary table or stats
  var = select(for_analysis, all_of(outcome))[,1] # just pick out the outcome
  if(class(var) == 'logical'){
    stab = janitor::tabyl(var) %>%
      mutate(percent = round(percent*100)) %>%
      select(var, n, percent) # remove valid_percent if it is there
    names(stab)[1] = outcome
    stab = flextable(stab) %>%
      theme_box() %>%
      autofit()
  }
  if(class(var) != 'logical'){
    stab = data.frame(min = min(var, na.rm = TRUE),
                      q1 = quantile(var, 0.25, na.rm = TRUE),
                      median = median(var, na.rm = TRUE),
                      q3 = quantile(var, 0.75, na.rm = TRUE),
                      max = max(var, na.rm = TRUE)) %>%
      mutate(min = roundz(min, dp),
             q1 = roundz(q1, dp),
             median = roundz(median, dp),
             q3 = roundz(q3, dp),
             max = roundz(max, dp)) %>%
      flextable() %>%
      theme_box() %>%
      autofit()
  }
  
  # make the formula
  formula_no_mother = paste(outcome, ' ~ group_2020 + ', paste(predictors, collapse = ' + '), sep='', collapse='')
  formula = paste(formula_no_mother, ' + (1|mother_id)', collapse = '', sep='') # add random intercept
  
  # exclude missing data
  for_analysis = filter(for_analysis, complete.cases(for_analysis))
  
  # prepare the data 
  X = model.matrix(as.formula(formula_no_mother), data=for_analysis)
  P = ncol(X)
  M = length(unique(for_analysis$mother_id))
  constants <- list(N = nrow(for_analysis),
                    X = X,
                    P = P,
                    M = M,
                    mother = as.numeric(as.factor(for_analysis$mother_id)))
  data <- list(outcome = select(for_analysis, all_of(outcome))[,1])
  # for version without intercept
  constants_no_intercept <- list(N = nrow(for_analysis),
                                 X = X,
                                 P = P)
  
  # initial values
  inits <- list(alpha = rep(0, P), tau = 1, tau.mother = 2, r_int = rep(0,M))
  inits_no_intercept <- list(alpha = rep(0, P), tau = 1)
  
  # parameters to store
  parms = c('sd','sd.mother','alpha')
  parms_no_intercept = c('sd','alpha')
  
  # model
  model <- nimbleModel(code, 
                       data = data, 
                       constants = constants,
                       inits = inits)
  model_no_intercept <- nimbleModel(code_no_intercept, 
                                    data = data, 
                                    constants = constants_no_intercept,
                                    inits = inits_no_intercept)
  
  # MCMC choices
  source('99_mcmc.R')
  
  # samples
  mcmc_out <- nimbleMCMC(model = model,
                         inits = inits,
                         monitors = parms,
                         niter = MCMC*2*thin, 
                         thin = thin,
                         nchains = n.chains, 
                         nburnin = MCMC,
                         summary = TRUE, 
                         setSeed = seed,
                         WAIC = TRUE)
  mcmc_out_no_intercept <- nimbleMCMC(model = model_no_intercept,
                                      inits = inits_no_intercept,
                                      monitors = parms_no_intercept,
                                      niter = MCMC*2*thin, 
                                      thin = thin,
                                      nchains = n.chains, 
                                      nburnin = MCMC,
                                      summary = TRUE, 
                                      setSeed = seed,
                                      WAIC = TRUE)
  
  # convert chains to coda
  mcmc = as.mcmc(mcmc_out$samples$chain1) # can only do one chain
  mcmc_no_intercept = as.mcmc(mcmc_out_no_intercept$samples$chain1) # can only do one chain
  n_eff = effectiveSize(mcmc) # Effective sample size for estimating the mean
  n_eff_no_intercept = effectiveSize(mcmc_no_intercept) # Effective sample size for estimating the mean
  # to do: posterior p-value?
  
  ## make a nice table
  # with intercept
  table = as.data.frame(mcmc_out$summary$all.chains) %>%
    tibble::rownames_to_column() %>%
    bind_cols(n_eff) %>%
    mutate(rowname = case_when(
      rowname == 'alpha[1]' ~ 'Intercept',
      rowname == 'alpha[2]' ~ 'group_2020',
      TRUE ~ as.character(rowname)
    ))
  names(table) = c('var','mean','median','sd','lower','upper','n_eff')
  # without intercept
  table_no_intercept = as.data.frame(mcmc_out_no_intercept$summary$all.chains) %>%
    tibble::rownames_to_column() %>%
    bind_cols(n_eff_no_intercept) %>%
    mutate(rowname = case_when(
      rowname == 'alpha[1]' ~ 'Intercept',
      rowname == 'alpha[2]' ~ 'group_2020',
      TRUE ~ as.character(rowname)
    ))
  names(table_no_intercept) = c('var','mean','median','sd','lower','upper','n_eff')
  # rename var for predictors
  n_pred = length(predictors)
  old_names = paste('alpha[', 3:(3+n_pred-1),']', sep='')
  for (k in 1:n_pred){
    index = table$var == old_names[k]
    table$var[index] = predictors[k]
    index = table_no_intercept$var == old_names[k]
    table_no_intercept$var[index] = predictors[k]
  }
  # combine tables
  tables = bind_rows(table, table_no_intercept, .id = 'with_intercept') %>%
    mutate(with_intercept = with_intercept == 1)
  
  # key estimates
  ests = filter(tables, var=='group_2020')
  
  # neaten tables
  table_neat = mutate(tables, mean = roundz(mean, dp),
                 lower = roundz(lower, dp),
                 upper = roundz(upper, dp),
                 n_eff = roundz(n_eff, 0),
                 ci = paste(lower, ' to ', upper, sep='')) %>%
    select(with_intercept, var, mean, ci, n_eff)
  
  # model fit comparing models with and without intercept
  f1 = data.frame(outcome = outcome, model = 'With intercept', pWAIC = mcmc_out$WAIC$pWAIC, WAIC = mcmc_out$WAIC$WAIC)
  f2 = data.frame(outcome = outcome, model = 'Without intercept', pWAIC = mcmc_out_no_intercept$WAIC$pWAIC, WAIC = mcmc_out_no_intercept$WAIC$WAIC)
  model_fit = bind_rows(f1, f2)
  
  # return
  to_return = list()
  to_return$model = mcmc_out_no_intercept
  to_return$model_fit = model_fit
  to_return$table = tables
  to_return$table_neat = table_neat
  to_return$g2020 = ests
  to_return$summary = stab
  to_return$missing = missing
  return(to_return)
}

# make file names for results and check if file already exists
make_file = function(infile, only_consent){
  ret = paste(paste(infile[1:2], collapse='/'), '.RData', sep='', collapse='')
  ret_short = paste(infile[2], '.RData', sep='', collapse='')
  exists = length(dir(infile[1], pattern = ret_short)) > 0
  #
  to_return = list()
  to_return$file = ret
  to_return$exists = exists
  return(to_return)
}

# make nice table for regression estimates for bayes model
bayes_table = function(intable, dp=0){
  intable = mutate(intable,
                 mean = roundz(mean, dp),
                 lower = roundz(lower, dp),
                 upper = roundz(upper, dp),
                 n_eff = roundz(n_eff, 0),
                 ci = paste(lower, ' to ', upper, sep=''))  %>%
    select(var, mean, ci, n_eff)
  return(intable)
}

# make percent change
make_percent_change = function(intable){
  intercept = filter(intable, str_detect(var, 'Intercept')) %>% pull(mean)
  group_2020 = filter(intable, str_detect(var, 'group_2020')) %>%
    mutate(mean = 100*mean/intercept,
           lower = 100*lower/intercept,
           upper = 100*upper/intercept)
  return(group_2020)
}