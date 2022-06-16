# 99_functions.R
# April 2022

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
