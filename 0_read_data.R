# 0_read_data.R
# read the GDM data; impute missing confinement date to estimate gestation time for glucose and a1c tests
# May 2022
library(dplyr)
options(dplyr.summarise.inform = FALSE)
library(janitor)
library(readxl)
library(tidyr)
library(stringr)
source('99_functions.R')
n_imp = 10 # number of imputed data sets

## get the ICD codes, from https://github.com/k4m1113/ICD-10-CSV/blob/master/codes.csv
# I added a few to the end of the csv file
icd_codes = read.csv('data/codes.csv', header = FALSE) # 
names(icd_codes) = c('code_short','unknown','code','text','text2','text3')
# make short code version (three numbers)
short_codes_3 = mutate(icd_codes, short_code = str_sub(code, 1, 4)) %>% # shorter code
  group_by(short_code) %>%
  arrange(short_code, code) %>%
  slice(1) %>% # remove duplicates after shortening code
  ungroup() %>%
  select(short_code, text)
# make short code version (two numbers)
short_codes_2 = mutate(icd_codes, short_code = str_sub(code, 1, 3)) %>% # shorter code
  group_by(short_code) %>%
  arrange(short_code, code) %>%
  slice(1) %>% # remove duplicates after shortening code
  ungroup() %>%
  select(short_code, text)
# make even shorter version
very_short_codes = group_by(icd_codes, code_short) %>%
  arrange(code_short, unknown) %>%
  slice(1) %>% # remove duplicates after shortening code
  ungroup() %>%
  select(code_short, text)
# long version
icd_codes = select(icd_codes, code, text)

## read the data that was provided
# find and loop through all csv files
na.strings = c('NA','N/A')
ddir = dir('data', pattern='.csv')
names = str_remove_all(ddir, '.csv')
raw_data = list()
for (i in seq_along(ddir)){
  infile = paste('data/', ddir[i], sep='')
  raw_data[[i]] = read.csv(file = infile, na.strings = na.strings) %>%
    clean_names() %>%
    mutate(source = names[i]) %>%
    mutate_if(is.numeric, replace_99) %>% # replace all 99 and 999 with missing
    mutate_if(is.character, replace_star) # replace odd characters in some numerical results that should be text
}
# now make list into individual data frames
names(raw_data) = names
list2env(raw_data, globalenv())

## can combine mother_glu and mother_glu_upd
mother_glu = bind_rows(mother_glu, mother_glu_upd) %>%
  unique() # remove small number of duplicates

## remove three mothers with missing id numbers
mother_data = filter(mother_data, !is.na(mother_id))

## data edits
baby_data = mutate(baby_data,
                   gestation = gestation_weeks + (gestation_days/7)) %>% # combine two gestation variables
  select(-starts_with('gestation_'), -fluid_last_24h_other_types_fluid) # no longer needed gestation; remove one variable with almost no variation
## convert text to number (and other edits)
#
mother_glu = mutate(mother_glu,
                    gestation = ifelse(gestation > 43, NA, gestation), # remove a few errors in gestation times
                    collected = as.Date(collected, '%d/%m/%Y'),
                    glufasting = as.numeric(glufasting),
                    glu1 = as.numeric(glu1),
                    glu2 = as.numeric(glu2),
                    glu = as.numeric(glu),
                    glubg = as.numeric(glubg),
                    gload = str_remove_all(gload, pattern='[^0-9]'),
                    gload = as.numeric(gload),
                    gload = ifelse(gload ==0 | gload > 500, NA, gload)) %>% # remove a few errors 
  select(-glu50, -rsug, -glufl, -gluru, # missing for all rows, glufl and gluru almost completely missing
         -gttbdt, -gttptp) %>% # not using comments
  unique() # remove a small number of duplicates
# remove exclusions (serious disease) for glucose (also used for A1C)
to_exclude = c('ALL','AML-M3','BMT','CF','Chemotherapy','Warfarin Tx')
mother_glu = filter(mother_glu,
            !alert %in% to_exclude) %>% # collected already in date format
  select(-alert)

#
mother_a1c = mutate(mother_a1c,
                    collected = as.Date(collected, '%d/%m/%Y'),
                    a1cpoc = as.numeric(a1cpoc)) %>%
  select(-a1cp, -a1cd) %>% # missing for all rows
  unique() # remove a small number of duplicates
mother_a1c = filter(mother_a1c,
                    !a1c_1_alert %in% to_exclude) %>% # remove serious diseases
  select(-a1c_1_alert)

## add ICD codes to some data 
baby_lab_deliv_compln = add_icd(indata = baby_lab_deliv_compln, var = 'labour_deliv_complicatn')
baby_neonatal_morb = add_icd(indata = baby_neonatal_morb, var = 'neonatal_morbidity_code')
mother_preg_complicatn = add_icd(indata = mother_preg_complicatn, var = 'pregnancy_complication')
mother_existing_medi_condtns = add_icd(indata = mother_existing_medi_condtns, var = 'medical_condition')
mother_antnat_tfr = add_icd(indata = mother_antnat_tfr, var = 'tfr_reason')
baby_data = add_icd(indata = baby_data, var = 'reason_caesarean_main')
baby_data = add_icd(indata = baby_data, var = 'reason_forceps_vacuum')
baby_data = add_icd(indata = baby_data, var = 'reason_icn_scn_code')
# check missing
# filter(baby_data, is.na(reason_icn_scn_code_text)) %>% group_by(reason_icn_scn_code) %>% tally() %>% arrange(-n)

## split based on baby's birth date - used to group data
baby_data = mutate(baby_data, 
                   baby_birth_year = as.numeric(str_remove_all(baby_birth_date, '[^0-9]'))) %>% # just keep year
  select(-baby_birth_date)
# add to mother data
baby_small = select(baby_data, mother_id, preg_seq_id, baby_birth_year) %>%
  unique() # for twins
mother_data = left_join(mother_data, baby_small, by = c('mother_id', 'preg_seq_id'))

## GDM diagnosis based on ICD codes
codes = c('O24','O240','O241','O242','O243','^O244')
codes = paste(codes, collapse='|')
mother_preg_complicatn = mutate(mother_preg_complicatn, 
                   gdm_diagnosis = str_detect(pregnancy_complication, codes)) # this covers the codes "O2444" "O2443" "O2442" "O240"  "O2429" "O2419" "O2439" "O2449"
# add GDM diagnosis to mother data
gdm = filter(mother_preg_complicatn, gdm_diagnosis == TRUE) %>% # just where there is a positive diagnosis (some pregnancies can have both)
  select(mother_id, preg_seq_id, gdm_diagnosis) %>%
  unique() # just GDM per pregnancy
mother_data = left_join(mother_data, gdm, by=c('mother_id', 'preg_seq_id')) %>%
  mutate(gdm_diagnosis = ifelse(is.na(gdm_diagnosis), FALSE, gdm_diagnosis)) # fill in missing
mother_preg_complicatn = select(mother_preg_complicatn, -gdm_diagnosis) # no longer needed in this data

# check glucose data with missing gestation
source('0_check_missing.R')

# Fix two dates that must be data entry errors
mother_data = mutate(mother_data, 
              est_confinment_date = ifelse(str_detect(est_confinment_date, pattern='OCT-2021'), NA, est_confinment_date),
              est_confinment_date = ifelse(str_detect(est_confinment_date, pattern='-2001'), NA, est_confinment_date))

## exclude some glu results depending on their date in relation to OGTT
source('0_exclude_some_glu.R')

## impute missing days for mothers confinement date for glucose tests
#
baby_gest = select(baby_data, mother_id, preg_seq_id, gestation) %>% # can have multiple rows for twins, etc
  unique() # just take one per multiple births
#
mother_glu_imputed = mother_a1c_imputed = mother_fasting_imputed = list()
TeachingDemos::char2seed('exeter')
for (k in 1:n_imp){ # loop through imputations
  # a) first impute missing confinement day (month and year are available)
  this_mother = mutate(mother_data,
                        month = str_sub(est_confinment_date, 1, 3),
                        new_day = case_when(
                          month == 'FEB' ~ sample(1:28, size=n(), replace=TRUE), 
                          month %in% c('APR', 'JUN', 'SEP', 'NOV') ~ sample(1:30, size=n(), replace=TRUE), 
                          TRUE ~ sample(1:31, size=n(), replace=TRUE)
                        ),
                        est_confinment_date = paste(new_day, '-', est_confinment_date, sep=''),
                        est_confinment_date = as.Date(est_confinment_date, format = '%d-%b-%Y')
  ) %>%
    select(mother_id, preg_seq_id, est_confinment_date, baby_birth_year) %>%
    left_join(baby_gest, by=c('mother_id', 'preg_seq_id')) %>% # add final gestation
    mutate(conception_date = est_confinment_date - gestation*7) %>% # estimate conception date
    rename('gestation_final' = 'gestation') # for merge
  
  # b) then add to glucose
  this_glucose = left_join(mother_glu, this_mother, by='mother_id') %>% # merging by mother ID creates multiple matches (glucose does not have pregnancy ID)
    mutate(gestation_est = as.numeric(collected - conception_date)/7, # estimate gestation based on confinement in weeks
           diff = gestation_est - gestation) %>% # check difference
    filter(collected <= est_confinment_date, # must be before birth
           gestation_est >= 0,
           gestation_est <= 39)
  # now find any high test per woman
  any_high = group_by(this_glucose, mother_id, preg_seq_id) %>%
    summarise(glufasting = max(glufasting, na.rm=TRUE),
              glu1 = max(glu1, na.rm=TRUE),
              glu2 = max(glu2, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate_all(function(x) ifelse(is.nan(x) | is.infinite(x), NA, x)) # remove Inf
  
  # repeat for early ogtt
  any_high_early = filter(this_glucose, gestation_est < 24) %>% # before 24 weeks
    group_by(mother_id, preg_seq_id) %>%
    summarise(early_glufasting = max(glufasting, na.rm=TRUE),
              early_glu1 = max(glu1, na.rm=TRUE),
              early_glu2 = max(glu2, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate_all(function(x) ifelse(is.nan(x) | is.infinite(x), NA, x)) # remove Inf
  # combine ogtt and early ogtt
  combined = full_join(any_high, any_high_early, by=c('mother_id','preg_seq_id'))
  mother_glu_imputed[[k]] = combined
  
  # c) then add to a1c
  this_a1c = left_join(mother_a1c, this_mother, by='mother_id') %>% # merging by mother ID creates multiple matches (glucose does not have pregnancy ID)
    mutate(gestation_est = as.numeric(collected - conception_date)/7) %>% # estimate gestation based on confinement in weeks
    filter(collected <= est_confinment_date, # must be before birth
           gestation_est >= 0,
           gestation_est < 13) # this test must be before 13 weeks
  # now find any high test per woman
  any_high_a1c = group_by(this_a1c, mother_id, preg_seq_id, est_confinment_date ) %>%
    summarise(a1c = max(a1c, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate_all(function(x) ifelse(is.nan(x) | is.infinite(x), NA, x)) # remove Inf
  mother_a1c_imputed[[k]] = any_high_a1c
  
  # d) then add to fasting glucose (2020 only)
  this_glucose_fasting = left_join(mother_glu, this_mother, by='mother_id') %>% # merging by mother ID creates multiple matches (glucose does not have pregnancy ID)
    mutate(gestation_est = as.numeric(collected - conception_date)/7) %>% # estimate gestation based on confinement in weeks
    filter(baby_birth_year == 2020, # 2020 only
           collected <= est_confinment_date, # must be before birth
           gestation_est >= 0,
           gestation_est < 13) # this test must be before 13 weeks
  ## now find any high test per woman; must precede ogtt results
  # first find earliest ogtt result per woman/pregnancy
  earliest_ogtt = group_by(this_glucose_fasting, mother_id, preg_seq_id) %>%
    mutate(ogtt_result = !is.na(glu1)|!is.na(glu2)|!is.na(glufasting)) %>%
    filter(ogtt_result == TRUE) %>%
    arrange(mother_id, preg_seq_id, collected) %>% 
    slice(1) %>%
    ungroup() %>%
    select(mother_id, preg_seq_id, collected ) %>%
    rename('earliest_ogtt' = 'collected')
  # now only look before OGTT
  any_high_glufasting = left_join(this_glucose_fasting, earliest_ogtt, by=c('mother_id','preg_seq_id') ) %>%
    filter(is.na(earliest_ogtt) | collected < earliest_ogtt) %>% # either missing OGTT or collected prior to OGTT
    group_by(mother_id, preg_seq_id ) %>%
    summarise(glu = max(glu, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate_all(function(x) ifelse(is.nan(x) | is.infinite(x), NA, x)) # remove Inf
  mother_fasting_imputed[[k]] = any_high_glufasting
}


## data dictionary
dictionary = read_excel('data/summary32687_updated.xlsx', skip = 25, col_names = TRUE) %>%
  clean_names() %>%
  mutate(csv = str_detect(data_items, pattern='.csv'), # find .csv that indicates new data
         source = ifelse(csv == TRUE, data_items, NA),
         source = str_remove_all(source, '.csv')) %>%
  fill(source) %>% # carry forward data source
  filter(csv == FALSE, # remove rows
         !is.na(data_items) , # remove missing
         description != 'Description') %>% # remove header rows 
  select(-csv)


## save all the data frames
save(list=c(names, 'dictionary', 'n_imp', 'mother_glu_imputed', 'mother_a1c_imputed', 'mother_fasting_imputed'), file='data/raw_data.RData')
