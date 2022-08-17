# 1_diagnosis_2019.R
# create the different diagnosis paths for gestational diabetes
# for 2019 data
# currently just using 1 imputed data set
# July 2022
library(dplyr)

# from 0_read_data.R
load('data/raw_data.RData')
mother_data = filter(mother_data, baby_birth_year == 2019) # just 2019 data

# see "GDM Screening_Diagnosis rules.docx"
# a) ogtt
ogtt_diagnosis = left_join(mother_data, mother_glu_imputed[[1]], by = c('mother_id','preg_seq_id')) %>%
  mutate(ogtt = case_when(
    glufasting > 5.0 ~ 'Yes',
    glu1 > 9.9 ~ 'Yes',
    glu2 > 8.4 ~ 'Yes',
    is.na(glufasting) & is.na(glu1) & is.na(glu2) ~ NA_character_,
    TRUE ~ 'No'
  )) %>% # add any mother variables used in 2_compare.Rmd
  select('mother_id','preg_seq_id', 'facility_name','baby_birth_year', 
         'body_mass_index', 'mother_birth_year',
         'nr_prev_caesar', 'prev_pregnancies_flag',
         "smoked_before_20wks", "smoked_after_20wks",
         'glufasting', 'glu1','glu2','ogtt', 'gdm_diagnosis') # keep a few additional variables in this data
# b) early ogtt
early_ogtt_diagnosis = left_join(mother_data, mother_glu_imputed[[1]], by = c('mother_id','preg_seq_id')) %>%
  mutate(ogtt_early = case_when(
    early_glufasting > 5.0 ~ 'Yes',
    early_glu1 > 9.9 ~ 'Yes',
    early_glu2 > 8.4 ~ 'Yes',
    is.na(early_glufasting) & is.na(early_glu1) & is.na(early_glu2) ~ NA_character_,
    TRUE ~ 'No'
  )) %>%
  select('mother_id','preg_seq_id', contains('early'))
# c) a1c
a1c_diagnosis = left_join(mother_data, mother_a1c_imputed[[1]], by = c('mother_id','preg_seq_id')) %>%
  mutate(a1c_diag = case_when(
    a1c > 5.9 ~ 'Yes',
    is.na(a1c) ~ NA_character_,
    TRUE ~ 'No'
  )) %>%
  select('mother_id','preg_seq_id', 'a1c', 'a1c_diag')
  
## merge diagnosis
diagnosis = full_join(full_join(ogtt_diagnosis, early_ogtt_diagnosis, by= c('mother_id','preg_seq_id')),
                      a1c_diagnosis, by= c('mother_id','preg_seq_id'))
# make variables
# "case_when arguments are evaluated in order, so you must proceed from the most specific to the most general"
diagnosis_2019 = mutate(diagnosis, 
    early_gdm = case_when(
      a1c_diag == 'Yes' | ogtt_early == 'Yes' ~ 'Yes',
      is.na(a1c_diag) & is.na(ogtt_early) ~ NA_character_,
      TRUE ~ 'No'),
    gdm = case_when(
      ogtt == 'Yes' ~ 'Yes',
      is.na(ogtt) ~ NA_character_,
      TRUE ~ 'No'),
    overall = case_when(
      early_gdm == 'Yes' ~ 'Early GDM', # this takes precedence
      gdm == 'Yes' ~ 'GDM',
      gdm == 'No' | early_gdm == 'No' ~ 'No', # using OR
      gdm_diagnosis == TRUE & is.na(gdm) == TRUE ~ 'GDM diagnosis with no glucose data',
      gdm_diagnosis == FALSE & is.na(gdm) == TRUE ~ 'No GDM diagnosis with no glucose data'
    )
  )

# output checks
rmarkdown::render(input = "1_check_diagnosis_2019.Rmd",
                  output_format = "word_document",
                  output_file = '1_check_diagnosis_2019.docx')

## save
save(diagnosis_2019, file='data/1_diagnosis_2019.RData')

