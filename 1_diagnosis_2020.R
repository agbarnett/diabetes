# 1_diagnosis_2020.R
# create the different diagnosis paths for gestational diabetes
# for 2020 data
# currently just using 1 imputed data set
# May 2022
library(dplyr)

# from 0_read_data.R
load('data/raw_data.RData')
mother_data = filter(mother_data, baby_birth_year == 2020) # just 2020 data

## see "GDM Screening_Diagnosis rules.docx"
diagnosis_2020 = left_join(left_join(left_join(mother_data, mother_glu_imputed[[1]], by = c('mother_id','preg_seq_id')), 
                       mother_fasting_imputed[[1]], by = c('mother_id','preg_seq_id')),
                       mother_a1c_imputed[[1]], by = c('mother_id','preg_seq_id')) %>%
  mutate(
    gdm = case_when(
      gdm_diagnosis == FALSE & glu < 4.7 & is.na(glufasting) & is.na(glu1) & is.na(glu2) ~ 'No diagnosis based on fasting blood glucose only',
      gdm_diagnosis == FALSE & (glu >= 4.7 & glu <=5.0) & (glufasting <=5.0 & glu1 <= 9.9 & glu2 <=8.4) ~ 'No diagnosis based on unequivocal fasting but negative OGTT',
      gdm_diagnosis == FALSE & is.na(glu) & (glufasting <=5.0 & glu1 <= 9.9 & glu2 <=8.4) ~ 'No diagnosis based on OGTT only',
      gdm_diagnosis == FALSE & is.na(glu) & is.na(a1c) & is.na(glufasting) & is.na(glu1) & is.na(glu2) ~ 'Missing',
      gdm_diagnosis == TRUE & glu > 5.0 & is.na(glufasting) & is.na(glu1) & is.na(glu2) ~ 'GDM diagnosis based on fasting blood glucose only',
      gdm_diagnosis == TRUE & (glu >= 4.7 & glu <=5.0) & (glufasting >5.0 | glu1 > 9.9 | glu2 > 8.4) ~ 'GDM diagnosis based on unequivocal fasting but positive OGTT',
      gdm_diagnosis == TRUE & is.na(glu) & is.na(a1c) & ((is.na(glufasting) & is.na(glu1) & is.na(glu2)) | (glufasting <=5.0 & glu1 <= 9.9 & glu2 <=8.4)) ~ 'GDM diagnosis method unknown',
      gdm_diagnosis == TRUE & glu > 5.0 ~ 'GDM diagnosis made on positive OGTT at any stage',
      gdm_diagnosis == TRUE & a1c >5.9 & is.na(glufasting) & is.na(glu1) & is.na(glu2)  ~ 'GDM diagnosis made by HbA1c only'
    ),
    gdm = ifelse(is.na(gdm), 'No result', gdm) # change missing
  )
table(diagnosis_2020$gdm) # quick check

# output checks, multiple reports per diagnosis
diagnosis = unique(diagnosis_2020$gdm) # 
for (diag_num in 1:length(diagnosis)){
  this_diagnosis = diagnosis[diag_num]
  outfile = paste('1_check_diagnosis_2020_', diag_num, '.docx', sep='')
  rmarkdown::render(input = "1_check_diagnosis_2020.Rmd",
                  output_format = "word_document",
                  output_file = outfile)
}

## save
save(diagnosis_2020, file='data/1_diagnosis_2020.RData')


