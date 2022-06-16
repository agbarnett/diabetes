# 0_exclude_some_glu.R
# exclude some glu results depending on their date in relation to OGTT
# called by 0_read_data.R
# May 2022

# get dates of OGTT
ogtt_dates = filter(mother_glu, !is.na(glufasting)&!is.na(glu1)&!is.na(glu2)) %>%
  select(mother_id, collected) %>%
  rename('ogtt' = 'collected')

# loop through each woman
all_ids = unique(mother_data$mother_id)
new_glucose = NULL
for (this_id in all_ids){
#  this_id = '3608' # test
  this_woman = filter(mother_glu, mother_id == this_id) # get their glucose results
  this_ogtt = filter(ogtt_dates, mother_id == this_id) # get their OGTT dates
  if(nrow(this_ogtt) == 0){
    new_glucose = bind_rows(new_glucose, this_woman) # if no OGTT then add all glucose results
    next
  }
  for (k in 1:nrow(this_ogtt)){ # loop if there are multiple
    this_this_ogtt = this_ogtt[k,]
    this_compare = left_join(this_woman, this_this_ogtt, by='mother_id') %>%
      mutate(is_ogtt = !is.na(glufasting)&!is.na(glu1)&!is.na(glu2)) %>% # flag OGTT results
      filter(is_ogtt == TRUE | (is_ogtt == FALSE & collected <= ogtt & collected >= (ogtt-28))) %>% # within four weeks ?
      select(-ogtt, -is_ogtt) # no longer needed
    if(nrow(this_compare) > 0){
      new_glucose = bind_rows(new_glucose, this_compare) # add to glucose results
    }
  }
}
new_glucose = unique(new_glucose) # remove any duplicates