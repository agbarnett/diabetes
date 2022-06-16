# 99_temp.R
# temporary code

### for `# then add to glucose` in 0_read_data.R

double = select(this_glucose, mother_id)
doubles = double$mother_id[duplicated(double)] %>% # keep duplicates
  unique()

# example
filter(this_glucose, mother_id==sample(doubles,1))

