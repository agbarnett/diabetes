# 99_non_bayes_model.R
# run the non-Bayesian versions of the models
# August 2022

# run the model
if(binomial_outcome==FALSE){
  model = lmer(as.formula(formula), data = for_analysis)
}
if(binomial_outcome==TRUE){
  model = glmer(as.formula(formula), data = for_analysis, family=binomial)
}

# get regression estimates for the 2020 group
# scale to percent changes for continuous
ests = broom.mixed::tidy(model, conf.int=TRUE)
if(binomial_outcome == TRUE | class(var) == 'logical'){
  ests = filter(ests, str_detect(term, 'group_2020')) 
}
if(binomial_outcome == FALSE & class(var) != 'logical'){
  int = filter(ests, str_detect(term, 'Intercept'), effect=='fixed') %>% pull(estimate)
  ests = filter(ests, str_detect(term, 'group_2020')) %>%
    mutate(estimate = 100* estimate  / int, # calculate percent change relative to intercept
           conf.low = 100* conf.low  / int,
           conf.high = 100* conf.high  / int) 
}
ests = select(ests, -effect, -group, -term) %>%
  mutate(outcome = outcome)

# check central limit theorem using bootstrap resampling
clt_plot = NULL
if(check_clt == TRUE){
  # bootstrap
  booted = NULL
  TeachingDemos::char2seed('newport')
  for (k in 1:n_boot){
    bootstrapped = sample_n(for_analysis, size=nrow(for_analysis), replace = TRUE)
    bmodel = lmer(as.formula(formula), data = bootstrapped)
    f = data.frame(fixef(bmodel)) %>%
      tibble::rownames_to_column()
    booted = bind_rows(booted, f)
  }
  names(booted) = c('variable','est')
  clt_plot = ggplot(data=booted, aes(x=est)) +
    geom_histogram(col='grey', fill='skyblue')+
    facet_wrap(~variable, scales='free')+
    theme_bw()
}