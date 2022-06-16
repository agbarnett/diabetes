# 0_check_missing.R
# checks of missing glucose data; called by 0_read_data.R
# April 2022

library(ggplot2)
for_plot = filter(mother_glu, !is.na(glu1)) %>%
  mutate(gmiss = factor(as.numeric(is.na(gestation))))
mplot = ggplot(data=for_plot, aes(x=gmiss, y=glu1))+
  geom_boxplot(fill='skyblue')+
  theme_bw()+
  scale_x_discrete(labels=c('No','Yes'))+
  xlab('Missing gestation')
mplot
