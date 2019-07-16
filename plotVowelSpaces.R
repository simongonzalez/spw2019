library(data.table)
library(ggplot2)

df <- fread('workingdf.csv')

#all speakers
ggplot(df, aes(F2, F1, color = vowel)) +
  geom_points(alpha = 0.6) + 
  stat_ellipse(geom = "polygon", alpha = 0.6)
scale_x_reverse() + scale_y_reverse() +
  facet_wrap(~grammatical) +
  theme_minimal()

#individual speakers
for(i in unique(df$speaker)){
  ggplot(df[df$speaker == i,], aes(F2, F1, color = vowel)) +
    geom_points(alpha = 0.6) + 
    stat_ellipse(geom = "polygon", alpha = 0.6)
  scale_x_reverse() + scale_y_reverse() +
    facet_wrap(~grammatical) +
    theme_minimal() 
  
  ggsave(paste0(i, '.png'))
}
