library("ggplot2")
library("dplyr")
library("tidyr")

df <- read.csv("socioPhoWorkData_20160716_fixed.csv",T)

mono <- df[df$vowel %in% c("BATH", "DRESS", "FOOT", "FORCE", "GOOSE", "KIT", "LOT", "NURSE", "START", "STRUT", "THOUGHT", "TRAP"),]

#Plot speaker vowel spaces, separate facets for content and grammatical words
#Unnormalised data
speaker_list_mono <- unique(mono$speaker) #Creates string of unique entries for speakers
for(i in seq_along(speaker_list_mono)){
  plot <- 
    ggplot(subset(mono, mono$speaker==speaker_list_mono[i]),
           aes(F2, F1, color=vowel))+
    geom_point()+
    stat_ellipse()+
    theme_bw()+
    scale_x_reverse()+scale_y_reverse() +
    facet_wrap(~grammatical)+
    ggtitle(paste(speaker_list_mono[i]))
  ggsave(plot, file=paste(speaker_list_mono[i], ".png", sep=''), scale=2)
  print(plot) #if you want to print the plots within R
}
