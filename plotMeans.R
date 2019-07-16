library("ggplot2")
library("dplyr")
library("tidyr")

df <- read.csv("socioPhoWorkData_20160716_fixed.csv",T)
summary(df)

df <- df[df$duration > .03,]

df.sydS <- df[df$corpus=="SydS",]

df.means <- df.sydS[df.sydS$vowel %in% c("BATH", "DRESS", "FOOT", "FORCE", "GOOSE", "KIT", "LOT", "NURSE", "START", "STRUT", "THOUGHT", "TRAP") & 
                      df.sydS$percentage=="50",] %>%
  group_by(vowel, grammatical, age) %>%
  summarise(F1_norm = mean(F1_norm),
            F2_norm = mean(F2_norm),
            F1 = mean(F1),
            F2 = mean(F2))

ggplot(df.means, aes(F2_norm, F1_norm, color=vowel))+
  geom_text(aes(label=vowel), size=8)+
  scale_x_reverse() + scale_y_reverse()+
  theme_bw() +
  facet_wrap(~age+grammatical, labeller=label_wrap_gen(multi_line=F))