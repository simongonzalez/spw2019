library(data.table)

#reads data
df <- fread('data.csv')

#subsets to 50% (midpoint)
df <- df[df$percentage == 50,]

#list of monopthongs
monoph <- unlist(strsplit('FLEECE TRAP KIT LOT MOUTH STRUT GOOSE DRESS FORCE BATH NURSE START THOUGHT FOOT', ' '))

#subsets to mpnophthongs
df <- df[df$vowel %in% monoph,]

#duration filter
df <- df[df$dur >= 0.03,]

#saves data
write.csv(df, 'workingdf.csv', row.names = F)
