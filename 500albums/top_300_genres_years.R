library(dplyr)

df <- read.csv("500albums/albumlist.csv")
new_df <- as.data.frame(xtabs(~ Genre + Year, data=df))
top_300_genres_years = head(arrange(new_df, desc(Freq)), 300)
