df <- read.csv("albumlist.csv")
xt <- xtabs(~ Genre + Year, data=df)
new_df <- as.data.frame <- (xt)
top_300_genres_years = head(arrange(new_df, desc(Freq)), 300)
