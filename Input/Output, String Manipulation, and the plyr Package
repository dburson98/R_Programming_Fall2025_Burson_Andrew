# Assignment 8 — Input/Output, Strings, plyr (NFL players CSV)
# Andrew Burson

# 1) Import ---------------------------------------------------------------
# Choose your NFL players .csv (NOT the .numbers file)
players <- read.csv(file.choose(), stringsAsFactors = FALSE, check.names = FALSE)

# (Optional) quick peek
names(players)[1:25]; head(players, 3)

# 2) Group + summarize with plyr -----------------------------------------
# install.packages("plyr")  # run once if needed
library(plyr)

# Average season by positionGroup
grouped_mean <- ddply(
  players,
  "positionGroup",
  summarise,
  MeanSeason  = mean(as.numeric(season), na.rm = TRUE),
  PlayerCount = sum(!is.na(season))
)

# 3) Write grouped means to a tab-separated text file --------------------
write.table(grouped_mean, "grouped_mean.txt", sep = "\t", row.names = FALSE)

# 4) Filter names containing "i" (case-insensitive) ----------------------
i_rows <- subset(players, grepl("i", displayName, ignore.case = TRUE))

# 5) Exports --------------------------------------------------------------
write.csv(i_rows$displayName, "i_names.csv", row.names = FALSE, quote = FALSE)
write.csv(i_rows,            "i_rows_full.csv", row.names = FALSE)


