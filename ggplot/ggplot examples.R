rm(list = ls())
for(x in c("ggplot2", "dplyr")) stopifnot(require(x, character.only = T))

ctd <- read.csv("../multiYearCTD.csv", stringsAsFactors = FALSE)
df <- ctd[ctd$station == "I12" & grepl("2015", ctd$sample_date), ]
df <- df[order(df$depth_meter), ]

p <- ggplot(df, aes(temp, depth_meter)) +
  geom_path() +
  scale_y_reverse() +
  facet_wrap(~ quarter, nrow = 1, scales = "free") +
  labs(x = "Temperature", y = "Depth")
print(p)


new.df <- ctd[ctd$station %in% c("I12", "I14", "I16") & grepl("2015", ctd$sample_date), ]
new.df <- new.df[order(new.df$depth_meter), ]

st.cols <- c(I16 = "brown", I14 = "yellow", I12 = "red")
p <- ggplot(new.df, aes(temp, depth_meter, color = station)) +
  geom_point() +
  scale_y_reverse() +
  scale_color_manual(values = st.cols) +
  facet_wrap(~ quarter) +
  labs(x = "Temperature", y = "Depth")
print(p)
