#Sunny, Mars, Tony
# Final project data wrangling
library(dplyr)

tiktok_transcriptionSet <- read.csv("tiktok_dataset 2.csv")
tiktok_bgmSet <- read.csv("top_users_vids.csv")

# Convert `video_like_count` to numeric, handling `#N/A` values
tiktok_transcriptionSet <- tiktok_transcriptionSet %>%
  mutate(video_like_count = as.numeric(replace(video_like_count, video_like_count == "#N/A", NA)))

# Ensure that `video_comment_count` is filtered properly
tiktok_transcriptionSet <- tiktok_transcriptionSet %>%
  filter(!is.na(video_like_count), author_ban_status == "active", video_comment_count != 0)

tiktok_bgmSet <- select(tiktok_bgmSet, user_name, video_length, n_likes, n_shares, n_comments, n_plays, hashtags, song)
# tiktok_bgmSet <- filter(tiktok_bgmSet, video_length >= 10)

tiktok_df <- left_join(tiktok_transcriptionSet, tiktok_bgmSet, by = c("video_like_count" = "n_likes"))
tiktok_df <- filter(tiktok_df, !is.na(song))