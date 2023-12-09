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

# Adding new categorical variable
tiktok_transcriptionSet$duration_category <- cut(tiktok_transcriptionSet$video_duration_sec, 
                                        breaks = c(0, 15, 60, Inf), 
                                        labels = c("Short", "Medium", "Long"), 
                                        right = FALSE)

# Adding new continuous/numerical variable
tiktok_transcriptionSet$engagement_rate <- (as.numeric(tiktok_transcriptionSet$video_like_count) +
                                              as.numeric(tiktok_transcriptionSet$video_comment_count)) /
                                              as.numeric(tiktok_transcriptionSet$video_view_count)

tiktok_bgmSet <- select(tiktok_bgmSet, user_name, video_length, n_likes, n_shares, n_comments, n_plays, hashtags, song)

tiktok_df <- left_join(tiktok_transcriptionSet, tiktok_bgmSet, by = c("video_like_count" = "n_likes"))
tiktok_df <- filter(tiktok_df, !is.na(song))

write.csv(tiktok_df, "tiktok.csv", row.names=TRUE)

like_count <- nrow(tiktok_df[tiktok_df$video_like_count > 10000, ])

popular_music <- names(which.max(table(tiktok_bgmSet$song)))
print(paste("The most popular music is:", popular_music))
