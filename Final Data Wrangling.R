#Sunny, Mars, Tony
# Final project data wrangling

library(dplyr)

sug_users_vids <- read.csv('sug_users_vids1.csv')
top_users_vids <- read.csv('top_users_vids.csv')


clean_username <- function(user_name) {
  user_name <- tolower(trimws(user_name)) 
  return(user_name)
}

sug_users_vids$user_name <- sapply(sug_users_vids$user_name, clean_username)
top_users_vids$user_name <- sapply(top_users_vids$user_name, clean_username)

sug_users_vids$duration_category <- cut(sug_users_vids$video_length, 
                                        breaks = c(0, 15, 35, Inf), 
                                        labels = c("Short", "Medium", "Long"), 
                                        right = FALSE)

top_users_vids_unique <- top_users_vids %>%
  group_by(user_name) %>%
  summarize(across(everything(), first))

tiktok_df <- left_join(sug_users_vids, top_users_vids_unique, by = "user_name")

tiktok_df <- filter(tiktok_df, !is.na(user_name))

tiktok_df <- filter(tiktok_df, !is.na(video_length.x), !is.na(video_length.y))

song_y_index <- which(names(tiktok_df) == "song.y")

# Keep only columns up to and including 'song.y'
tiktok_df <- tiktok_df[, 1:song_y_index]


write.csv(tiktok_df, 'tiktok_df.csv', row.names = FALSE)

like_count <- nrow(tiktok_df[tiktok_df$n_likes.x > 10000, ])

popular_music <- names(which.max(table(top_users_vids$song)))
print(paste("The most popular music is:", popular_music))
