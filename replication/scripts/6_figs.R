# This script needs to run "data_prep before"

#Load packages
library(here)

#Specify your path
source(here("/scripts/data_prep.R"))

# Figure 1: anatomy of threads ----

# Group the data by thread_id
posts_time_df <- all_text_df %>%
  mutate(timestamp  = as.POSIXct(datetime)) %>%
  arrange(thread_id, timestamp, title_clean) %>%
  group_by(thread_id) %>%
  mutate(first_post_time = first(timestamp)) %>%
  # Calculate response time for each post to the first post
  mutate(response_time = as.numeric(timestamp - first_post_time)) %>%
  mutate(response_time_days = response_time / (3600 * 24), 
         response_time_hours = response_time / 3600) %>% 
  # Filter out the first post (as response time to itself would be 0)
  filter(text_id != 0) %>% 
  ungroup()


a <- ggplot(posts_time_df, aes(response_time_hours)) +
  geom_histogram(bins = 500) + 
  theme_ipsum() + 
  theme(plot.margin = margin(0.2, 0.5, 0.5 , 0.5, "cm")) +
  geom_vline(xintercept = median(posts_time_df$response_time_hours), 
             col = "red", linetype = "dashed", lwd = 0.6) +
  labs(x = "Response time to the opening post in hours", 
       y = "number of threads", 
       title = "Anatomy of conversations in the r/Fundrise corpus", 
       caption = "The dashed red line indicates the median",
       subtitle = "Short-lived: most threads expire after a day") + 
  
  scale_x_continuous(breaks = seq(0, 200, 24), limits = c(0, 200))

a

b <- all_text_df %>% 
  group_by(thread_id) %>% 
  count() %>% 
  ggplot(., aes(x = "", y = n)) +
  geom_boxplot(width = 0.3) + 
  scale_y_continuous(breaks = seq(0, 200, 20))+
  # coord_flip() + 
  theme_ipsum() + 
  theme(plot.margin = margin(0.2, 0.5, 0.5 , 0.5, "cm")) +
  labs(title = "", 
       x = "", y = "Number of comments", ,
       subtitle = "Concise: 75% of threads get less than 20 comments") 
b

#Combine plots
fig1 <- a + b 
fig1

#Save - SPecify your path
ggsave(file="Figure1.jpeg", plot=fig1, bg = "white", 
       width=30, height=20,unit = "cm", dpi = 300)


# Figure 2: temporal distribution of threads ----

# Dataframe with stats per day
day_stats <-  all_text_df %>%
  group_by(Day) %>%
  summarise(n_posts = n()) %>% 
  mutate(Day = as.Date(Day, format = "%Y-%m-%d"))

# Create dataframe for selected dates

quaterly <- as.Date(c("2021-10-01", 
                      # "2021-12-31",  
                      # "2022-04-01", 
                      "2022-06-30",
                      "2022-10-01", "2023-01-02",
                      # "2023-03-31", 
                      "2023-07-01", 
                      "2023-09-30")) %>% 
  as_tibble() %>% 
  rename(Day = value) %>% 
  mutate(Type = "Quaterly results") 

# Select particular events
events <- as.Date(c("2021-10-15", "2022-04-22", "2023-03-23")) %>% 
  as_tibble() %>% 
  rename(Day = value) %>% 
  mutate(Type = "Interventions from CEO")

vlinedata <-  bind_rows(quaterly, events) 
vlinedata <-  left_join(vlinedata, day_stats)

#Plot
fig2 <- ggplot(day_stats, aes(x = Day, y = n_posts, group = 1)) + 
  geom_line(stat = "identity") + 
  labs(subtitle = "Temporal distribution of comments in the r/Fundrise corpus",
       title = "Platformed conversations",
       x = "",
       y = "Total number of comments per day") + 
  scale_x_date(
    date_breaks = "2 months",         # Sets breaks to quarterly intervals
    date_labels = "%b %Y"            # Formats labels as "Q1 2023", "Q2 2023", etc.
  ) +
  scale_y_continuous(breaks = seq(0, 130, 20)) +
  theme_ipsum() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)   ) +
  geom_point(data = vlinedata, aes(x = Day, y = n_posts, col = Type), 
             size = 5, shape = 21, stroke = 2) +
  guides(fill = F) +
  scale_fill_ipsum() +
  theme(plot.margin = margin(0.2, 0.5, 0.5 , 0.5, "cm"))  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))
fig2
# Save - SPecify your path
ggsave(file="Figure2.jpeg", plot=fig2, bg = "white", 
       width=40, height=25,unit = "cm", dpi = 300)


#Optional: Export table for exploration
# 
#     df_threads$Day <- df_threads$date
#     table <- day_stats %>%
#       mutate(Day = as.character(Day)) %>% 
#       filter(n_posts > 50) %>% 
#       left_join(., df_threads, by = "Day")
#     
#     Specify your path
#     write.csv(table, "/stats_day.csv")
# 

# Figure 3: included in script "model15k.R" ----