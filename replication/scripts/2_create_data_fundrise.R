# Load or install packages

library(tidyverse)
library(lubridate)
library(readr)
library(stringi)
library(here)
library(textclean)

# Specify your path here
#Open data
df_threads <- read.csv(here("/data/allthreads_10_20_2023_Sortby_new_Period_year.csv"), encoding = "latin1")
df_comments <- read.csv(here("/data/back_up/allcomments_10_20_2023_Sortby_new_Period_year.csv"), encoding = "latin1")


#Combine threads and comments into a single object
text_threads <- df_threads %>% 
  rename(n_comment = comments) %>% 
  select(url, author, date, timestamp, text, title, n_comment, score) %>% 
  mutate(text_id = "0") %>% 
  mutate(cat = "thread")  %>% 
  mutate(thread_id =str_extract(url, "comments/([A-Za-z0-9]+)")) %>% 
  mutate(thread_id = sub("comments/", "", thread_id)) 


text_comments <- df_comments %>% 
  rename(text = comment) %>% 
  rename(text_id = comment_id) %>% 
  select(url, author, date, timestamp, text, text_id, score) %>% 
  mutate(cat = "comment") %>% 
  mutate(title = NA, 
         n_comment = NA) %>% 
  mutate(thread_id =str_extract(url, "comments/([A-Za-z0-9]+)")) %>% 
  mutate(thread_id = sub("comments/", "", thread_id))

#Prepare dataset
all_text_df <- bind_rows(text_threads, text_comments) %>% 
  mutate(datetime = as.numeric(timestamp)) %>% 
  arrange(url, datetime) %>% 
  group_by(url) %>% 
  mutate(order_com = rank(timestamp)) %>% 
  mutate(all_text = ifelse(cat == "thread", paste0(title, "\n", text), text)) %>% 
  relocate(title, thread_id, order_com, all_text, date) %>% 
  fill(title, .direction = "down") %>% 
  ungroup()

  #Cleaning text
all_text_df <- all_text_df %>% 
  mutate(clean_text = replace_non_ascii(all_text)) %>% 
  mutate(clean_text = replace_html(clean_text)) %>% 
  mutate(clean_text = replace_white(clean_text)) %>%
  mutate(clean_text = str_replace_all(clean_text, "\xa0", " ")) %>% 
  mutate(clean_text = str_replace_all(clean_text, "-&gt;", "->")) %>% #the arrow
  mutate(clean_text = str_replace_all(clean_text, "&.;", "...")) %>% #the arrow
  mutate(clean_text = str_replace_all(clean_text, "&#x200B;", "")) %>% #line break
  relocate(clean_text, .after = order_com) %>% 
  relocate(all_text, .after = text) %>% 
  mutate(title_clean = replace_non_ascii(title)) %>% 
  mutate(year = year(date)) %>% 
  relocate(title_clean) %>% 
  relocate(title, .after = text) %>% 
  relocate(text_id, .after = order_com)
