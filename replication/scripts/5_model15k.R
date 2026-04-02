# This script needs to run "data_prep.R" before

#Load packages
library(here)
library(openxlsx)

#Specify your path
source(here("/scripts/data_prep.R"))

#------------------#
#   RUN MODEL ----
#------------------#

# Convert to the STM format package
dfm_stm <- convert(dfm_trim, 
                   to = "stm")


#Run with stm
stm_nometa15k <- stm(documents = dfm_stm$documents,
                          vocab = dfm_stm$vocab,
                          verbose = T,
                          K = 15,
                          seed = 101215,
                          max.em.its = 500,
                          data = dfm_stm$meta,
                          init.type = "Spectral")

# Optional: save model
# saveRDS(stm_nometa15k, file = "15kmodel.RDS")

#----------------------#
#   EXPLORE MODEL ----
#----------------------#

## Probability words ----
prob15 <- labelTopics(stm_nometa15k, n=20) 
prob15 <- data.frame(t(prob15$prob))
colnames(prob15) <- paste0("topic", 1:ncol(prob15))
prob15 %>% 
  gt() %>% 
  tab_header("Model 15k = top 20 high prob words by topic") %>% 
  tab_options(table.width = "60%",  table.font.size = 10 ) %>% 
  opt_stylize(style = 5) 


## FREX words ----

frex15 <- labelTopics(stm_nometa15k, n=20) 
frex15 <- data.frame(t(frex15$frex))
colnames(frex15) <- paste0("topic", 1:ncol(frex15))
frex15 %>% 
  gt() %>% 
  tab_header("Model 15k: 20 FREX words") %>% 
  tab_options(table.width = "60%",  table.font.size = 10 ) %>% 
  opt_stylize(style = 5) 



## Fig 3: Plot gamma of topics with frex words ----
td_gamma <- tidy(stm_nometa15k,
                 matrix = "gamma",
                 document_names = rownames(dfm))


top_terms <- tidy(stm_nometa15k, matrix = "beta") %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

top_terms_frex <- tidy(stm_nometa15k, matrix = "frex") %>% 
  group_by(topic) %>%
  slice_head(n = 10) %>%
  mutate(rank = row_number()) %>% 
  ungroup() %>% 
  group_by(topic) %>%
  arrange(rank) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

#Join prob and frex words
all_words <-  left_join(top_terms_frex, top_terms ,by = "topic") %>% 
  mutate(terms = paste0(terms.x, "\n" , terms.y)) %>% 
  select(topic, terms)

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  # left_join(all_words, by = "topic") %>%
  left_join(top_terms_frex, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

fig3 <- gamma_terms %>%
  # top_n(15, gamma) %>%
  ggplot(aes(topic, gamma, label = terms)) +
  geom_col(show.legend = FALSE, width = 0.8) +
  geom_text(hjust = 0, nudge_y = 0.005, nudge_x =0.2, size = 2.5) + # change nudge for both frex and beta
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.4),
                     labels = scales::percent_format()) +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 13)) +
  labs(x = NULL, y = "Topic proportion (gamma)",
       title = "Topics by prevalence in the r/Fundrise corpus",
       subtitle = "With high FREX (top) and high probability (bottom) words for each topic") + 
  labs(x = NULL, y = "Topic proportion (gamma)",
       title = "Topics by prevalence in the r/Fundrise corpus",
       subtitle = "With top FREX (FRequent and EXclusive) words for each topic") + 
  theme_ipsum() + 
  theme(plot.margin = margin(0.2, 0.5, 0.5 , 0.5, "cm")) 
fig3  

#Specify your path
ggsave(file="Figure3.jpeg", plot=fig3, bg = "white", 
       width=25, height=25,unit = "cm", dpi = 300)

#-------------------------------#
#   EXPORT TO EXCEL ----
#-------------------------------#

# Exporting the threads to excel

to_export_df <-  all_thread_df 

#Create new columns with the names of the two most prevalent topics 

to_export_df$Rank15_FirstTopic <- NA 
to_export_df$Rank15_SecondTopic <- NA 
to_export_df$Rank15_FirstTopic_value <- NA 
to_export_df$Rank15_SecondTopic_value <- NA 

#Extract the two most prevalent topics and append to dataframe of threads

for (i in 1:nrow(to_export_df)){
  
  ## Extract the first row as a vector
  row_values <- as.numeric(theta_nometa15[i, -1])
  
  # Get the column names
  column_names <- colnames(theta_nometa15)[2:   ncol(theta_nometa15)]
  
  # Order the row values and get the indices
  order_indices <- order(row_values, decreasing = TRUE)
  
  # first topic
  first_highest_value <- row_values[order_indices[1]]
  first_highest_column <- column_names[order_indices[1]]
  
  
  # second topic
  second_highest_value <- row_values[order_indices[2]]
  second_highest_column <- column_names[order_indices[2]]
  
  
  to_export_df$Rank15_FirstTopic[i] <- first_highest_column
  to_export_df$Rank15_FirstTopic_value[i] <- first_highest_value
  to_export_df$Rank15_SecondTopic[i] <- second_highest_column
  to_export_df$Rank15_SecondTopic_value[i] <- second_highest_value
}

# Create dataframe
final_export_df <- to_export_df %>% 
  select(thread_id, url, title_clean, date, docnum, 
         Rank15_FirstTopic, Rank15_SecondTopic,
         Rank15_FirstTopic_value, Rank15_SecondTopic_value
  ) %>% 
  mutate(Topic_value_sum =Rank15_FirstTopic_value + Rank15_SecondTopic_value) %>% 
  left_join(. , all_thread_info %>% select(docnum, author, n_authors, n_comments))



# Create a new workbook
wb <- createWorkbook()

# Get the unique topics
topics <- unique(final_export_df$Rank15_FirstTopic)

# Loop through each topic and export to a spreadsheet
for (topic in topics) {
  # Filter the dataframe for the current topic
  topic_df <- to_check_df_export[final_export_df$Rank15_FirstTopic == topic, ]
  
  # Add a new sheet to the workbook
  addWorksheet(wb, sheetName = topic)
  
  # Write the filtered dataframe to the new sheet
  writeData(wb, sheet = topic, x = topic_df)
}

# Save the workbook to a file
saveWorkbook(wb, file = "data/processed/threads_with_2_topics.xlsx", overwrite = TRUE)
