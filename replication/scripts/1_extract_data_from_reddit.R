#Load and install packages
library(RedditExtractoR) 
library(tidyverse) 
library(tictoc)
library(stringi)

#---------------------------#
# Get URLS from r/Fundrise ----
#---------------------------#
# By date

new_fund_urls <- find_thread_urls(subreddit="FundRise", 
                                  sort_by="new", 
                                  period = "year")
sum(new_fund_urls$comments)


#----------------------------------------------------#
# Extract threads and comments from the list of urls
#----------------------------------------------------#

url_list <- new_fund_urls|>
   pull(url)

# Initialize a counter
counter <- 0

# Define a function to process each URL 
process_url_both <- function(url) {
  counter <<- counter + 1
  cat(paste0(counter, "/", length(url_list), "\n"))
  
        tryCatch({
        
        output <- get_thread_content(url)
        df_i <- output$threads
        comments_i <- output$comments
        Sys.sleep(5)
        
        return(list(threads = df_i, comments = comments_i))
      }, 
      
      error = function(e) {
        cat("Error processing URL:", url, "\n")
        cat("Error message:", conditionMessage(e), "\n")
        return(NULL)  # Return NULL for failed attempts
      })
  }

# Use purrr::map to process the URLs and get a list of results
tic()
results_list <- map(url_list, process_url_both)
toc(log = TRUE)

#Optional: save extract
# saveRDS(results_list, "name.RDS")

#Extract comments from the list
comments_list <- map(results_list, pluck, "comments")
y <- Filter(Negate(is.null), comments_list) 
df_comments <- map_dfr(y, ~.x %>% mutate(across(everything(), as.character)))


#Extract threads from the list
thread_list <- map(results_list, pluck, "threads")
y <- Filter(Negate(is.null), thread_list) 
df_threads <- map_dfr(y, ~.x %>% mutate(across(everything(), as.character)))

#Encoding object

stri_enc_detect(df_threads$title)

#----------------------------------------------------#
#Export data as csv ----
#----------------------------------------------------#

write.csv(df_threads, "allthreads_10_20_2023_Sortby_new_Period_year.csv", row.names = F)
write.csv(df_comments, "allcomments_10_20_2023_Sortby_new_Period_year.csv", row.names = F)


