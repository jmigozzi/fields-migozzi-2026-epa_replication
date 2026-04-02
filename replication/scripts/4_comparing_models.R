# This script needs to run "data_prep before"
# Load or install packages
library(here)
# Specify your path here

source(here("/scripts/data_prep.R"))



# Convert to the STM format package
dfm_stm <- convert(dfm_trim, 
                   to = "stm")


# ---------------------#
# Searching for K ----
# ---------------------#


## Using STM ----

### Long K interval ----
# This takes about 15 minutes to run
ideak_k_long <- searchK(
                  documents = dfm_stm$documents, 
                  vocab =  dfm_stm$vocab,
                  K = seq(5, 50, 
                          by = 5), 
                  N =  floor(0.1*927),
                  heldout.seed = 101215,
                  max.em.its = 500)

#Export figures
plot(ideak_k_long)
#Specify your path
png(filename="/appendices_Figure1.png", 
    width     = 20,
    height    = 20,
    units     = "cm",
    res       = 300)
plot(ideak_k_long)
dev.off()

### Short K interval ----
ideak_k_short <- searchK(documents = dfm_stm$documents, 
                         vocab =  dfm_stm$vocab,
                         K = seq(8, 30, 
                                 by = 1), 
                         N =  floor(0.1*927), 
                         heldout.seed = 101215,
                         max.em.its = 500)

#Export figures
plot(ideak_k_short)
#Specify your path
png(filename="appendices_Figure2.png", 
    width     = 20,
    height    = 20,
    units     = "cm",
    res       = 300)
plot(ideak_k_short)
dev.off()


## using a tidy method proposed by Julia Silge ----
# This takes about 15 minutes to run

ideal_k <- searchK(
  documents = dfm_stm$documents, 
  vocab =  dfm_stm$vocab,
  K = seq(6, 20, 
          by = 1), 
  N =  floor(0.1*927),
  heldout.seed = 101215,
  max.em.its = 500
)

plot(ideal_k)

# Save object
# saveRDS(object = ideal_k_long, file = "ideal_k_10_20_by_1.RDS")

tidy_result <- ideal_k$results %>% 
  pivot_longer(
    cols = -K, 
    names_to = "metric", 
    values_to = "value") %>% 
  filter(metric %in% 
           c("lbound", "exclus", "residual", "semcoh")) %>% 
  mutate(value = map_dbl(value, 1)) %>% 
  mutate(K = map_dbl(K, 1))


ggplot(tidy_result, aes(K, y = value, color = metric)) + 
  geom_point(size = 2) + 
  geom_line() + 
  guides(color = "none") + 
  theme_ipsum() + 
  facet_wrap(~ metric, scales = "free")


tidy_result %>%
  filter(metric %in% c("exclus", "semcoh")) %>% 
  pivot_wider(names_from = metric, values_from = value) %>% 
  select(K, exclus, semcoh) %>%
  # filter(K %in% c(20, 60, 100)) %>%
  # unnest() %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semcoh, exclus, label = K )) +
  geom_point(size = 5, alpha = 0.7) +
  geom_label_repel(col = "black") +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")


ideal_k_long$results %>% 
  as.data.frame() %>% 
  mutate(across(c(exclus, semcoh), as.numeric)) %>% 
  ggplot(aes(x = semcoh, y = exclus)) +
  geom_point() +
  geom_label_repel(aes(label = K)) +
  labs(x = "Semantic Coherence", 
       y = "Exclusivity") +
  theme_ipsum()

# We select a model with 15k over 14k for higher exclusivity