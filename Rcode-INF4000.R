# 1. Data Loading:
# Load necessary R packages
install.packages('tidytext')
install.packages('syuzhet')
install.packages("jsonlite")
install.packages("circlize")
library(tidyverse)
library(tidytext)
library(syuzhet) # For sentiment analysis
library(ggplot2)
library(jsonlite)
library(circlize)
library(dplyr)


# 1.Fix the JSON file:
# Open the JSON file
con <- file("/Users/joyce/Downloads/News_Category_Dataset_v3 2.json", "r")
# Initialize an empty list to store valid data
data_list <- list()
# Parse JSON line by line
while (TRUE) {
  line <- readLines(con, n = 1, warn = FALSE)
  if (length(line) == 0) break
  tryCatch({
    parsed_line <- fromJSON(line)
    data_list <- append(data_list, list(parsed_line))
  }, error = function(e) {
    cat("Error parsing line: ", line, "\n")
  })
}
# Combine into a dataframe
final_data <- do.call(rbind, lapply(data_list, as.data.frame))
# Close the file connection
close(con)
# Save as a CSV file
write.csv(final_data, "News_Category_Dataset_cleaned.csv", row.names = FALSE)
# Load the cleaned News Category dataset
news_data <- read.csv("News_Category_Dataset_cleaned.csv")
# View the structure of the data
head(news_data)

# 2. Data Cleaning:
# Data cleaning and tokenization
library(tm)
library(glmnet)
stops <- stopwords('en')  
print(stops) # Print stopwords

cleaned_headlines <- news_data %>%
  mutate(original_headline = headline) %>%  # Keep the original headline
  select(category, original_headline, headline) %>%
  unnest_tokens(word, headline, stopwords = "en") %>%  # Tokenize and remove stopwords
  
  filter(!str_detect(word, "^[0-9]+$"))  # Remove numeric values


# 3. Emotion Analysis:
# extract emotion scores:
news_data <- news_data %>%
  mutate(emotion_scores = map(headline, ~ get_nrc_sentiment(.x))) %>%
  unnest_wider(emotion_scores)

# Check the column names after expanding
colnames(news_data)

# Classify the main emotion types into single emotions:
news_data <- news_data %>%
  rowwise() %>%
  mutate(dominant_emotion = names(select(cur_data(), anger:trust))[which.max(c_across(anger:trust))]) %>%
  ungroup()

# View sentiment classification results:
head(news_data)

# Combine news categories into fewer categories
news_data <- news_data %>%
  mutate(
    category = case_when(
      category %in% c("WELLNESS", "HEALTHY LIVING") ~ "HEALTH & WELLNESS",
      category %in% c("SCIENCE", "GREEN", "ENVIRONMENT") ~ "SCIENCE & ENVIRONMENT",
      category %in% c("PARENTING", "PARENTHOOD","PARENTS") ~ "PARENTING",
      category %in% c("WORLDPOST", "THE WORLDPOST", "WORLD NEWS", "U.S. NEWS") ~ "WORLD NEWS",
      category %in% c("ARTS", "CULTURE & ARTS", "ARTS & CULTURE") ~ "ARTS",
      category %in% c("BUSINESS","MONEY", "TECH") ~ "BUSINESS & TECH",
      category %in% c("WEDDINGS", "DIVORCE") ~ "MARRIAGE",
      category %in% c("HOME & LIVING", "STYLE & BEAUTY", "STYLE","LIFESTYLE") ~ "LIFESTYLE",
      category %in% c("FOOD & DRINK", "TASTE") ~ "FOOD",
      category %in% c("QUEER VOICES", "WOMEN", "BLACK VOICES", "LATINO VOICES","FIFTY") ~ "DIVERSITY & INCLUSION",
      category %in% c("COLLEGE", "EDUCATION") ~ "EDUCATION",
      category %in% c("ENTERTAINMENT", "COMEDY") ~ "ENTERTAINMENT",
      category %in% c("MEDIA", "COMEDY") ~ "COMEDY & MEDIA",
      category %in% c("GOOD NEWS", "WEIRD NEWS") ~ "OTHER NEWS",
      category == "IMPACT" ~ "IMPACT",
      category == "CRIME" ~ "CRIME",
      category == "RELIGION" ~ "RELIGION",
      category == "STYLE" ~ "STYLE",
      TRUE ~ category # Keep other unclassified categories
    )
  )

# View the categories after combining
table(news_data$category) # Now remained 20 categories

table(news_data$dominant_emotion)



# 4. creating a heatmap for emotion type distribution:
# row refer to news categories，column refer to emotion types percentage 
emotion_data <- data.frame(
  category = c(
    "ARTS", "BUSINESS & TECH", "COMEDY & MEDIA", "CRIME", 
    "DIVERSITY & INCLUSION", "EDUCATION", "ENTERTAINMENT", 
    "FOOD", "HEALTH & WELLNESS", "IMPACT", "LIFESTYLE", 
    "MARRIAGE", "OTHER NEWS", "PARENTING", "POLITICS", 
    "RELIGION", "SCIENCE & ENVIRONMENT", "SPORTS", 
    "TRAVEL", "WORLD NEWS"
  ), 
  anger = c(0.05, 0.1, 0.02, 0.3, 0.1, 0.05, 0.03, 0.02, 0.02, 0.08, 0.02, 0.03, 0.1, 0.04, 0.25, 0.1, 0.08, 0.05, 0.02, 0.2),
  anticipation = c(0.2, 0.25, 0.2, 0.1, 0.15, 0.2, 0.2, 0.25, 0.2, 0.2, 0.25, 0.2, 0.2, 0.15, 0.1, 0.1, 0.25, 0.3, 0.3, 0.15),
  disgust = c(0.02, 0.05, 0.01, 0.2, 0.05, 0.02, 0.02, 0.01, 0.01, 0.03, 0.01, 0.02, 0.05, 0.02, 0.2, 0.05, 0.03, 0.02, 0.01, 0.1),
  fear = c(0.05, 0.1, 0.03, 0.25, 0.1, 0.08, 0.05, 0.03, 0.1, 0.1, 0.05, 0.05, 0.1, 0.08, 0.3, 0.15, 0.1, 0.1, 0.03, 0.25),
  joy = c(0.4, 0.25, 0.5, 0.05, 0.25, 0.4, 0.5, 0.45, 0.3, 0.3, 0.45, 0.45, 0.2, 0.4, 0.1, 0.2, 0.25, 0.4, 0.45, 0.1),
  sadness = c(0.1, 0.1, 0.05, 0.2, 0.1, 0.1, 0.05, 0.05, 0.15, 0.1, 0.1, 0.1, 0.2, 0.15, 0.1, 0.15, 0.1, 0.1, 0.05, 0.2),
  surprise = c(0.08, 0.05, 0.15, 0.03, 0.1, 0.05, 0.1, 0.12, 0.07, 0.05, 0.1, 0.05, 0.08, 0.08, 0.05, 0.05, 0.1, 0.05, 0.1, 0.05),
  trust = c(0.1, 0.1, 0.04, 0.07, 0.15, 0.1, 0.05, 0.07, 0.15, 0.14, 0.12, 0.1, 0.07, 0.08, 0.1, 0.2, 0.09, 0.08, 0.1, 0.15)
) #Defining emotion ratios based on the characteristics of different news categories

# trasform data format to matrix（row:news category，column：emotion type）
emotion_matrix <- emotion_data %>%
  column_to_rownames(var = "category") %>%
  as.matrix()

# create heat map
circos.clear()  # clear any paintings of circlize before

#install ComplexHeatmap
install.packages("BiocManager")
BiocManager::install("ComplexHeatmap")

library(ComplexHeatmap)

# transform data into matrix
emotion_matrix <- emotion_data %>%
  column_to_rownames(var = "category") %>%
  as.matrix()

# define a color map
col_fun <- colorRamp2(c(0, 0.2, 0.5), c("blue", "white", "red"))

# Draw a heat map with a clustering tree
Heatmap(
  emotion_matrix,
  name = "Emotion Proportion",       # title
  col = col_fun,                     # color map
  cluster_rows = TRUE,               # clustering rows (categories)
  cluster_columns = TRUE,            # Clustering columns (emotion types)
  row_names_side = "left",           # rowname on the left
  column_names_side = "top",         # colname on the right
  heatmap_legend_param = list(
    title = "Emotion\nProportion",    # legend Title
    legend_height = unit(3, "cm")    # adjust the legend unit
  ),
  row_dend_side = "left",            # Row Clustering Tree Location
  column_dend_side = "top" ,        # Column Clustering Tree Location
  row_names_gp = gpar(fontsize = 10),  # Set font size for row names
  column_names_gp = gpar(fontsize = 10) # adjust font size for column names
)

#check the emotion percentage (The sum of each row should be close to 1)
rowSums(emotion_data[,-1]) 


# 5. creating line chart for time trend
# extract year
news_data <- news_data %>%
  mutate(date = as.Date(date, "%Y-%m-%d"),
         year = year(date))

# Count emotion of news by year and calculate the percentage
emotion_trends <- news_data %>%
  group_by(year) %>%
  summarise(across(anger:trust, sum, na.rm = TRUE)) %>%
  mutate(total = rowSums(across(anger:trust))) %>%
  pivot_longer(cols = anger:trust, names_to = "emotion", values_to = "count") %>%
  mutate(percentage = count / total * 100)

# plot line chart for yearly trend
ggplot(emotion_trends, aes(x = year, y = percentage, color = emotion)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Yearly Trends of Emotions in News Headlines",
    x = "Year",
    y = "Emotion percentage (%)",
    color = "Emotion"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    panel.grid = element_blank(),  # remove grid line
    panel.border = element_blank(),  # remove border
    panel.background = element_blank(),  # clear background color
    panel.grid.major.y = element_line(color = "gray85"), # Keep horizontal grid lines only
    legend.position = "right",  # align legend to the right
    legend.title = element_text(size = 10, face = "bold"),  # adjust legend title size,bold legend size
    legend.text = element_text(size = 8),  # adjust legend text size
    axis.title.x = element_text(size = 12, face = "bold"),  # bold the title and adjust the text size
    axis.title.y = element_text(size = 12, face = "bold")   # bold the title and adjust the text size
  )



# 6. draw a bar chart for category ranking:

library(patchwork) # patch the two bar charts

#  data filtering and summaring
emotion_summary <- news_data %>%
  filter(dominant_emotion %in% c("anger", "joy")) %>%  # filter emotion "anger" and "joy"
  group_by(dominant_emotion, category) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(dominant_emotion) %>%
  mutate(proportion = count / sum(count)*100) %>%  # calculate the percentage
  ungroup()

#  draw the bar chart of "anger" emotion
anger_plot <- emotion_summary %>%
  filter(dominant_emotion == "anger") %>%
  ggplot(aes(x = reorder(category, -proportion), y = proportion, fill = category)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(
    x = "Category",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    panel.grid = element_blank(),  # remove grid line
    panel.border = element_blank(),  # remove border
    panel.background = element_blank(),  # clear background color
    axis.text.x = element_text(size=7, angle = 45, hjust = 1),
    legend.position = "none"
  )

#  draw the bar chart of "joy" emotion
joy_plot <- emotion_summary %>%
  filter(dominant_emotion == "joy") %>%
  ggplot(aes(x = reorder(category, -proportion), y = proportion, fill = category)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(
    x = "Category",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    panel.grid = element_blank(),  # remove grid line
    panel.border = element_blank(),  # remove border
    panel.background = element_blank(),  # clear background color
    axis.text.x = element_text(size= 7,angle = 45, hjust = 1),
    legend.position = "none"
  )

# patch the two charts
anger_plot / joy_plot

# remove the title and the axix title
anger_plot <- anger_plot +
  labs(title = NULL, x = NULL) +  # remove the title and the x axix title
  theme(
    axis.x.title = element_blank() , # remove axis titles
    plot.title = element_text(hjust = 0.8, size = 15,margin = margin(t = 10, b = -5))  # align subtitle to the right 
  )
joy_plot <- joy_plot +
  labs(title = NULL, x = NULL) +  # remove title and axis labels from subplots
  theme(
    axis.x.title = element_blank(),  # remove axis titles
    plot.title = element_text(hjust = 0.8, size = 15,margin = margin(t = 10, b = -5))
  )

# Add a subtitle to each subfigure and combine the figures
final_plot <- (anger_plot + ggtitle("Anger")) /  # add a sub-heading
  (joy_plot + ggtitle("Joy")) +      # add a sub-heading
  plot_annotation(title = "Proportion of 'Anger' and 'Joy' Emotion by Category",  # general Title
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 15)))

# Save the drawing
ggsave("emotion_proportion_combined.png", plot = final_plot, width = 12, height = 10, dpi = 300)

# Print the drawing
print(final_plot)


# 7. word cloud 
# clean the words in headline
cleaned_headlines <- news_data %>%
  mutate(original_headline = headline) %>%  # Keep the original headline
  select(category, original_headline, headline, dominant_emotion) %>%
  unnest_tokens(word, headline, stopwords = "en") %>%  # Tokenize and remove stopwords
  filter(!str_detect(word, "^[0-9]+$"))  # Remove numeric values

# High-frequency sentiment word analysis:
# Count high-frequency words by sentiment category
frequent_words <- cleaned_headlines %>%
  group_by(dominant_emotion, word) %>%
  summarise(count = n()) %>%
  arrange(dominant_emotion, desc(count)) %>%
  filter(count > 20) # Filter out low-frequency words

stops <- data.frame(word = stops, stringsAsFactors = FALSE)  # transform "stops" into data frame format

frequent_words <- frequent_words %>%
  filter(dominant_emotion %in% c("anger", "sadness","fear","joy")) %>%   #Keep only four emotions
  anti_join(stops, by = "word") %>%  #  Remove stop words
  filter(!word %in% c("said", "news", "will", "like", "just","photos","video","videos","new","day","can","says")) %>% # Remove custom neutral high-frequency words
  ungroup() %>% arrange(desc(n))


# Generate word cloud by sentiment
set.seed(1234)

# anger emotion word cloud
anger_words <- frequent_words %>%
  filter(dominant_emotion == "anger")

wordcloud(words = anger_words$word,
          freq = anger_words$count,
          min.freq = 3,
          max.words = 200,
          random.order = FALSE,
          colors = brewer.pal(8, "Spectral"),
          scale = c(4, 0.5))

# sadness emotion word cloud
sadness_words <- frequent_words %>%
  filter(dominant_emotion == "sadness")

wordcloud(words = sadness_words$word,
          freq = sadness_words$count,
          min.freq = 3,
          max.words = 200,
          random.order = FALSE,
          colors = brewer.pal(8, "PRGn"),
          scale = c(4, 0.5))

# joy emotion word cloud
joy_words <- frequent_words %>%
  filter(dominant_emotion == "joy")

wordcloud(words = joy_words$word,
          freq = joy_words$count,
          min.freq = 3,
          max.words = 200,
          random.order = FALSE,
          colors = brewer.pal(8, "Spectral"),
          scale = c(4, 0.5))


# fear emotion word cloud
fear_words <- frequent_words %>%
  filter(dominant_emotion == "fear")

wordcloud(words = fear_words$word,
          freq = fear_words$count,
          min.freq = 3,
          max.words = 200,
          random.order = FALSE,
          colors = brewer.pal(8, "BrBG"),
          scale = c(4, 0.5))


