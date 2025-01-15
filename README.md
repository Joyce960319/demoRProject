# demoRProject
This is a demo submission for inf6027

# 1. Data Loading:
# Load necessary R packages
install.packages('tidytext')
install.packages('syuzhet')
install.packages("jsonlite")
library(tidyverse)
library(tidytext)
library(syuzhet) # For sentiment analysis
library(ggplot2)
library(jsonlite)

# Load the News Category dataset
news_data <- fromJSON("/Users/joyce/Downloads/News_Category_Dataset_v3 2.json")
# View the structure of the data
head(news_data)
# View JSON data structure
str(json_data)
library(jsonlite)

# Fix the JSON file:
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

# 3. Sentiment Analysis:
# Extract sentiment scores
news_data$sentiment <- get_sentiment(news_data$headline, method = "bing")
# Classify based on sentiment scores
news_data <- news_data %>%
  mutate(sentiment_label = case_when(
    sentiment > 0 ~ "Positive",
    sentiment < 0 ~ "Negative",
    TRUE ~ "Neutral"
  ))
# View sentiment classification results
head(news_data)

# 4. Analyze Sentiment Distribution by Category:
sentiment_distribution <- news_data %>%
  group_by(category, sentiment_label) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)
# View sentiment distribution
head(sentiment_distribution)

# 5. Visualization:
# Plot stacked bar chart of sentiment distribution by news category

ggplot(sentiment_distribution, aes(y = category, x = percentage, fill = sentiment_label)) +
  geom_bar(stat = "identity", position = "fill", color = "white") + # Add white border
  labs(
    title = "Sentiment Distribution by News Category (Percentage)",
    x = "Percentage",
    y = "News Category"
  ) +
  scale_x_continuous(labels = scales::percent_format()) + # Show x-axis in percentage
  scale_fill_manual(
    values = c("Negative" = "#F1948A", 
               "Neutral" = "#F7DC6F", 
               "Positive" = "#82E0AA") # Custom color palette
  ) +
  scale_y_discrete(expand = expansion(add = c(0.5, 0.5))) + # Adjust spacing for News Category
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 7, margin = margin(r = 5)), # Adjust y-axis text size and margin
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)), # Style x-axis title
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)), # Style y-axis title
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 15)), # Center the title and add spacing
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines
    panel.grid.major.x = element_line(color = "gray85"), # Adjust vertical grid line style
    panel.grid.minor = element_blank(), # Remove minor grid lines
    legend.position = "top", # Place legend at the top
    legend.direction = "horizontal", # Arrange legend horizontally
    legend.title = element_blank(), # Remove legend title
    legend.text = element_text(size = 10) # Adjust legend text size
  )

# Plot sentiment proportion pie chart for all titles
overall_sentiment <- news_data %>%
  count(sentiment_label) %>%
  mutate(percentage = n / sum(n) * 100)


ggplot(overall_sentiment, aes(x = "", y = percentage, fill = sentiment_label)) +
  geom_bar(width = 1, stat = "identity",color = "white") +
  scale_fill_manual(
    values = c("Negative" = "#FFA726", 
               "Neutral" = "#42A5F5", 
               "Positive" = "#EF5350") )+ # Custom color scheme
  coord_polar("y", start = 0) +
  labs(title = "Overall Sentiment Distribution in News Headlines",
       x = NULL,
       y = NULL) +
  theme_void()+
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 15)) # 标题居中并增加下方间距
  )

# High-frequency sentiment word analysis:
# Count high-frequency words by sentiment category
frequent_words <- cleaned_headlines %>%
  inner_join(news_data, by = c("original_headline" = "headline")) %>%
  group_by(sentiment_label, word) %>%
  summarise(count = n()) %>%
  arrange(sentiment_label, desc(count)) %>%
  filter(count > 20) # Filter out low-frequency words

# Prepare high-frequency word data, remove stop words and neutral high-frequency words
frequent_words <- frequent_words %>%
  filter(sentiment_label %in% c("Positive", "Negative")) %>%  # Keep only positive and negative sentiments
  anti_join(stops, by = "word") %>%  #  Remove stop words
  filter(!word %in% c("said", "news", "will", "like", "just","photos","video","videos")) %>% # Remove custom neutral high-frequency words
  count(sentiment_label, word, sort = TRUE) %>%              # Count word frequency
  ungroup()

# Plot sentiment word clouds
install.packages('wordcloud')
install.packages('RColorBrewer')
library(wordcloud)

# Generate word cloud by sentiment
set.seed(1234)

# Positive sentiment word cloud
positive_words <- frequent_words %>%
  filter(sentiment_label == "Positive")

wordcloud(words = positive_words$word,
          freq = positive_words$count,
          min.freq = 5,
          max.words = 100,
          random.order = FALSE,
          colors = brewer.pal(8, "Blues"),
          scale = c(4, 0.5))

# Negative sentiment word cloud
negative_words <- frequent_words %>%
  filter(sentiment_label == "Negative")

wordcloud(words = negative_words$word,
          freq = negative_words$count,
          min.freq = 5,
          max.words = 100,
          random.order = FALSE,
          colors = brewer.pal(8, "Reds"),
          scale = c(4, 0.5))

# Analyze sentiment trends over time in news headlines:
# Ensure correct date format and extract year
news_data <- news_data %>%
  mutate(year = year(as.Date(date)))

# Count negative sentiment news by year
sentiment_by_year <- news_data %>%
  group_by(year,sentiment_label) %>%
  summarise(count = n()) %>%
  left_join(
    news_data %>%
      group_by(year) %>%
      summarise(total_count = n()), 
    by = "year"
  ) %>%
  mutate(percentage = (count / total_count) * 100)
# View result
sentiment_by_year


library(ggplot2)
# Plot line chart
ggplot(sentiment_by_year, aes(x = year, y = percentage, color = sentiment_label, group = sentiment_label)) +
  geom_line(size = 1.2) + # Adjust line thickness
  geom_point(size = 3) + # Adjust point size
  scale_color_manual(
    values = c("Positive" = "#0096D6", "Negative" = "#FF6B6B", "Neutral" = "#FFD700") # Custom colors

  ) +
  labs(
    title = "Sentiment Distribution Over Time",
    x = "Year",
    y = "Percentage (%)",
    color = "Sentiment"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top", # Set legend position
    legend.justification = "left", # Align legend to the left
    legend.direction = "horizontal", # Display legend horizontally
    legend.title = element_text(face = "bold"), # Bold legend title
    legend.text = element_text(size = 10), # Set legend text size
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 10), # Adjust X-axis text
    axis.text.y = element_text(size = 10), # Adjust Y-axis text
    panel.grid.major.y = element_line(color = "gray85"), # Keep horizontal grid lines only
    panel.grid.major.x = element_blank(), # Remove vertical grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    plot.background = element_blank(), # Clear background color
    panel.background = element_blank() # Clear panel background color
  )


# 8. Ranking of negative sentiment proportion by news category:
library(dplyr)
library(ggplot2)

# Count negative sentiment news and total news by category
negative_sentiment_by_category <- news_data %>%
  group_by(category) %>%
  summarise(
    negative_count = sum(sentiment_label == "Negative"),
    total_count = n()
  ) %>%
  mutate(negative_percentage = (negative_count / total_count) * 100) %>%
  arrange(desc(negative_percentage)) # Sort by descending negative sentiment percentage

# View ranking of negative sentiment proportion
print(negative_sentiment_by_category)
# Plot bar chart of negative sentiment proportion ranking
library(ggplot2)
ggplot(negative_sentiment_by_category, aes(x = reorder(category, -negative_percentage), y = negative_percentage, fill = negative_percentage)) +
  geom_bar(stat = "identity") +
  scale_fill_gradientn(
    colours = c("20" = "steelblue", "30" = "skyblue", "40" = "coral", "50" = "tomato", "60" = "firebrick"),  
    values = scales::rescale(c(min(negative_sentiment_by_category$negative_percentage), 
                               mean(negative_sentiment_by_category$negative_percentage), 
                               max(negative_sentiment_by_category$negative_percentage))) # Set gradient positions
  ) +
  labs(
    title = "Negative Sentiment Percentage by News Category",
    x = "News Category",
    y = "Negative Sentiment Percentage (%)"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_blank(),  #Remove background
    panel.grid = element_blank(),       # Remove grid lines
    axis.text.x = element_text(angle = 90,hjust = 1, vjust = 0.5, size = 8), # Adjust X-axis text
    axis.text.y = element_text(size = 10),  # 
    legend.title = element_text(face = "bold"), # Bold legend title
    legend.position = "top" # Place legend at the top
  )












