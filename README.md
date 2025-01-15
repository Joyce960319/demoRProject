Hey,here is my research topic R code file for INF602-Introduction to data science, and I have some descriptions for you to help understand the code and the research aim:

My research topic is "Analysis of the distribution and temporal trend of news headlines sentiment, exploration based on news categories and time dimensions". 
Limited studies have integrated both category-based and time-based analyses of sentiment, leaving a gap in knowledge about how emotional tones differ across topics and change in response to shifts in society. My aim is to analyze the time and category variations in headline sentiment may reveal valuable patterns in emotional communication and show the influence of major events in society on news sentiment.

The research questions are as followed:
1.Sentiment distribution: 
Are there significant differences in the sentiment distribution (positive, negative, and neutral) of headlines across news categories? Which news categories show the most significant sentiment bias?
2.Time trend:
How does the distribution of sentiment (positive, negative, and neutral) of news headlines change over time? Do peaks in affective change correlate with specific social events or trends?
3.High frequency word analysis:
What are the significant differences in the high frequency words that appear in positive and negative news headlines? Are specific words highly associated with a certain emotion category?
4.Differences between categories:
Is there a significant difference in the proportion of negative sentiment headlines across news categories? Which categories are more likely to have negative sentiment headlines? What underlying social phenomenon might this association reflect?

My key findings are as followed:
1. Distribution of Sentiment by Category：
"Politics" and "International News" bear the highest negative sentiment, since many controversial topics and crises were tracked globally. "Entertainment" and "Lifestyle" bear very high positive sentiments, as these topics are light in nature. Sentiment distribution in news of different categories reflects their possible influence on readers' psychology.
2. Temporal Trend of Sentiment:
The share of negative sentiment keeps on increasing with every year and peaks in the global crisis year. Neutral sentiment is stable, while the positive sentiment displays a gradually decaying trend based on economic and social challenges. 
3. Vocabulary insights:
While positive sentiment is often represented with words like "love" and "family," negative sentiment is often captured with words such as "crisis" and "death". Lexical distribution offers support toward the development of Sentiment Analysis tools.

Finally, about Instructions for Running the R Code：
1.Install Required Packages:
Ensure all necessary R packages are installed. Use the following command to install missing packages:

install.packages(c("tidytext", "syuzhet", "jsonlite","ggplot2","tidyverse"))

2.Load Packages:
Before running the code, load all required libraries:

library(tidyverse)
library(tidytext)
library(syuzhet)
library(ggplot2)
library(jsonlite)

3.Set Working Directory:
Ensure your R session is set to the directory containing your input files. Use:

setwd("path/to/your/directory")

4.Prepare Data:
Confirm that all input files (e.g., CSVs, datasets) are available in the specified directory. Verify the file names match those referenced in the code.

5.Run the Code:
Execute the code in RStudio or your preferred R environment by copying and pasting it into the console or running it as a script.

6.Review Output:
	•	Check for errors or warnings in the console.
	•	Review the generated outputs (e.g., plots, tables, or summary statistics).
 
7.Save Results (Optional):
If the code generates outputs to files (e.g., CSVs or images), verify the output directory and ensure files are saved correctly.

8.Dependencies:
If specific software versions are required, ensure they are installed (e.g., R version ≥4.0.0).

By following these steps, you should be able to run the R code smoothly and replicate the intended results.



 
