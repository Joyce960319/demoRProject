About Instructions for Running the R Code：
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
