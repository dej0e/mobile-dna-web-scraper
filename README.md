# Web Scraping and Analysis of Journal Articles

## Project Overview
This project involves scraping article data from a journal, cleaning and preprocessing the data, performing basic analysis, and creating visualizations to derive insights. The results will be summarized in a PowerPoint presentation.

---

## Learning Objectives
By the end of this project, you will:
1. Understand the basics of web scraping.
2. Use R to scrape article data from a journal.
3. Clean and preprocess scraped data.
4. Perform basic data analysis on the collected article data.
5. Create visualizations and insights based on the data.

---

## Project Tasks

### Task 1: Set Up Your R Environment
1. Ensure you have **R** and **RStudio** installed.
2. Install the necessary R packages:
    ```R
    install.packages("rvest")
    install.packages("httr")
    install.packages("xml2")
    ```
3. Load the required libraries in your R script:
    ```R
    library(rvest)
    library(httr)
    library(xml2)
    ```

---

### Task 2: Scraping Article Data
1. Input the target year for scraping.
2. Extract the following fields for each article:
   - **Title**
   - **Authors**
   - **Correspondence Author**
   - **Correspondence Author's Email**
   - **Publish Date**
   - **Abstract**
   - **Keywords**
3. Example R code snippet for scraping:
    ```R
    url <- "https://example-journal.com/year/2023"
    webpage <- read_html(url)
    
    titles <- webpage %>% html_nodes(".article-title") %>% html_text()
    authors <- webpage %>% html_nodes(".authors") %>% html_text()
    # Add similar code for other fields...
    ```

---

### Task 3: Data Cleaning and Preprocessing
1. Handle missing or incomplete data by:
   - Removing rows with empty critical fields (e.g., Title, Publish Date).
   - Filling missing Keywords or Abstract fields with "N/A."
2. Convert dates to a standard format using the `lubridate` package:
    ```R
    library(lubridate)
    publish_date <- as.Date(publish_date, format = "%Y-%m-%d")
    ```
3. Save the cleaned data as a CSV for analysis:
    ```R
    write.csv(cleaned_data, "cleaned_articles.csv", row.names = FALSE)
    ```

---

### Task 4: Data Analysis and Visualization
1. Conduct a keyword frequency analysis:
    ```R
    keyword_freq <- table(unlist(strsplit(cleaned_data$Keywords, ", ")))
    ```
2. Create a simple bar chart to show keyword frequency:
    ```R
    library(ggplot2)
    ggplot(data = as.data.frame(keyword_freq), aes(x = Var1, y = Freq)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      xlab("Keywords") + ylab("Frequency") +
      ggtitle("Most Frequent Keywords in Articles")
    ```
3. Save the plot as an image for the presentation.

---

### Task 5: Report and Insights
Prepare a **PowerPoint presentation** with the following slides:
1. **Introduction**:
   - Project overview and objectives.
2. **Setup**:
   - Description of tools and libraries used.
3. **Web Scraping**:
   - Key code snippets for scraping data.
4. **Data Cleaning**:
   - Challenges and solutions during preprocessing.
5. **Visualization**:
   - Keyword frequency chart and any other insights.
6. **Challenges**:
   - Common issues (e.g., CAPTCHA, inconsistent data) and how they were resolved.
7. **Team Contributions**:
   - Describe individual contributions.
8. **Conclusion**:
   - Summarize findings and discuss potential future work.

---

## How to Run
Open the R project in RStudio.
1. Run the script `project.R`.
2. View the output CSV and visualizations in the working directory folder.
