# ------------------------------- #
# Automated Web Scraping and Analysis of Journal Articles
# ------------------------------- #

# Load Required Libraries
# Ensure that the required libraries are installed, and then load them.
packages <- c("rvest", "dplyr", "xml2", "httr", "stringr", "ggplot2", "viridis", "tidyr")
install_if_missing <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p) # Install missing packages
  }
}
lapply(packages, install_if_missing) # Install any missing packages

# Load libraries
library(rvest)  # Web scraping
library(dplyr)  # Data manipulation
library(xml2)   # XML parsing
library(httr)   # HTTP requests
library(stringr) # String manipulation
library(ggplot2) # Data visualization
library(viridis) # Color palettes
library(tidyr)  # Tidy data

# ------------------------------- #
# Helper Functions for Data Extraction
# ------------------------------- #

# Function to extract the abstract of a journal article
get_abstract_text <- function(journal_link) {
  # Read the journal's webpage
  journal_page <- read_html(journal_link)
  # Extract the abstract text
  abstract_text <- journal_page %>%
    html_nodes("#Abs1-content p") %>% # Locate the abstract section
    html_text() %>%                   # Extract the text
    paste(collapse = ",")             # Combine multiple paragraphs, if any
  return(ifelse(nchar(abstract_text) == 0, "", abstract_text)) # Return empty string if no abstract found
}

# Function to extract the publication date
get_journal_date <- function(journal_link) {
  # Read the journal's webpage
  journal_page <- read_html(journal_link)
  # Extract the publication date
  journal_date <- journal_page %>%
    html_nodes(".c-article-identifiers__item time") %>% # Locate the date element
    html_text()
  return(ifelse(nchar(journal_date) == 0, "", journal_date)) # Return empty string if no date found
}

# Function to extract corresponding author's information
get_corresponding_author <- function(journal_link) {
  journal_page <- read_html(journal_link)
  # Extract author details
  corauth <- journal_page %>%
    html_nodes("#corresponding-author-list") %>% # Locate corresponding author section
    html_text() %>%                              # Extract the text
    paste(collapse = ",")
  # Remove unnecessary text like "Correspondence to"
  corauth <- sub("^\\s*Correspondence to\\s*", "", corauth)
  return(ifelse(nchar(corauth) == 0, "", corauth))
}

# Function to extract keywords of the article
get_keywords <- function(journal_link) {
  journal_page <- read_html(journal_link)
  # Extract keywords
  keywrd <- journal_page %>%
    html_nodes(".c-article-subject-list__subject a") %>% # Locate keyword elements
    html_text(trim = TRUE) %>%                           # Extract and trim text
    paste(collapse = ",")                                # Combine into a single string
  return(ifelse(nchar(keywrd) == 0, "", keywrd))
}

# Function to extract corresponding author's email
get_author_email <- function(journal_link) {
  journal_page <- read_html(journal_link)
  # Extract email address
  email_full <- journal_page %>%
    html_nodes("#corresponding-author-list a") %>% # Locate email links
    html_attr("href") %>%                           # Get the 'mailto' links
    paste(collapse = ",")
  # Remove "mailto:" prefix from email
  em <- gsub("mailto:", "", email_full)
  return(ifelse(nchar(em) == 0, "", em))
}

# ------------------------------- #
# Data Scraping and Aggregation
# ------------------------------- #
# Function to convert year to volume number
year_to_volume <- function(year) {
  if (year < 2010 || year > 2024) {
    stop("Year must be between 2010 and 2024.")
  }
  volume <- year - 2009 # Volume 1 corresponds to 2010, so subtract 2009
  return(volume)
}

# Function to get the total number of pages
get_total_pages <- function(base_url) {
  # Read the first page
  first_page <- read_html(base_url)
  
  # Extract the pagination text (e.g., "Page 1 of 1")
  pagination_text <- first_page %>%
    html_node(".u-text-sm") %>% # Locate the pagination text element
    html_text(trim = TRUE)
  
  # Extract the total number of pages using a regular expression
  total_pages <- as.numeric(gsub(".*Page \\d+ of (\\d+).*", "\\1", pagination_text))
  
  return(total_pages)
}

input_year <- as.integer(readline(prompt = "Enter the year (2010-2024): "))
volume_number <- year_to_volume(input_year)
cat("The corresponding volume number is:", volume_number, "\n")

# Initialize an empty data frame to store the results
scrape_journal_volume <- function(volume) {
  df <- data.frame()
  base_url <- paste0(
    "https://mobilednajournal.biomedcentral.com/articles?query=&volume=", 
    volume, "&searchType=&tab=keyword"
  )
  total_pages <- get_total_pages(base_url)
  # Loop through multiple pages of the journal website
  for (page in seq(from = 1, to = total_pages)) { 
    # Construct the URL for the current page
     url <- paste0(
      "https://mobilednajournal.biomedcentral.com/articles?query=&volume=", 
      volume, "&searchType=&tab=keyword&page=", 
      page
    )
    
    # Load the webpage
    journal <- read_html(url)
    print(paste("Processing -", url))
    # Extract key details: title, authors, and links to individual articles
    title <- journal %>% html_nodes(".c-listing__title a") %>% html_text()
    authors <- journal %>% html_nodes(".c-listing__authors-list") %>% html_text()
    journal_links <- journal %>% html_nodes(".c-listing__title a") %>% 
      html_attr("href") %>%
      paste("https://mobilednajournal.biomedcentral.com", ., sep = "")
    
    # Use helper functions to scrape additional details for each article
    abstract_text <- sapply(journal_links, FUN = get_abstract_text, USE.NAMES = FALSE)
    publish_date <- sapply(journal_links, FUN = get_journal_date, USE.NAMES = FALSE)
    keywords <- sapply(journal_links, FUN = get_keywords, USE.NAMES = FALSE)
    email <- sapply(journal_links, FUN = get_author_email, USE.NAMES = FALSE)
    corr_author <- sapply(journal_links, FUN = get_corresponding_author, USE.NAMES = FALSE)
    
    # Combine the extracted details into a single data frame
    df <- rbind(df, data.frame(title, authors, abstract_text, publish_date, keywords, email, corr_author))
  }
  return(df)
}

df <- scrape_journal_volume(volume_number)

# ------------------------------- #
# Data Cleaning and Preprocessing
# ------------------------------- #
df <- df %>%
  mutate(across(where(is.character), ~ replace_na(., ""))) %>%  # Replace NA with empty strings
  mutate(across(where(is.character), ~ str_trim(.))) %>%       # Trim whitespace
  distinct() %>%                                               # Remove duplicate rows
  mutate(publish_date = as.Date(publish_date, format = "%d %B %Y")) # Convert date to Date format

# ------------------------------- #
# Visualization: Publication Trends Over Time
# ------------------------------- #

publication_trends <- df %>%
  mutate(year_month = as.Date(paste0(format(publish_date, "%Y-%m"), "-01"))) %>% # Convert 'Year-Month' to a Date object
  group_by(year_month) %>%                                                      # Group data by 'Year-Month'
  summarise(Count = n()) %>%                                                    # Count the number of publications for each 'Year-Month'
  complete(year_month = seq(min(year_month), max(year_month), by = "month"),    # Fill in missing months
           fill = list(Count = 0))                                              # Set missing counts to 0


# Create a line plot with viridis color palette
ggplot(publication_trends, aes(x = year_month, y = Count)) +    
  geom_line(size = 0.5) +                                                         
  geom_point(size = 1) +                                                        
  labs(title = "Publication Trends Over Time",                                  
       x = "Year-Month",                                                       
       y = "Number of Publications") +                                         
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +                # Show all months on x-axis with formatted labels
  theme_minimal() +                                                             
  theme(axis.text.x = element_text(angle = 45, hjust = 1),                      
        plot.title = element_text(size = 14, face = "bold"))                              


# ------------------------------- #
# Visualization: Top 10 Corresponding Authors
# ------------------------------- #

# Prepare the data for visualization
author_counts <- df %>%
  count(corr_author, sort = TRUE) %>%       # Count the number of articles for each corresponding author, sorted in descending order
  filter(corr_author != "") %>%             # Remove rows with empty corresponding author names
  head(10)                                  # Keep only the top 10 corresponding authors

# Create a horizontal bar chart using the viridis color palette
ggplot(author_counts, aes(x = reorder(corr_author, n), y = n, fill = n)) + # Use fill color based on the article count
  geom_bar(stat = "identity") +                                        # Create a bar chart
  geom_text(aes(label = n),                                            # Add text labels with the count
            hjust = -0.2,                                              # Position the labels slightly outside the bars
            size = 4) +
  coord_flip() +                                                       # Flip coordinates to make it horizontal
  scale_fill_viridis_c(option = "D") +                                 # Apply viridis color scale (continuous)
  labs(title = "Top 10 Corresponding Authors",                         # Add a plot title
       x = "Author",                                                   # Label for x-axis
       y = "Number of Articles") +                                     # Label for y-axis
  theme_minimal() +                                                    # Apply a clean and minimal theme
  theme(axis.text.x = element_text(size = 10),                         # Adjust axis text size
        axis.text.y = element_text(size = 10),                         # Adjust y-axis text size
        plot.title = element_text(size = 14, face = "bold"))           # Make the title larger and bold

# ------------------------------- #
# Analysis: Keyword Frequency
# ------------------------------- #

# Split keywords into a list for counting
keyword_list <- unlist(strsplit(paste(df$keywords, collapse = ","), ",")) # Split keywords
keyword_list <- str_trim(keyword_list)                                    # Trim whitespace
keyword_list <- tolower(keyword_list)                                     # Convert to lowercase
keyword_list <- keyword_list[!keyword_list %in% c("", "n/a")]             # Remove empty or invalid entries

# Count the frequency of each keyword
keyword_counts <- as.data.frame(table(keyword_list)) # Tabulate frequencies
colnames(keyword_counts) <- c("Keyword", "Count")    # Rename columns
keyword_counts <- keyword_counts[order(-keyword_counts$Count), ] # Sort by count (descending)

# ------------------------------- #
# Visualization: Top 10 Keywords
# ------------------------------- #

ggplot(keyword_counts[1:10, ], aes(x = reorder(Keyword, Count), y = Count, fill = Count)) + # Map 'Count' to the fill aesthetic
  geom_bar(stat = "identity") +                                                          # Create a bar chart
  geom_text(aes(label = Count),                                                         # Add text labels with the count
            hjust = -0.2,                                                               # Position the labels slightly outside the bars
            size = 4) +                                                                 # Set the text size
  scale_fill_viridis_c(option = "D") +                                                  # Apply viridis color scale (continuous)
  coord_flip() +                                                                         # Flip coordinates for a horizontal bar chart
  labs(title = "Top 10 Keywords",                                                       # Add a plot title
       x = "Keyword",                                                                    # Label for x-axis
       y = "Frequency",                                                                  # Label for y-axis
       fill = "Keyword Frequency") +                                                    # Add legend title for fill
  theme_minimal() +                                                                      # Apply a clean and minimal theme
  theme(axis.text.x = element_text(size = 10),                                          # Adjust x-axis text size
        axis.text.y = element_text(size = 10),                                          # Adjust y-axis text size
        plot.title = element_text(size = 14, face = "bold"),                            # Make title bold and larger
        legend.position = "right")                 

# ------------------------------- #
# Save Data
# ------------------------------- #

# Save the final data frame to a CSV file
write.csv(df, "final_scraped_data.csv", row.names = FALSE)
