##################################
#### Arctic Scraping Tutorial ####
##################################

# Presented by Paul Love

# We are going to get scrape data about Arctic Council documents.
# Scraping allows us to access data that is not provided through an API.
                                                                                                                        
#### Getting ready #### 
# We are going to use the pacman package to manage the loading and install of packages.
# Install the package if you do not already use pacman.
# Alternatively, individually install and load tidyverse, rvest, and polite packages.

#install.packages("pacman")

pacman::p_load(tidyverse, rvest, polite)
# Set your working directory
#setwd()
main_url <- "https://oaarchive.arctic-council.org/"

# Install Selector Gadget to easily identify the css selector you want to use
# Visit https://selectorgadget.com/
# Either drag the bookmarklet to your browser's toolbar or install the Chrome Extension
# The bookmarklet works in Firefox and Chrome. I cannot attest to other browsers compatibility.
# Installation instructions and a tutorial video is available at the website
# If you are comfortable with using your browser's web developer tools, you can also access the selectors using only the browser.

# Check if web scraping is permitted by the site and if there are crawl delays
bow <- bow(main_url)
bow

# This site allows scrapping with a 10 second delay. This can be done with custom function.

slow_read <- function (url) {
  read <- read_html(url)
  Sys.sleep(10) #10 second pause
  read
}

#### Format search ####
# Copied URL from page 2 of the results
copied_url <- "https://oaarchive.arctic-council.org/discover?rpp=100&etal=0&scope=/&group_by=none&page=2&sort_by=dc.date.issued_dt&order=asc&filtertype_0=author&filter_relational_operator_0=equals&filter_0=Arctic+Council"

# Note the various parameters in the URL.
# rpp = 100 shows that 100 responses are requested per page
# sort_by = dc.date.issues_dt shows that those results should be displayed in descending order by date
# filter = Arctic+Council limits the scope of the search to those documents directly authored by the Arctic Council

# Alter the copied URL to allow information to be imputed from a list containing the range of possible page numbers at the time of scraping.
# Note that there are 5 pages at 2021-01-25 02.32.40 UTC and create a variable containing 1 through 42
pn <- 1:5

# Remove the current page number parameter and replace it with {pn}, where the curly brackets indicate what is to imputed
q_url <- str_glue("https://oaarchive.arctic-council.org/discover?rpp=100&etal=0&scope=/&group_by=none&page={pn}&sort_by=dc.date.issued_dt&order=asc&filtertype_0=author&filter_relational_operator_0=equals&filter_0=Arctic+Council")

#Confirm that the page numbers have been added
head(q_url)

# Use Selector Gadget to find the css selector that corresponds with each document entry.

css_select <- "#aspect_discovery_SimpleSearch_div_search-results a"
read_html(q_url[1]) %>%
  html_nodes(css = css_select) %>%
  html_attr(name = "href")

# Since there are 200 results, it appears upon inspection that each href attribute is repeated. Those duplicates can be removed later.

#### Automate scraping with purrr ####

p_css <- partial(html_nodes, css = css_select) #Prefill css selector 
p_attr <- partial(html_attr, name = "href") #Pre-fill html attribute

#Build a purrr custom function
p_read <- compose(p_attr,
                  p_css,
                  slow_read #custom function from earlier with crawl delay
                  )

scrape <- map(q_url, safely(p_read)) %>% set_names(q_url)
# Save your work
#saveRDS(scrape, "scrape.rds")

#### Review and format document URLs ####
scrape_result <- scrape %>% transpose()

# Check if the errors are all empty, using the following logical evaluation
sum(str_detect(scrape_result$error, "")) == length(scrape_result$error)

# Create a tibble of all of the results and bind all rows using map_dfr
scrape_res <- map_dfr(scrape_result$result, ~tibble(href = .x))

# First result full link
# https://oaarchive.arctic-council.org/handle/11374/2047

# The href attributes scraped are only a portion of the URL of each document. A base URL must be added to the beginning.
base_url <- "https://oaarchive.arctic-council.org"

scrape_res_mod <- scrape_res %>%
  distinct(href) %>% # Remove duplicate href values
  mutate(doc_url = str_c(base_url,href)) #append base_url to the front of href

#### Scraping the metadata ####
# Use a custom function to extract metadata on each document, creating a tibble for each document
meta_extract <- function (doc_url) {
        #Scrape data from page with 10 second delay
        data <- slow_read(doc_url)
        
        title <- html_nodes(data, css = ".first-page-header") %>%
          html_text()
        
        abstract <- html_nodes(data, css = ".simple-item-view-description div") %>%
          html_text()
        
        date <- html_nodes(data, css = ".simple-item-view-date") %>%
          html_text(trim = TRUE) %>% #Remove the white space indicating a next line
          str_sub(start = 5, end = 8) # Remove the word "Date" and extract the year
        
        author <- html_nodes(data, css = ".simple-item-view-authors div") %>%
          html_text()
        
        tib <- tibble(title, date, author, abstract, doc_url)
        
        tib
}

collected_meta_test <- map(scrape_res_mod$doc_url[1:2], safely(meta_extract)) %>%
  transpose()

# Inspect the test scrape
collected_meta_test

# Estimated time for scraping with delay in minutes is... 
length(scrape_res_mod$doc_url) * 10 / 60

# Begin scraping a sample of only 5 documents
collected_meta <- map(scrape_res_mod$doc_url[1:5], safely(meta_extract)) %>%
  transpose() # Transpose list to combine list by result and error outcomes

#### Review and format collected metadata
collected_meta_result <- collected_meta$result %>%
  bind_rows() # Combine all of the results in to one tibble

collected_meta_result

collected_meta_error <- collected_meta$error %>% 
  bind_rows() # Combine all of the errors in to one tibble

collected_meta_error

#Save your work
#saveRDS(collected_meta_result, "collected_meta_result.rds")

# Load previously scraped rds file with full data, so we can explore the data

#collected_meta_result <- readRDS("collected_meta_result.rds")

#### Exploring the data ####
# How many documents were released in 1998?
collected_meta_result %>%
  filter(date == 1998) %>%
  count()

# How many documents refer to Canada?
collected_meta_result %>%
  mutate(canada = grepl("canad", collected_meta_result$abstract, ignore.case = TRUE)) %>%
  filter(canada == TRUE) %>%
  count()

# How many documents refer to the EU?
collected_meta_result %>%
  mutate(eu = grepl("European Union", collected_meta_result$abstract, ignore.case = FALSE)) %>%
  filter(eu == TRUE) %>%
  count()