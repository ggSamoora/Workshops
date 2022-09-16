# set the locale to Arabic
Sys.setlocale("LC_ALL","Arabic")

# load necessary packages
library(tidyverse) # for data wrangling
library(rvest) # for web scraping
library(data.table) # for faster merging of data

# start out with one link
# NOTE: You may need to change the link in here, because the url may have expired. Please visit the main page through the following url
# "https://om.opensooq.com/ar/%D8%B9%D9%82%D8%A7%D8%B1%D8%A7%D8%AA-%D9%84%D9%84%D8%A7%D9%8A%D8%AC%D8%A7%D8%B1/%D8%B4%D9%82%D9%82-%D9%84%D9%84%D8%A7%D9%8A%D8%AC%D8%A7%D8%B1"
# then click on any apartment with a price and use that url as the url variable
url <- "https://om.opensooq.com/ar/search/188269493/%D8%A8%D9%86%D8%A7%D9%8A%D9%87-%D8%AC%D8%AF%D9%8A%D8%AF%D9%87-%D9%81%D8%A7%D8%AE%D8%B1%D9%87-%D9%84%D9%84%D8%A7%D9%8A%D8%AC%D8%A7%D8%B1-%D8%A8%D9%85%D8%B7%D9%82%D9%87-%D8%A7%D9%84%D9%82%D9%88%D9%81"

# read the url as an html
page <- read_html(url)

# removes values and keeps headers
headers_and_values <- page %>% html_elements(xpath = "//li[@class='inline vTop relative mb15']") %>% 
  html_text() %>% 
  str_remove_all("\n") %>% 
  str_squish()

# create the empty dataframe
df <- matrix(ncol=10) %>% as.data.frame()

# headers only
headers_only <- headers_and_values %>% 
  str_remove_all("[:].*") %>% 
  str_squish()

# set the headers_only as the column names of df
colnames(df) <- c(headers_only)

# function to extract the values from the value-header combination
return_value <- function(h, h_v) {
  val <- h_v[grepl(h, h_v)] %>% 
    str_remove(h) %>% 
    str_remove("[:]") %>% 
    str_squish()
  
  if (length(val) == 0) {
    return(NA)
  } else {
    return(val)
  }
}

# apply the function to extract the values from the headers_and_values object
values_only <- map(headers_only, function(x) {return_value(x, headers_and_values)})

# add the values as a row to the df variable
df <- rbindlist(list(df, values_only))

# Now let's start with script to collect data from all apartments
#----------------------------------------------------------------

# the main URL (which contains the list of apartments)
url_main <- "https://om.opensooq.com/ar/%D8%B9%D9%82%D8%A7%D8%B1%D8%A7%D8%AA-%D9%84%D9%84%D8%A7%D9%8A%D8%AC%D8%A7%D8%B1/%D8%B4%D9%82%D9%82-%D9%84%D9%84%D8%A7%D9%8A%D8%AC%D8%A7%D8%B1"

# for loop to iterate over the first 3 pages of apartment postings
for (pg in 1:3) {
  
  # visit the nth page of apartments
  url <- paste0(url_main, "?page=", pg)
  
  # store the url in an html object
  page <- read_html(url)
  
  # extract the URLs of each apartment posting
  url_list <- page %>% html_elements(xpath = "//h2[@class='fRight mb15']") %>% 
    html_elements(xpath = ".//a") %>% 
    html_attr("href")
  
  # for loop to iterate over the URLs of the apartment postings
  for (i in 1:length(url_list)) {
    
    # give the code a 1 second break to prevent errors
    Sys.sleep(1)
    
    # store the apartment posting url as an html object
    page_2 <- read_html(paste0("https://om.opensooq.com", url_list[i]))
    
    # extract the headers and values
    headers_and_values <- page_2 %>% html_elements(xpath = "//li[@class='inline vTop relative mb15']") %>% 
      html_text() %>% 
      str_remove_all("\n") %>% 
      str_squish()
    
    # extract the values only
    values_only <- map(headers_only, function(x) {return_value(x, headers_and_values)})
    
    # append the values as a row to the df variable
    df <- rbindlist(list(df, values_only))
    
    # progress bar of each apartment posting
    {Sys.sleep(0.1); cat("\r",i)}
  }
  
  # progress bar of each apartment posting page
  print(paste("Page:", pg, "Complete!"))
  
  # allow code to sleep for 3 seconds to prevent errors
  Sys.sleep(3)
}

# remove the first row of NAs from the dataset
df2 <- df %>% slice(-1L)

# write the dataframe to a csv file for future analysis
write_csv(df2, "All Oman Apartment Data.csv")
