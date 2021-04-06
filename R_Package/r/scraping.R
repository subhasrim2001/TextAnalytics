install.packages("rvest")
library(rvest)
news_html <- read_html("https://indianexpress.com/article/india/coronavirus-india-live-updates-second-wave-maharashtra-lockdown-restrictions-7259180/")
news_html_content <- html_nodes(news_html, xpath = "//p")
news_html_text <- html_text(news_html_content)
news_html_text

#use tm to parse further