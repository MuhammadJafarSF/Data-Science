library(rvest)

baseUrl <- "https://www.tripadvisor.com"
hotelUrl <- "/Hotels-g294230-Yogyakarta_Region_Java-Hotels.html"

url <- paste(baseUrl, hotelUrl, sep = "")
webpage <- read_html(url)
  
hotelName <- webpage %>% html_nodes('.prominent') %>% html_text()
hotelURL <- webpage %>% html_nodes('.prominent') %>% html_attr('href')

hotels <- data.frame(name = hotelName, link = hotelURL, stringsAsFactors = FALSE)

# set direktori untuk simpan data
setwd("D:/project_final/data")
# simpan data
saveRDS(hotels, "hotels.rds")