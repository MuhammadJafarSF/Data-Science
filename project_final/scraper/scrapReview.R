#load library
library(rvest)

# get_hotel_reviews return data frame contains reviews and authors
baseUrl <- "https://www.tripadvisor.com"
hotelUrl <- "/Hotel_Review-g294230-d9589410-Reviews-Satoria_Hotel_Yogyakarta-Yogyakarta_Region_Java.html"
get_hotel_reviews <- function(hotelUrl, incProgress = NULL) {
  
  withProgress(message = "Collecting data ", value = 0, {
    
    reviewPage <- read_html(paste(baseUrl, hotelUrl, sep = ""))
    review <- reviewPage %>%
      html_nodes('.IRsGHoPm') %>%
      html_text()
    
    reviewer <- reviewPage %>%
      html_nodes('._1r_My98y') %>%
      html_text()
    
    reviews <- character()
    reviewers <- character()
    reviews <- c(reviews, review)
    reviewers <- c(reviewers, reviewer)
    
    nextPage <- reviewPage %>%
      html_nodes('.next') %>%
      html_attr('href')
    
    while (!is.na(nextPage) && length(reviews) < 300) {
      incProgress(10/length(reviews), detail = paste(length(reviews), " data"))
      print(paste(length(reviews), "data", "collected"))
      
      reviewUrl <- paste(baseUrl, nextPage, sep = "")
      reviewPage <- read_html(reviewUrl)
      
      review <- reviewPage %>%
        html_nodes('.IRsGHoPm') %>%
        html_text()
      
      reviewer <- reviewPage %>%
        html_nodes('._1r_My98y') %>%
        html_text()
      
      reviews <- c(reviews, review)
      reviewers <- c(reviewers, reviewer)
      
      nextPage <- reviewPage %>%
        html_nodes('.next') %>%
        html_attr('href')
    }
    
    totalReviews <- length(reviews)
    
    print(paste(length(reviews), "data", "collected"))
    
  })
  
  return(data.frame(reviewer = reviewers, review = reviews, stringsAsFactors = FALSE))
}
