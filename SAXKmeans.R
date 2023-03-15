library("tidyverse")
library("stringr")
library(purrr)

sp500 <- read_csv("all_stocks_5yr.csv")
sp500
sax_data <- sp500 %>%
  group_by(Name) %>%
  summarise(SAX_string = map_chr(open, ~SAX(.x, n=35.5, alpha=3)))

SAX <- function(ts, n, alpha) {
  breakpoints <- qnorm(seq(1/n, 1-1/n, length=n-1), mean=0, sd=1)
  means <- apply(matrix(ts, nrow=n), 2, mean)
  symbols <- cut(means, breakpoints, labels=LETTERS[1:(n-1)])
  SAX_string <- paste(symbols, collapse="")
  return(SAX_string)
}
num_distinct_points <- sax_data %>% 
  distinct() %>% 
  nrow()


sax_data <- na.omit(sax_data)
sax_data <- unique(sax_data)
sax_data$SAX_string <- as.numeric(sax_data$SAX_string)

k <- 100 # number of clusters
sax_clusters <- kmeans(as.matrix(stringr::str_split_fixed(sax_data$SAX_string, "", n), n), k)

sax_data <- cbind(sax_data, cluster = sax_clusters$cluster)
