library("tidyverse")
library("stringr")

sp500 <- read_csv("all_stocks_5yr.csv")
sp500
stock <- sp500 %>% filter(Name == "AAPL") %>% select(date, close) %>% arrange(date)

SAX <- function(ts, n, alpha) {
  breakpoints <- qnorm(seq(1/n, 1-1/n, length=n-1), mean=0, sd=1)
  means <- apply(matrix(ts, nrow=n), 2, mean)
  symbols <- cut(means, breakpoints, labels=LETTERS[1:(n-1)])
  SAX_string <- paste(symbols, collapse="")
  compressed_ts <- apply(matrix(ts, nrow=n), 2, mean)
  return(list(SAX_string=SAX_string, compressed_ts=compressed_ts))
}

ts <- stock$close
N <- length(ts)
alpha <- 3
n <- round(alpha * sqrt(N))
SAX_result <- SAX(ts, n=35.5, alpha=3)

cat("SAX string: ", SAX_result$SAX_string, "\n")
cat("Compressed time series: ", SAX_result$compressed_ts, "\n")

length(SAX_result$compressed_ts)
