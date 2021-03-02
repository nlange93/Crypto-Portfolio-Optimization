source("kraken_dictionary.R")
get_ohlc<- function(pair = "XBTEUR",
                          interval = "day",
                          since = FALSE) {

  Interval <- case_when(
    interval == "minute" ~ 1,
    interval == "5 minutes" ~ 5,
    interval == "hour" ~ 60,
    interval == "day" ~ 1440,
    interval == "week" ~ 10080)
  
  Since <- ifelse(since == FALSE, 0, as.numeric(as.POSIXct(since, format="%Y-%m-%d")))

  base_url <- "https://api.kraken.com/0/public/OHLC"
  url <- paste0(base_url, "?", "pair=", pair,
                "&since=", Since,
                "&interval=", Interval)

  ohlc_out <- jsonlite::fromJSON(url)
  return(ohlc_out)
}

get_return <- function(pairs = c("XBTEUR"), interval = "day", since = FALSE){
  #browser()
  out <- NULL
  i = 1
  for (Pair in pairs){
    request <- get_ohlc(pair = Pair, interval, since)[["result"]][[kraken_dictionary[[Pair]]]]
    request <-  as.data.frame(request) %>%
      select(V1,V2,V5) %>%
      mutate(time = as_datetime(as.numeric(as.character(V1))),
             open <- as.numeric(as.character(V2)),
             close <- as.numeric(as.character(V5)),
             return = (open/close)-1) %>% 
      select(time, return)
    if (i == 1){out = rbind(out, request)} else{
      out = left_join(out, request, by = "time")
    }
    i = i+1
  }
  names <- c("time", pairs)
  colnames(out)<- names
  out <- tibble::column_to_rownames(out, var= "time")
  return(out)
}

