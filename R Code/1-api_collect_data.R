# Use Pushshift API to access Reddit data
# install.packages("httr")
# install.packages("jsonlite")
library("httr") # make api calls
library("jsonlite") # process response data

setwd("/Users/pingjingyang/Documents/is527-network-science/is527")

###################################################################

# simple example to call pushshift api once
# https://github.com/pushshift/api
submission_url <- "https://api.pushshift.io/reddit/search/submission/"
params <- list(subreddit="academia", after=as.numeric(as.POSIXct("2023-01-01")), before=as.numeric(as.POSIXct("2023-01-30")), size="1000", sort="created_utc")
res <- GET(url=submission_url, query=params)
result <- jsonlite::fromJSON(httr::content(res, "text"))

# save RData
saveRDS(result, file="fname.RData")
loadRDS <- readRDS("fname.RData")

# write json data to external file
write(content(res, "text"), "output.json")

###################################################################
# collect all submissions in a specific subreddit
retrieve_submissions <- function(subreddit, start_date, end_date, page_size=1000) {
  
  submission_url <- "https://api.pushshift.io/reddit/search/submission/"
  comment_url <- "https://api.pushshift.io/reddit/search/comment/"
  
  after <- as.numeric(as.POSIXct(start_date))
  before <- as.numeric(as.POSIXct(end_date))
  
  all_submissions <- list()
  next_page <- TRUE
  
  while(next_page) {
    params <- list(subreddit=subreddit, after=after, before=before, size=page_size, sort="created_utc")
    res <- GET(url=submission_url, query=params)
    data <- content(res, as="parsed")$data
    
    if(length(data) > 0) {
      all_submissions <- c(all_submissions, data)
      before <- tail(data, n=1)[[1]]$created_utc - 1
      print(paste(before, "--before; after--", after))
    } else {
      next_page <- FALSE
    }
    Sys.sleep(1)
  }
  
  return(all_submissions)
}

subreddit <- "academia"
start_date <- "2023-01-01"
end_date <- "2023-01-30"

subs <- retrieve_submissions(subreddit, start_date, end_date)


###################################################################
# collect all comments in a specific subreddit
retrieve_comments <- function(subreddit, start_date, end_date, page_size=1000) {
  
  comment_url <- "https://api.pushshift.io/reddit/comment/search"
  
  after <- as.numeric(as.POSIXct(start_date))
  before <- as.numeric(as.POSIXct(end_date))
  
  all_comments <- list()
  next_page <- TRUE
  
  while(next_page) {
    params <- list(subreddit=subreddit, after=after, before=before, size=page_size, sort="created_utc")
    res <- GET(url=comment_url, query=params)
    data <- content(res, as="parsed")$data
    
    if(length(data) > 0) {
      all_comments <- c(all_comments, data)
      before <- tail(data, n=1)[[1]]$created_utc - 1
      #      print(paste(before, "--before; after--", after))
    } else {
      next_page <- FALSE
    }
    Sys.sleep(1)
  }
  
  return(all_comments)
}


subreddit <- "academia"
start_date <- "2023-01-01"
end_date <- "2023-01-30"

coms <- retrieve_comments(subreddit, start_date, end_date)


###################################################################

# create edgelist format
# source -> target

sub_comment_pair <- list()

subs_author <- list()
for (sub in subs){
  subs_author[[ sub$id ]] <- sub$author
}

# loop over every comments
for(com in coms) {
  link_id <- com$link_id
  link_id <- strsplit(link_id, "_")[[1]][2]
  if (link_id %in% names(subs_author)) {
    edge <- list("id"=link_id, "source"=subs_author[[link_id]], "target"=com$author)
    sub_comment_pair <- append(sub_comment_pair, list(edge))
  }
}
print(sub_comment_pair)


# Convert the list of lists to a data frame
my_df <- data.frame(do.call(rbind, sub_comment_pair))


# Print the data frame
print(my_df)

saveRDS(my_df, file="my_df.RData")
my_df <- readRDS("my_df.RData")
