# === Project Setup ===
Data_folder = "DATA/" 
Output_folder = "OUTPUT/"
Project_name = "News_Images"

# === File I/O and Data Handling ===
library(readr)        # Reading delimited files (CSV, TSV, etc.)
library(readxl)       # Reading .xls/.xlsx Excel files
library(openxlsx)     # Alternative for reading/writing Excel files

# === Text and Language Processing ===
library(quanteda)     # Text analysis and corpus management
library(tidytext)     # Tokenization, sentiment dictionaries, tf-idf, etc.
library(cld2)         # Language detection
library(stringi)      # High-performance string processing (Unicode-aware)
library(stringr)      # Tidy string manipulation (part of tidyverse)

# === Topic Modeling and NLP Utilities ===
library(topicmodels)  # Latent Dirichlet Allocation (LDA), CTM, etc.
library(ldatuning)    # Choose optimal number of topics (e.g., K in LDA)

# === Data Manipulation and Visualization ===
library(tidyverse)    # Collection of core tidy tools (ggplot2, dplyr, tidyr, etc.)
library(dplyr)        # Data manipulation (also in tidyverse)
library(tidyr)        # Data tidying (also in tidyverse)
library(ggplot2)      # Grammar of Graphics plotting
library(scales)       # Axis formatting, rescaling (for ggplot2)
library(ggthemes)     # Extra themes for ggplot2 (e.g., WSJ, Economist)

# === Date and Time Handling ===
library(lubridate)    # Simplified date and time manipulation

# === Parallel and Efficient Computing ===
library(doParallel)   # Parallel backend for foreach

# === Network and Graph Analysis ===
library(igraph)       # Network analysis and graph theory

# === Image Processing (if used for thumbnails/visuals) ===
library(magick)       # Image reading, editing, conversion
library(daiR)         # OCR w/ Google Document AI

# === Intercoder Reliability ===
library(irr)          # Reliability metrics (e.g., Krippendorff's Alpha)
library(ragree)       # Additional agreement coefficients

### SECTION: Newswhip API -----

num_articles <- 250

###############################################
# everything below can be run without changes #
###############################################

###########################################
# calling the newship API to get articles #
###########################################

# setup API key and set the URL of the endpoint

# create a variable that has the API key in it
api_key <- Sys.getenv("nw_api_key")

get_newswhip_articles <- function(api_key, limit, start_time, end_time) {
  api_endpoint <- paste0('https://api.newswhip.com/v1/articles?key=', api_key)          
  data <- paste0('{\"filters\": [\"language:en AND country_code:us\"], 
                           \"size\": ', limit, ', 
                           \"from\": ', start_time, ',
                           \"to\": ', end_time, ',
                           \"search_full_text\": true,
                           \"find_related\": false}')
  r <- httr::POST(api_endpoint, body = data)
  httr::stop_for_status(r)         
  jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"), flatten = TRUE)$articles          
}

################################################################
################################################################
###### You only need to change the days ########################
################################################################
################################################################

# start_date <- as.Date("2025-05-01")
# end_date <- as.Date("2025-05-12")
# days <- as.character(as.Date(as.Date(start_date):as.Date(end_date), origin="1970-01-01"))

## change to weekly
start_date <- as.Date("2024-06-01")
end_date <- as.Date("2024-12-31")
days <- as.character(seq(as.Date(start_date), as.Date(end_date), by = "7 days"))


mylist <- list()

for (i in days) {
  print("now running days:")
  print(i)
  
  start_loop_time <- Sys.time()
  
  Sys.sleep(runif(1, 0.3, 10))
  start_time <- as.numeric(as.POSIXct(paste(i, "00:00:00 EST", sep=" "))) * 1000 # datetime conversion
  end_time <- as.numeric(as.POSIXct(paste(as.Date(paste(i)) + 7,  "00:00:00 EST", sep=" "))) * 1000 - 1 # datetime conversion change to weekly by adding +7
  
  elapsed_time <- Sys.time() - start_loop_time
  if (elapsed_time > 10) {
    print(paste("Skipping day", i, "because it took more than 10 seconds"))
    next
  }
  
  data_temp <- get_newswhip_articles(api_key = api_key, limit = num_articles, start_time = start_time, end_time = end_time) # datetime conversion
  data_temp$date_time <- as.POSIXct(round(data_temp$publication_timestamp/1000), origin="1970-01-01") # datetime conversion
  data_temp$date <- as.Date(as.POSIXct(round(data_temp$publication_timestamp/1000), origin="1970-01-01")) # datetime conversion
  data_temp$relatedStories <- NULL
  data_temp$topics <- NULL
  data_temp$authors <- NULL
  data_temp$entities <- NULL
  data_temp$videos <- NULL
  
  try(data_temp <- data_temp |> dplyr::select(
    uuid, 
    publication_timestamp, 
    link, 
    headline, 
    excerpt, 
    keywords, 
    image_link, 
    has_video, 
    fb_data.total_engagement_count, 
    fb_data.likes, 
    fb_data.comments, 
    fb_data.shares, 
    tw_data.tw_count, 
    source.publisher, 
    source.domain, 
    source.link, 
    source.country, 
    source.country_code, 
    source.language, 
    date_time, 
    date))
  
  mylist[[i]] <- data_temp
}

##add month and year name, or if you are pulling for a full year just put year
data_temp <- do.call("rbind",mylist) |> data.frame()

### make sure about the language
data <- data_temp


start_date <- format(start_date, "%Y_%m_%d")
end_date <- format(end_date,  "%Y_%m_%d")
write.csv(data, 
          paste0(Data_folder, Project_name, "_", start_date, "_", end_date, "_NewswhipCollection.csv"))

save.image(paste0(Data_folder, Project_name, "_", start_date, "_", end_date, ".RData"))

summary(data$fb_data.total_engagement_count)

data2 <- filter(data, fb_data.total_engagement_count > 0)

summary(data2$fb_data.total_engagement_count)

data2$article_index <- paste0("article_", seq(nrow(data2)))

data2 <- data2 |> 
  distinct(link, .keep_all = TRUE) |> 
  distinct(image_link, .keep_all=TRUE)

write.csv(data2, 
          paste0(Data_folder, Project_name, "_", start_date, "_", end_date, "_NewswhipCollection.csv"),
          row.names = FALSE)

save.image(paste0(Data_folder, Project_name, "_", start_date, "_", end_date, ".RData"))

rm(list=setdiff(ls(), c("data2", "Project_name", "Output_folder", "Data_folder")))

#### SECTION: Post Image Captioning -----

# Load and join the data
captions_file <- read_csv("OUTPUT/captions_file.csv")
captions_file2 <- read_csv("OUTPUT/captions_file2.csv")

data_temp2 <- full_join(captions_file, captions_file2)

rm(captions_file, captions_file2)

# combine with the master file

data_temp2$article_index <- gsub(".*/(article_\\d+)\\.jpg", "\\1", data_temp2$image_path)
write.csv(data_temp2, 
          file = paste0(Output_folder, Project_name, "study2_captions.csv"),
          row.names = FALSE)


data3 <- left_join(data_temp2, data2, by = "article_index")

save.image(paste0(Data_folder, Project_name, "_Raw_Data", ".RData"))

rm(list=setdiff(ls(), c("data3", "Project_name", "Output_folder", "Data_folder")))

sources = data2 |> group_by(source.publisher) |> count() |> arrange(-n)
write.csv(sources,
          file = paste0(Output_folder, Project_name, "NW_Sources_Count.csv"),
          row.names = FALSE)
engagamenet = data2 |> group_by(source.publisher) |> summarize(engagement = sum(fb_data.total_engagement_count)) |> arrange(-engagement)
write.csv(engagamenet,
          file = paste0(Output_folder, Project_name, "NW_Sources_Engagement.csv"),
          row.names = FALSE)

temp_table <- left_join(sources, engagamenet)
temp_table$engpern <- temp_table$engagement / temp_table$n

openxlsx::write.xlsx(temp_table,
                     file = paste0(Output_folder, Project_name, "NW_Sources_Count_Engagement.xlsx"))

rm(sources, engagamenet, temp_table)

### SECTION: Load Data ------

data <- data3
rm(data3)

data$text<-data$caption                                

## clean for specific language
data2<-data
data2<-data2[data2$caption!="Failed to get caption",]


data2$CLD2<-cld2::detect_language(data2$text)
data2<-data2 |> filter(CLD2=="en")

## calculate and examine text length
data2$nwords <- str_count(data2$text, "\\w+")

nrow(data2)
length(which(data2$nwords==0))
length(which(data2$nwords<3))
length(which(data2$nwords<10))
length(which(data2$nwords<20))
length(which(data2$nwords<50))
length(which(data2$nwords<100))

## check by hand the samples to decide
TEMP<-data2[which(data2$nwords<4),]
TEMP<-TEMP[which(TEMP$nwords>2),]

# in this case 2 and up seems fine
data3<-data2[which(data2$nwords>3),]                         
data3$index <- data3$article_index

### SECTION: Pre-LDA Tidy ----

mystopwords<-c(stopwords("en"),stopwords(source = "smart"),"http","https","image","picture",
               "colors", "colored", "colorful", "red", "blue", "green", "yellow", "orange", "purple", "pink", "brown",
               "black", "white", "gray", "cyan", "magenta", "maroon", "lime", "navy", "teal", "silver",
               "dark","background", "blurry","blurred","visible","fuzzy", "reads", "overlay", "caption",
               "subtitles", "bright",
               "reads","read","reading", "overlaid", "letters", "shapes","foreground",
               "top","bottom","front","back","left","right","center","side",
               "text","logo",
               "nbc","news","present","banner","breaking","headline","studio","ticker")


# creating tidy tokens
tidy_data<-data3 |>
  unnest_tokens(word, text) |> # tokenizing
  # option 1 REGEX --># Be careful this is deangerous as it gets word parts
  #filter(!grepl(paste(mystopwords, collapse="|"), word)) |>  
  # option 2 Exact Match -->
  anti_join(data.frame(word=mystopwords)) |>
  mutate(nchar=nchar(word)) |> #counting the number of letters
  filter(nchar>2) |> # remove from minumum number of chars
  filter(!grepl("[0-9]{1}", word)) |> # removing numbers 
  filter(!grepl("\\W", word))  # removing any word containing non letter/number 

# choosing top words by tf-idf                            
maxndoc=0.5
minndoc=0.00005

# filter to tokens not too common and not too rare
templength<-length(unique(tidy_data$index))

good_common_words <- tidy_data |>
  count(index, word, sort = TRUE) |>
  group_by(word) |>
  summarize(doc_freq=n()/templength) |>
  filter(doc_freq<maxndoc) |>
  filter(doc_freq>minndoc)

# clean tidy to fit the tokens - NOTE: this is where you might lost indexes
tidy_data_pruned<-tidy_data |> inner_join(good_common_words)


tidy_data_pruned |>
  group_by(word) |>
  dplyr::summarise(n=n()) |>
  arrange(desc(n)) |>
  mutate(word = reorder(word, n)) |>
  top_n(50) |>    
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

# STOP HERE - check words - correct stopwords - continue only if happy

# DFM-ing it (termdoc)
tidy_dfm <- tidy_data_pruned |>
  count(index, word) |>
  cast_dfm(index, word, n)

# feed to lda object

full_data<- convert(tidy_dfm, to = "topicmodels")

rm(tidy_data)
rm(good_common_words)
rm(tidy_data_pruned)
rm(tidy_dfm)


### SECTION: LDA Search K -----

closeAllConnections() 

mycores <- detectCores()-10 # if RAM issues - reduce cores even more

## Add your params
candidate_alpha = c(50,25,10,5,2,1) # candidates for alpha values
#candidate_k = c(2, seq(5,100,by=5),seq(110,150,by=10),175,200) # candidates for how many topics
candidate_k = c(2, seq(5,100,by=5)) # candidates for how many topics

folds = 5
n = nrow(full_data)


## Run the search

# Create the splitfolds outside the loop
splitfolds = sample(1:folds, n, replace = TRUE)
# Set up the DF that will be the batch queue
validationQueueDF = data.frame(k = c(1), alpha = c(1), fold = c(1))
validationQueueDF = validationQueueDF[-1, ]
# Create all the rows for the batch queue. Alpha * K * folds
for (val_alpha in candidate_alpha) {
  for (val_k in candidate_k) {
    for (val_i in seq(from = 1, to = folds, by = 1)) {
      val_model = c(val_k, val_alpha/val_k, val_i)
      validationQueueDF[nrow(validationQueueDF) + 1, ] = val_model
    }
  }
}
# Reorganize batch queue in descending order of K
validationQueueDF = validationQueueDF[order(-validationQueueDF$k), ]
# Put the loop in a function so that it was easier to execute and debug in RStudio
validation=function() {
  print(Sys.time())
  # This console message confirms the number of items in the batch queue.
  print(paste("Starting validation of ",as.character(nrow(validationQueueDF))," models!",sep = ""))
  # Adding outfile="" to makeCluster allows print messages in the loop to go to the master console instead of being hidden.
  cluster = makeCluster(mycores, outfile="") # We are leaving one Core spare. If number of corse on pc is 1, then -1 in this line should be removed.
  registerDoParallel(cluster)
  clusterEvalQ(cluster, {
    library(topicmodels)
  })
  # Sending the batch queue, the splitfolds, and the dataset to each cluster.
  clusterExport(cluster, c("full_data", "validationQueueDF", "splitfolds"))
  results = foreach(j = 1:nrow(validationQueueDF), .combine = rbind) %dopar%{
    print(j)
    print(nrow(validationQueueDF))
    print(paste0("Running model ", j, "/", nrow(validationQueueDF), 
                 " — ", nrow(validationQueueDF) - j, " left"))
    # Pull the row in the batch queue for this loop and assign the items to variables.
    model = as.numeric(as.vector(validationQueueDF[j,]))
    model_k = model[1]
    model_alpha = model[2]
    model_fold = model[3]
    # Print confirmation that the job has started.
    print(paste("Starting fit of Row ",as.character(j),". K, Alpha, Fold: ",as.character(model_k),", ",as.character(model_alpha),", ",as.character(model_fold),".",sep = ""))
    train_set = full_data[splitfolds != model_fold, ]
    valid_set = full_data[splitfolds == model_fold, ]
    fitted = LDA(train_set, k = model_k, method = "Gibbs", control = list(alpha = model_alpha))
    # Create the one row DF to be rbinded (rbound?) together by foreach once all the jobs are run.
    result_1model = data.frame(k = c(1), alpha = c(1), fold = c(1), perplexity = c(1))
    # Next four lines are kind of sad. I'm sure there's a more efficient way to replace a row in a DF but it's late and this works!
    result_1model[1,1] = model_k
    result_1model[1,2] = model_alpha
    result_1model[1,3] = model_fold
    result_1model[1,4] = perplexity(fitted,newdata = valid_set)
    # Confirm the job has finished.
    print(paste("Model fitting of Row ",as.character(j)," complete. K, Alpha, Fold: ",as.character(model_k),", ",as.character(model_alpha),", ",as.character(model_fold),".",sep = ""))
    return(result_1model)
    print(Sys.time())
    print (validationQueueDF[j,])
  }
  stopCluster(cluster)
  print("Done!")
  print(Sys.time())
  return(results)
}

# Execute the above function and end up with a four column DF: K, Alpha, Fold, and Perplexity.

save.image(paste0(Data_folder, Project_name,"_pre-searchk.rdata"))

print(Sys.time())
val_results=validation()
print(Sys.time())

save.image(paste0(Data_folder, Project_name,"_post-searchk.rdata"))

closeAllConnections() 

#### Check your results ####

MainresultDF<-val_results

MainresultDF$kalpha <- paste0(as.character(MainresultDF$k),MainresultDF$alpha) 
MainresultDF$newalpha <- as.numeric(MainresultDF$alpha*MainresultDF$k) 

ggplot(MainresultDF) +geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=alpha))

ggplot(MainresultDF) +geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=factor(newalpha)))+
  ggtitle("Twitter Users 5-fold cross-validation of topic modelling",
          "(ie five different models fit for each candidate number of topics)") +
  labs(x = "Candidate number of topics", y = "Perplexity when fitting the trained model to the hold-out set")

MainDF <- MainresultDF[MainresultDF$newalpha ==2, ] #correct alpha 

MainresultDF<-MainresultDF[order(MainresultDF$k),]

cars.spl <- with(MainresultDF, smooth.spline(k, perplexity, df = 3))
with(cars, predict(cars.spl, x = MainresultDF$k, deriv = 2))

plot(with(cars, predict(cars.spl, x = MainresultDF$k, deriv = 2)), type = "l")

# play with abline to find the point AFTER max point
abline(v=30)

save.image(paste0(Data_folder, Project_name,"_post-searchk.rdata"))

### SECTION: LDA Find K -----
myalpha=2/30 #enter alpha level divided by a good k

# Running the searchK command ################################################################## RUN LATER
Sys.time()
FTN_result <- FindTopicsNumber(
  full_data,
  topics = c(2, seq(5,100,by=5)), # Specify how many topics you want to try.
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(alpha=myalpha, seed = 6723), # add random seed for reproducability
  mc.cores = 4L, # Specify the number of cores that you computer has to achieve the best performance. 
  verbose = TRUE
)
Sys.time()

FindTopicsNumber_plot(FTN_result)  

ggsave(paste0(Output_folder, Project_name,"FindK_Results.jpeg"), bg="white", width=12, height=8, dpi=300)

save.image(paste0(Data_folder, Project_name, "_post-findk.rdata"))

rm(FTN_result, val_results, validationQueueDF, candidate_alpha, candidate_k, folds, 
   val_i, val_k, val_model, validation, splitfolds, val_alpha, mycores, myalpha, n)

### SECTION: LDA ----

runsdf<-data.frame(myk=c(15,50,60))
myalpha=2
mymodels<-list()

cluster <- makeCluster(detectCores(logical = TRUE) - 6) # leave one CPU spare...
registerDoParallel(cluster)

clusterEvalQ(cluster, {
  library(topicmodels)
})

clusterExport(cluster, c("full_data","runsdf","myalpha"))

system.time({
  mymodels <- foreach(j = 1:nrow(runsdf)) %dopar%{
    k_run <- runsdf[j,1]
    #alpha_run<-runsdf[j,2]
    fitted <- LDA(full_data, k = k_run, method = "Gibbs",
                  control = list(alpha=myalpha/k_run,seed=527) )
  }
})
stopCluster(cluster)

LDAlist=mymodels

closeAllConnections() 

save.image(paste0(Data_folder, Project_name,"_post-models.rdata"))

#### SECTION: Analysis Part 2: Making excels ----
LDAfit<-LDAlist[[2]] # or whatever model you want to inspect
nwords <-30 # number of words/texts you want
ntext <-30 # number of texts you want
mydata<-data3 # change if you used for your corpus some other data like data3, trump or X
nimage<-40 #### NOTE: If changed then also mfraw in images print needs to change to accomodate

get_top_Words<-function(LDAmodel) {
  
  ## extract the beta (termXtopic) matrix
  mybeta<-data.frame(LDAmodel@beta)
  ## input the words
  colnames(mybeta)<-LDAmodel@terms
  ## transpose to see it more comfortably
  mybeta<-t(mybeta)
  ## give colnames
  colnames(mybeta)<-seq(1:ncol(mybeta))
  ## exponenciate the values back as software logs them
  mybeta=exp(mybeta)
  ## Convert to dataframe 
  mybeta<-data.frame(mybeta)
  # create a container (just a table with ncols being topics, and nrows being nwords)
  topwords <- mybeta[1:nwords,]
  
  ## lopping over all topics
  for (i in 1:LDAmodel@k) {
    print(i)
    ## grab beta, select the tight column, and choose top_n based on nwords
    tempframe <- mybeta |> select(i) |> arrange(desc(1)) |> top_n(nwords)
    ## now that the order is correct - grab and vectorize words (rownames)
    tempvec<-as.vector(rownames(tempframe))
    ## plug in to the i'th column (1 for topic 1, 2 for 2 etc)
    topwords[,i]<-tempvec[1:nwords]
  }
  
  # add row names to look pretty
  rownames(topwords)<-c(1:nwords)
  topwords <- as.data.frame(topwords)
  # write to excel
  openxlsx::write.xlsx(topwords,paste0(Output_folder, Project_name,"_Topwordsk_",LDAmodel@k,".xlsx"))
  return(topwords)
}

get_FREX_Words<-function(LDAmodel) {
  ## apply formula below from Roberts et al.
  ## 1/(w/(bword/sumbrow)+(1-w)/(bword)) for each cell
  
  mybeta<-data.frame(LDAmodel@beta)
  colnames(mybeta)<-LDAmodel@terms
  mybeta<-t(mybeta)
  colnames(mybeta)<-seq(1:ncol(mybeta))
  mybeta=exp(mybeta)
  
  # apply formula below
  # 1/(w/(bword/sumbrow)+(1-w)/(bword)) for each cell
  myw=0.3
  word_beta_sums<-rowSums(mybeta)
  my_beta_for_frex<-mybeta
  for (m in 1:ncol(my_beta_for_frex)) {
    for (n in 1:nrow(my_beta_for_frex)) {
      my_beta_for_frex[n,m]<-1/(myw/(my_beta_for_frex[n,m]/word_beta_sums[n])+((1-myw)/my_beta_for_frex[n,m]))
    }
    print (m)
  }
  
  ########  print 50 frex:
  topwords <- my_beta_for_frex[1:nwords,]
  
  for (i in 1:LDAmodel@k) {
    tempframe <- my_beta_for_frex[order(-my_beta_for_frex[,i]),]
    tempframe <- tempframe[1:nwords,]
    tempvec<-as.vector(rownames(tempframe))
    topwords[,i]<-tempvec[1:nwords]
  }
  
  rownames(topwords)<-c(1:nwords)
  topFREX <- as.data.frame(topwords)
  
  openxlsx::write.xlsx(topFREX, paste0(Output_folder, Project_name,"_TopFREXk_",LDAmodel@k,".xlsx"))
  return(topwords)
  
}

get_top_texts<-function(LDAmodel) {
  mydata$index <- as.character(mydata$index)
  
  # Get gamma matrix in tidy format
  tidy_gamma <- tidy(LDAmodel, matrix = "gamma")
  tidy_gamma$document <- as.character(tidy_gamma$document)
  
  # Join to get the corresponding text from mydata
  gamma_with_text <- tidy_gamma |>
    left_join(mydata |> select(index, text), by = c("document" = "index"))
  
  # For each topic, get top texts by gamma
  top_texts_long <- gamma_with_text |>
    group_by(topic) |>
    slice_max(gamma, n = ntext, with_ties = FALSE) |>
    arrange(topic, -gamma) |>
    mutate(rank = row_number()) |>
    select(topic, rank, text) |>
    ungroup()
  
  # Convert to wide format: columns = topics, rows = top texts
  top_texts_wide <- top_texts_long |>
    pivot_wider(names_from = topic, values_from = text) |>
    arrange(rank) |>
    select(-rank)
  
  openxlsx::write.xlsx(top_texts_wide, paste0(Output_folder, Project_name, "_TopTEXTSk_", LDAmodel@k, ".xlsx"))
  
  return(top_texts_wide)
}

get_top_images<-function(LDAmodel) {
  
  print("Starting images:")
  print(LDAmodel@k)
  
  # Ensure mydata$index is character and has image_path
  mydata$index <- as.character(mydata$index)
  print("Converted index to character")
  
  # Extract tidy gamma matrix
  tidy_gamma <- tidy(LDAmodel, matrix = "gamma")
  tidy_gamma$document <- as.character(tidy_gamma$document)
  print("Extracted tidy gamma")
  
  print("Gamma sample:")
  print(head(tidy_gamma))
  
  # Join to get image path
  gamma_with_paths <- tidy_gamma |>
    left_join(mydata |> select(index, image_path), by = c("document" = "index"))
  print("Join completed")
  
  print("Checking for valid image paths:")
  print(sum(!is.na(gamma_with_paths$image_path)))
  
  # Get top image paths for each topic
  top_images_long <- gamma_with_paths |> 
    group_by(topic) |>
    slice_max(gamma, n = nimage, with_ties = FALSE) |>
    arrange(topic, -gamma) |>
    mutate(rank = row_number()) |>
    select(topic, rank, image_path) |>
    ungroup()
  
  print("Top images extracted")
  
  
  # Pivot to wide format *after* sorting
  toppaths <- top_images_long |>
    select(topic, rank, image_path) |>
    pivot_wider(names_from = topic, values_from = image_path) |>
    arrange(rank) |>
    select(-rank)
  
  print("Toppaths built:")
  print(dim(toppaths))
  print("Column names:")
  print(colnames(toppaths))
  
  # Write Excel file of image paths
  openxlsx::write.xlsx(toppaths, paste0(Output_folder, Project_name, "_TopPATHSk_", LDAmodel@k, ".xlsx"))
  
  print("Starting saving images")
  
  ## row names to make pretty
  dirname<-paste0("TOPIC_IMAGES_K",LDAmodel@k)
  try(dir.create(dirname))
  
  for(topicnum in 1:ncol(toppaths)) {
    print(topicnum)
    
    imagelist<-list()
    
    # paths<-toppaths[,topicnum]
    paths <- as.character(toppaths[[topicnum]])
    
    
    # paths<-paste0("../",paths)
    paths <- paths
    
    for (i in 1:length(paths)) {
      imagelist[[i]]<-image_read(paths[i])
    }
    
    add0 <- function(input){
      ifelse(input < 10, paste0("0", input), input)
    }
    
    png(filename=paste0(dirname,"/Topic_",add0(topicnum),".png"), width = 3000, height = 2400, units = "px")
    
    par(mfrow = c(9, 5), mar = c(1, 1, 1, 1))
    
    for (j in imagelist) {plot(j)}
    
    topfrextoprint<-topFREX[,topicnum]
    
    #mtext(paste0("Topic #: ",topicnum), side = 3, line = - 4, outer=TRUE,cex=4)
    
    topfrextoprint<-paste(topfrextoprint,collapse=", ")
    
    topfrextoprint<-paste0("Topic ",topicnum,": ",topfrextoprint)
    
    topfrextoprint<-str_wrap(topfrextoprint, width = 80)
    
    mtext(topfrextoprint, side = 1, line = -2, outer=TRUE,cex=5)
    
    dev.off()
  }
  print("Getting PDF")
  
  allimagesforpdf<-list.files(dirname,full.names = T)
  
  image_to_pdf(allimagesforpdf,paste0(Project_name,"_Topimages_",LDAmodel@k,".pdf"))
  
  # dev.off()
  return(toppaths)
}


nwords <-30 # number of words/texts you want
ntext <-30 # number of texts you want
mydata<-data3 # change if you used for your corpus some other data like data3, trump or X
nimage<-40 #### NOTE: If changed then also mfraw in images print needs to change to accomodate
LDAfit <- LDAlist[[3]]

LDAfit

topwords<-get_top_Words(LDAfit)
topFREX<-get_FREX_Words(LDAfit)
toptexts<-get_top_texts(LDAfit)
topimages<-get_top_images(LDAfit)


for (eachmod in 1:length(LDAlist)) {
  LDAfit<-LDAlist[[eachmod]]
  print("getting words")
  topwords<-get_top_Words(LDAfit)
  print("getting FREX")
  topFREX<-get_FREX_Words(LDAfit)
  print("getting texts")
  toptexts<-get_top_texts(LDAfit)
  print("getting images")
  topimages<-get_top_images(LDAfit) ## make sure we ran FREX first (@TO DO: add the whole frex part to the images so we can just use the function alone)
}


save.image(paste0(Project_name,"_post-excel.rdata"))

### SECTION: Validation ----
# PARAMETERS ------------------------------------------------
LDAmodel<-LDAlist[[2]]
LDAmodel
nimage=20
mydata = data3

pathcolumn=which(colnames(mydata)=="image_path")

print("Starting images:")
print(LDAmodel@k)

mydata$index <- as.character(mydata$index)
tidy_gamma <- tidy(LDAmodel, matrix = "gamma")
tidy_gamma$document <- as.character(tidy_gamma$document)
gamma_with_paths <- tidy_gamma |>
  left_join(mydata |> select(index, image_path), by = c("document" = "index"))
print(sum(is.na(gamma_with_paths$image_path)))

top_images_long <- gamma_with_paths |> 
  group_by(topic) |>
  slice_max(gamma, n = nimage, with_ties = FALSE) |>
  arrange(topic, -gamma) |>
  mutate(rank = row_number()) |>
  select(topic, rank, image_path) |>
  ungroup()

toppaths <- top_images_long |>
  select(topic, rank, image_path) |>
  pivot_wider(names_from = topic, values_from = image_path) |>
  arrange(rank) |>
  select(-rank)
print(dim(toppaths))
print(colnames(toppaths))


add0 <- function(input){
  ifelse(input < 100 & input > 9, paste0("0", input), 
         ifelse(input < 10, paste0("00", input), input))
}


############################################################
# IMAGE INTRUSION TASK
# One in 6 images is from a different topic
############################################################

try(dir.create("VALIDATION_IMAGE_INT"))
try(dir.create("VALIDATION_IMAGE_INT/RAWIMAGES"))

dirname<-"VALIDATION_IMAGE_INT/RAWIMAGES/"

topiclist=seq(1:LDAmodel@k)

log_of_answers <- data.frame(image_num = character(),
                             topicnum = integer(),
                             rightanswer = integer(),
                             stringsAsFactors = FALSE)

# Setup output folder
dirname <- "VALIDATION_IMAGE_INT"
if (!dir.exists(dirname)) dir.create(dirname)

# Initialize answer log
log_of_answers <- data.frame(image_num = character(),
                             topicnum = integer(),
                             rightanswer = integer(),
                             stringsAsFactors = FALSE)

# Setup output folder
dirname <- "VALIDATION_IMAGE_INT"
if (!dir.exists(dirname)) dir.create(dirname)

# Initialize answer log
log_of_answers <- data.frame(image_num = character(),
                             topicnum = integer(),
                             rightanswer = integer(),
                             stringsAsFactors = FALSE)

# Loop over trials
for (trialnum in 1:105) {
  cat("\n======== Trial", trialnum, "========\n")
  
  myloopnum <- trialnum
  origseed <- 3947 + myloopnum
  set.seed(origseed)
  
  # Refresh topiclist if needed
  if (!exists("topiclist") || length(topiclist) == 0) {
    topiclist <- seq_len(LDAmodel@k)
  }
  
  # Pick target topic
  valtopicnum <- sample(topiclist, 1)
  topiclist <- setdiff(topiclist, valtopicnum)
  cat("Selected topic:", valtopicnum, "\n")
  
  # xtract usable images from the topic
  pics_in_topic <- toppaths[[valtopicnum]]
  pics_in_topic <- pics_in_topic[file.exists(pics_in_topic)]
  
  if (length(pics_in_topic) < 6) {
    warning(paste("Topic", valtopicnum, "has fewer than 6 valid images — skipping trial"))
    next
  }
  
  goodpics <- sample(pics_in_topic, 6)
  
  # elect BAD (intrusive) topic
  valid_other_topics <- setdiff(seq_len(LDAmodel@k), valtopicnum)
  if (length(valid_other_topics) == 0) {
    warning(paste("No valid BAD topics for trial", myloopnum, "- skipping"))
    next
  }
  
  valtopicnumBAD <- sample(valid_other_topics, 1)
  badpics <- toppaths[[valtopicnumBAD]]
  badpics <- badpics[file.exists(badpics)]
  
  if (length(badpics) < 1) {
    warning(paste("BAD topic", valtopicnumBAD, "has no valid images — skipping trial"))
    next
  }
  
  badpic <- sample(badpics, 1)
  
  # Choose position for BAD image
  rightanswer <- sample(1:6, 1)
  cat("Inserting BAD image in position", rightanswer, "from topic", valtopicnumBAD, "\n")
  
  # Insert intrusive image
  goodpics[rightanswer] <- badpic
  
  # ecord test file name
  filenametest <- paste0("TEST_", add0(myloopnum), ".png")
  
  # og the answer
  log_of_answers[myloopnum, ] <- c(filenametest, valtopicnum, rightanswer)
  
  # Load and plot the 6 images
  testimagelist <- list()
  valid_images <- TRUE
  
  for (i in seq_along(goodpics)) {
    if (!file.exists(goodpics[i])) {
      warning(paste("Image missing:", goodpics[i]))
      valid_images <- FALSE
      break
    }
    testimagelist[[i]] <- image_read(goodpics[i])
  }
  
  if (!valid_images) next  # Skip saving if any images are missing
  
  # save the composite image
  png(filename = file.path(dirname, filenametest),
      width = 1000, height = 800, units = "px")
  par(mfrow = c(3, 2), mar = c(5, 1, 1, 1))
  try(for (img in testimagelist) {
    plot(img)
  })
  mtext(paste0("TEST NUM: ", add0(myloopnum)),
        side = 1, line = -2, outer = TRUE, cex = 3)
  dev.off()
}



# print the answer sheet
# Save answer key and blank version
write.csv(log_of_answers, file.path(dirname, "log_of_answers_Image_Int.csv"), row.names = FALSE)

log_of_answers_to_fill <- log_of_answers[, -2]  # remove topicnum
log_of_answers_to_fill[, 2] <- 0  # zero out answer column
write.csv(log_of_answers_to_fill, file.path(dirname, "log_of_answers_Image_Int_TOFILL.csv"), row.names = FALSE)

# Create PDF from .png test images
allimagesforpdf <- list.files(dirname, pattern = "\\.png$", full.names = TRUE)
image_to_pdf(allimagesforpdf, file.path(dirname, "TEST_Image_Int.pdf"))

print("Getting PDF")

try(dev.off())

############################################################
# TOPIC INTRUSION TASK
# One in 4 topic sets is correct for the given test image
############################################################

# Setup folders
dir.create("VALIDATION_WHICH_TOPIC", showWarnings = FALSE)
dir.create("VALIDATION_WHICH_TOPIC/RAWIMAGES", showWarnings = FALSE)

dirname <- "VALIDATION_WHICH_TOPIC/RAWIMAGES/"

# Initialize topic list and answer log
topiclist <- seq(1:LDAmodel@k)

log_of_answers <- data.frame(image_num = character(),
                             topicnum = integer(),
                             rightanswer = integer(),
                             stringsAsFactors = FALSE)

# Loop over test items
for (trialnum in 1:105) {
  cat("\n======== Trial", trialnum, "========\n")
  
  myloopnum <- trialnum
  set.seed(7344 + myloopnum)
  
  # Select target topic
  if (length(topiclist) == 0) {
    topiclist <- seq(1:LDAmodel@k)
  }
  valtopicnum <- sample(topiclist, 1)
  topiclist <- setdiff(topiclist, valtopicnum)
  
  # Get 5 images from the correct topic
  goodpics_all <- toppaths[[valtopicnum]]
  goodpics_all <- goodpics_all[file.exists(goodpics_all)]
  
  if (length(goodpics_all) < 5) {
    warning(paste("Skipping trial", trialnum, "- topic", valtopicnum, "has fewer than 5 valid images"))
    next
  }
  
  goodpics <- sample(goodpics_all, 5)
  testpic <- goodpics[5]
  goodpics <- goodpics[1:4]
  
  # Select 4 non-target topics
  valtopicnumBAD <- sample(setdiff(1:LDAmodel@k, valtopicnum), 4)
  
  # Get 4×3 images from the bad topics
  badpiclist <- list()
  for (i in 1:4) {
    badpics <- toppaths[[valtopicnumBAD[i]]]
    badpics <- badpics[file.exists(badpics)]
    if (length(badpics) < 4) {
      warning(paste("Skipping trial", trialnum, "- BAD topic", valtopicnumBAD[i], "has fewer than 4 valid images"))
      next
    }
    badpiclist[[i]] <- sample(badpics, 4)
  }
  
  # Randomly place the correct topic group
  rightanswer <- sample(1:4, 1)
  badpiclist[[rightanswer]] <- goodpics
  
  # Build final image list: 4×4 (topics) + 1 (test image)
  imagestoprint <- c(unlist(badpiclist), testpic)
  testimagelist <- list()
  valid_images <- TRUE
  
  for (i in seq_along(imagestoprint)) {
    if (!file.exists(imagestoprint[i])) {
      warning(paste("Missing file:", imagestoprint[i]))
      valid_images <- FALSE
      break
    }
    testimagelist[[i]] <- image_read(imagestoprint[i])
  }
  
  if (!valid_images) next
  
  # Output composite image
  filenametest <- paste0("TEST_", add0(myloopnum), ".png")
  log_of_answers[myloopnum, ] <- c(filenametest, valtopicnum, rightanswer)
  
  png(filename = file.path(dirname, filenametest), width = 1000, height = 800, units = "px")
  par(mfrow = c(5, 4), mar = c(5, 1, 1, 1))
  try(for (j in testimagelist) plot(j))
  mtext(paste0("TEST NUM: ", add0(myloopnum)), side = 1, line = -2, outer = TRUE, cex = 3)
  dev.off()
}

# Save logs
write.csv(log_of_answers, "VALIDATION_WHICH_TOPIC/log_of_answers_WhichTopic.csv", row.names = FALSE)
log_of_answers_to_fill <- log_of_answers[, -2]
log_of_answers_to_fill[, 2] <- 0
write.csv(log_of_answers_to_fill, "VALIDATION_WHICH_TOPIC/log_of_answers_WhichTopic_TOFILL.csv", row.names = FALSE)

# Build final PDF
allimagesforpdf <- list.files(dirname, pattern = "\\.png$", full.names = TRUE)
image_to_pdf(allimagesforpdf, "VALIDATION_WHICH_TOPIC/TEST_WhichTopic.pdf")

try(dev.off())

# we need to read all files in folder and generate a pdf for the students
save.image(paste0(Data_folder, Project_name,  "_post-validation.rdata"))


############################################################
# INTER-CODER RELIABILITY CALCULATION
# Calculates % Agreement, Krippendorff's Alpha, and BP coefficient
############################################################

# Load coder responses
# coder1: first coder's responses
# coder2: second coder's responses
coder1 <- tovalidate[c(1, 3)]
coder2 <- tovalidate[c(1, 4)]

# Create results dataframe
RESULTS <- data.frame(
  item = colnames(coder1)[2:ncol(coder1)],
  PERC = c(0),
  KRIP = c(0),
  BP = c(0),
  stringsAsFactors = FALSE
)

############################################################
# 1. Percent Agreement
############################################################
for (i in 1:nrow(RESULTS)) {
  print(i)
  RESULTS[i, 2] <- sum(coder1[, i + 1] == coder2[, i + 1]) / nrow(coder1)
}

############################################################
# 2. Krippendorff's Alpha (Ordinal)
############################################################
for (i in 1:nrow(RESULTS)) {
  newdf <- data.frame(coder1[, i + 1], coder2[, i + 1])
  newdf <- t(newdf)
  RESULTS[i, 3] <- as.numeric(
    kripp.alpha(newdf, method = c("ordinal"))[[5]]
  )
}

############################################################
# 3. Brennan–Prediger Coefficient (Unweighted)
############################################################
for (i in 1:nrow(RESULTS)) {
  newdf <- data.frame(coder1[, i + 1], coder2[, i + 1])
  newdf <- t(newdf)
  temp <- bp.coeff.raw(
    t(newdf),
    weights = "unweighted",
    conflev = 0.95,
    N = Inf,
    print = TRUE
  )
  RESULTS[i, 4] <- as.numeric(temp[[3]])
}

############################################################
# Save results
############################################################
print(RESULTS)
write.csv(RESULTS, "results_validation.csv")


