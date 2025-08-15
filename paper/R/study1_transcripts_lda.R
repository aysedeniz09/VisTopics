# === Project Setup ===
Data_folder = "DATA/" 
Output_folder = "OUTPUT/"
Project_name = "NBC_Transcripts_"


### File I/O and Data Handling
library(readr)      # Reading delimited text files (CSV, TSV, etc.)
library(foreign)    # Import/export of data files from other stats packages (e.g., SPSS, Stata)
# library(xlsx)     # Reading/writing Excel files — may be problematic on macOS
library(openxlsx)

### Text and Language Processing
library(quanteda)   # Text analysis and corpus management
library(tidytext)   # Tokenization and dictionary-based text processing
library(cld2)       # Language detection
library(stringi)    # String processing (fast and Unicode-aware)
library(stringr)    # String manipulation (part of the tidyverse)
library(rvest)      # Web scraping and HTML parsing

### Topic Modeling and NLP
library(topicmodels)  # Latent Dirichlet Allocation (LDA) and other topic models
library(ldatuning)    # Determining the optimal number of topics (K)
library(RNewsflow)    # NLP utilities for temporal and textual similarity

### Data Manipulation and Visualization
library(tidyverse)  # Core tidy tools: ggplot2, dplyr, tidyr, etc.
library(dplyr)      # Data manipulation (also part of tidyverse)
library(ggplot2)    # Plotting system based on the Grammar of Graphics
library(scales)     # Formatting and scaling axes in ggplot2
library(ggthemes)   # Additional themes and styles for ggplot2

### Date and Time Handling
library(lubridate)  # Simplifies date and time manipulation

### Parallel and Efficient Computing
library(doParallel) # Parallel processing support

### Networks and Graphs
library(igraph)     # Network analysis and graph theory tools

### SECTION: Load Data ------

# Download data from OSF
download.file("https://osf.io/7ehqy/download", 
              destfile = "video_transcripts.csv", 
              mode = "wb")

data <- readr::read_csv("video_transcripts.csv")
data$text<-data$transcript                                
data$index <- seq(1:nrow(data))

## clean for specific language
data2<-data
data2$CLD2<-cld2::detect_language(data2$text)
data2<-data2 |>  filter(CLD2=="en")

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

### SECTION: Pre-LDA Tidy ----

mystopwords<-c(stopwords("en"),stopwords(source = "smart"))

# creating tidy token
tidy_data<-data3 |> 
  unnest_tokens(word, text) |>  # tokenizing
  # option 1 REGEX --># Be careful this is deangerous as it gets word parts
  #filter(!grepl(paste(mystopwords, collapse="|"), word)) |>   
  # option 2 Exact Match -->
  anti_join(data.frame(word=mystopwords)) |> 
  mutate(nchar=nchar(word)) |>  #counting the number of letters
  filter(nchar>2) |>  # remove from minumum number of chars
  filter(!grepl("[0-9]{1}", word)) |>  # removing numbers 
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
tidy_data_pruned<-tidy_data |>  inner_join(good_common_words)


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

mycores <- detectCores()-6 # if RAM issues - reduce cores even more


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
abline(v=25)

### SECTION: LDA Find K -----
myalpha=1/25 #enter alpha level divided by a good k

# Running the searchK command ################################################################## RUN LATER
Sys.time()
FTN_result <- FindTopicsNumber(
  full_data,
  topics = c(2, seq(5,100,by=5)), # Specify how many topics you want to try.
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(alpha=myalpha, seed = 6723), # add random seed for reproducability
  mc.cores = mycores, # Specify the number of cores that you computer has to achieve the best performance. 
  verbose = TRUE
)
Sys.time()

FindTopicsNumber_plot(FTN_result)  

ggsave(paste0(Output_folder, Project_name,"FindK_Results.jpeg"), bg="white", width=12, height=8, dpi=300)

save.image(paste0(Data_folder, Project_name, "_post-findk.rdata"))

rm(FTN_result, val_results, validationQueueDF, candidate_alpha, candidate_k, folds, 
   val_i, val_k, val_model, validation, splitfolds, val_alpha, mycores, myalpha, n)

### SECTION: LDA ----
runsdf<-data.frame(myk=c(15,30,45))
myalpha=1
mymodels<-list()

cluster <- makeCluster(detectCores(logical = TRUE) - 6) # leave one CPU spare...
registerDoParallel(cluster)

clusterEvalQ(cluster, {
  library(topicmodels)
})

#clusterExport(cluster, c("full_data", "burnin", "iter", "keep", "splitfolds", "folds", "candidate_k"))
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


nwords <-30 # number of words/texts you want
ntext <-30 # number of texts you want
mydata<-data3 # change if you used for your corpus some other data like data3, trump or X
LDAfit <- LDAlist[[1]]

# LDAfit
# 
# topwords<-get_top_Words(LDAfit)
# topFREX<-get_FREX_Words(LDAfit)
# toptexts<-get_top_texts(LDAfit)
# topimages<-get_top_images(LDAfit)


for (eachmod in 1:length(LDAlist)) {
  LDAfit<-LDAlist[[eachmod]]
  print("getting words")
  topwords<-get_top_Words(LDAfit)
  print("getting FREX")
  topFREX<-get_FREX_Words(LDAfit)
  print("getting texts")
  toptexts<-get_top_texts(LDAfit)
}


save.image(paste0(Data_folder, Project_name,"_post-excels.rdata"))