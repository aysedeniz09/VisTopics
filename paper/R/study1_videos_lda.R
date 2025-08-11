# === Project Setup ===
Data_folder <- "DATA/" 
Output_folder <- "OUTPUT/"
Project_name <- "NBCtop452_Final_July_"

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


### SECTION: Load Data ------

imported<-read.csv("nbc_captions.csv")                           
# assign data
data<-imported                                     

# assign index
data$index<-(1:nrow(data))

## convert the content to text column
data$text<-data$caption                                

## clean for specific language
data2<-data
data2<-data2[data2$caption!="Failed to get caption",]

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

save.image(paste0(Data_folder, Project_name, "_prelda-clean-data.Rdata"))

rm(list=setdiff(ls(), c("Data_folder", "Output_folder", 
                        "Project_name", "data3",
                        "dataALL")))

### SECTION: Pre-LDA Tidy ----

mystopwords<-c(stopwords("en"),stopwords(source = "smart"),"http","https","image","picture",
               "colors", "red", "blue", "green", "yellow", "orange", "purple", "pink", "brown", 
               "black", "white", "gray", "cyan", "magenta", "maroon", "lime", "navy", "teal", "silver", 
               "dark","background", "blurry","blurred","visible","fuzzy", "reads", "overlay", "caption", "subtitles", 
               "reads","read","reading", "overlaid", "letters", "shapes","foreground",
               "top","bottom","front","back","left","right","center","side",
               "text","logo",
               "nbc","news","present","banner","breaking","headline","studio","ticker") 


# creating tidy tokens
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

mycores <- detectCores()-10 # if RAM issues - reduce cores even more

mycores <- 18

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

save.image(paste0(Data_folder, Project_name, "_post-tidy-presearchk.rdata"))

print(Sys.time())
val_results=validation()
print(Sys.time())

save.image(paste0(Data_folder, Project_name, "_post-tidy-postsearchk.rdata"))

closeAllConnections() 


MainresultDF<-val_results

MainresultDF$kalpha <- paste0(as.character(MainresultDF$k),MainresultDF$alpha) 
MainresultDF$newalpha <- as.numeric(MainresultDF$alpha*MainresultDF$k) 

ggplot(MainresultDF) +geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=alpha))

ggplot(MainresultDF) +geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=factor(newalpha)))+
  ggtitle("5-fold cross-validation of topic modelling",
          "(ie five different models fit for each candidate number of topics)") +
  labs(x = "Candidate number of topics", y = "Perplexity when fitting the trained model to the hold-out set")

MainDF <- MainresultDF[MainresultDF$newalpha ==2, ] #correct alpha 

MainresultDF<-MainresultDF[order(MainresultDF$k),]

cars.spl <- with(MainresultDF, smooth.spline(k, perplexity, df = 3))
with(cars, predict(cars.spl, x = MainresultDF$k, deriv = 2))

plot(with(cars, predict(cars.spl, x = MainresultDF$k, deriv = 2)), type = "l")

# play with abline to find the point AFTER max point
abline(v=35)

### SECTION: LDA Find K -----

myalpha=2/35 #enter alpha level divided by a good k

# Running the searchK command ################################################################## RUN LATER
Sys.time()
FTN_result <- FindTopicsNumber(
  full_data,
  topics = c(2, seq(5,100,by=5)), # Specify how many topics you want to try.
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(alpha=myalpha, seed = 6723), # add random seed for reproducability
  mc.cores = 18L, # Specify the number of cores that you computer has to achieve the best performance. 
  verbose = TRUE
)
Sys.time()

FindTopicsNumber_plot(FTN_result)  

ggsave(paste0(Output_folder, Project_name,"FindK_Results.jpeg"), bg="white", width=12, height=8, dpi=300)

save.image(paste0(Data_folder, Project_name, "_post-findk.rdata"))

rm(FTN_result, val_results, validationQueueDF, candidate_alpha, candidate_k, folds, 
   val_i, val_k, val_model, validation, splitfolds, val_alpha, mycores, myalpha, n)

### SECTION: LDA ----

runsdf<-data.frame(myk=c(35,15,50))
myalpha=2
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

save.image(paste0(Data_folder, Project_name, "_post-models.rdata"))

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
nimage<-40 #### NOTE: If changed then also mfraw in images print needs to change to accomodate
mydata<-data3 # change if you used for your corpus some other data like data3, trump or X
LDAfit <- LDAlist[[2]]

LDAfit

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
  print("getting images")
  topimages<-get_top_images(LDAfit) ## make sure we ran FREX first (@TO DO: add the whole frex part to the images so we can just use the function alone)
}
save.image(paste0(Data_folder, Project_name,"_post-excel.rdata"))

### SECTION: Meta Theta DF ----
LDAfit <- LDAlist[[1]]
LDAfit #35 topics

data33 <- data3

missing_docs<-setdiff(as.character(data33$index),as.character(LDAfit@documents))

## now we can add together the gamma and data (notice we only add TEXt from original data)
TEMP_gamma<-(cbind.data.frame(LDAfit@documents,LDAfit@gamma))

colnames(TEMP_gamma)[1]<-"index"

TEMP_gamma$index <- as.numeric(TEMP_gamma$index)
meta_theta_df <- data33 |>  left_join((TEMP_gamma))
#meta_theta_df<-meta_theta_df[,-c(which(colnames(meta_theta_df)=="index"))]
rm(TEMP_gamma, data33, missing_docs)

### SECTION: Add deleted frames -----

frame_df <- read_csv("frames_combined.csv")


frame_df <- frame_df |> 
  mutate(
    original_path  = gsub("^.*?(IMAGES/.*)$", "\\1", ORIGINAL),
    duplicate_path = gsub("^.*?(IMAGES/.*)$", "\\1", DUPLICATES)
  ) |> 
  distinct()


orig_rows <- frame_df |> 
  inner_join(meta_theta_df, by = c("original_path" = "image_path"))


dup_rows <- orig_rows |> 
  mutate(
    image_path = duplicate_path,
    deleted    = "deleted",
    index      = index  
  ) |> 
  dplyr::select(any_of(names(meta_theta_df)))

meta_theta_df <- bind_rows(meta_theta_df, dup_rows)

#### SECTION: Video-Level Theta -----


colnames(meta_theta_df)[6:40] <- paste0("X", 1:35)

meta_theta_df <- meta_theta_df |> 
  mutate(video_id = gsub("^.*?/VIDZSAMPLE/([^/]+)/.*$", "\\1", image_path))

video_theta_df <- meta_theta_df |> 
  group_by(video_id) |> 
  summarise(across(starts_with("X"), \(x) mean(x, na.rm = TRUE)))|> 
  ungroup() |> 
  mutate(
    video_id = as.character(video_id)
  )


### get the top 5 for each video 
video_top5 <- video_theta_df |> 
  pivot_longer(cols = starts_with("X"), names_to = "topic", values_to = "theta") |> 
  group_by(topic) |> 
  slice_max(order_by = theta, n = 5, with_ties = FALSE) |> 
  ungroup() |> 
  arrange(topic, desc(theta)) |> 
  mutate(topic = as.numeric(gsub("X", "", topic)),
         video_id = gsub("_", " ", video_id))


video_top5 <- video_theta_df |> 
  tidyr::pivot_longer(cols = starts_with("X"),
                      names_to = "topic", values_to = "theta") |> 
  group_by(topic) |> 
  slice_max(order_by = theta, n = 5, with_ties = FALSE) |> 
  ungroup() |> 
  arrange(topic, desc(theta)) |> 
  mutate(
    topic = as.numeric(gsub("X", "", topic)),
    video_id = gsub("_", " ", video_id),
    topic_label = c(
      "Law Enforcement & Security Incidents",
      "U.S. Military & Combat Operations",
      "Trump Campaign & Republican Primary",
      "Israel-Hamas War & Hostage Crisis",
      "Fast Food Labor & Democratic Leadership",
      "Royal Family & Child Health",
      "Personal Tech & Public Behavior",
      "Investigative Reports & Interviews",
      "Aerial Views & Infrastructure",
      "Sports Tragedies & Athletes",
      "Political Figures in Public",
      "Government Officials & Speeches",
      "Medical Emergencies & Air Travel",
      "Flooding & Natural Disasters",
      "Political Commentary & Legal Figures",
      "News Studio & Broadcast Graphics",
      "Tech Breaches & Data Security",
      "Health Regulations & Public Concerns",
      "Wildfires & Climate Extremes",
      "Hunter Biden & Political Trials",
      "Broadcast News Anchors",
      "Trump in Courtroom Settings",
      "Ports, Shipping & Conflict Zones",
      "Biden-Harris Speeches & Events",
      "Nighttime Scenes & Urban Life",
      "Protests & Public Gatherings",
      "Space Exploration & Security Barriers",
      "Winter Storms & Traffic Conditions",
      "Crowds, Attire & Outdoor Events",
      "Weather Maps & Regional Forecasts",
      "Hurricane Tracking & Storm Damage",
      "Legislative Settings & Discussions",
      "Supreme Court & Legal Rulings",
      "Tornado Destruction & Debris",
      "Corporate Investigations & Public Figures"
    )[topic]
  )


openxlsx::write.xlsx(video_top5, 
                     file = "NBC_Video_Top5Videos_PerTopic.xlsx")

save.image(paste0(Data_folder, Project_name,"_post-video-agg.rdata"))

### SECTION: Validation ----

# PARAMETERS ------------------------------------------------
LDAmodel <- LDAlist[[1]]  # Select first LDA model from list
nimage <- 20              # Number of top images per topic

# Identify column index for image paths
pathcolumn <- which(colnames(mydata) == "image_path")

# Combine topic loadings (gamma) with image paths
if (nrow(mydata) == LDAmodel@Dim[1]) {
  meta_theta_df <- cbind(mydata[pathcolumn], LDAmodel@gamma)
} else {
  # Handle case where some documents were deleted
  data_short <- mydata
  
  # Find missing docs between original data and LDA
  missing_docs <- setdiff(
    as.character(mydata$index),
    as.character(LDAmodel@documents)
  )
  
  # Remove missing docs
  data_short <- data_short |>
    filter(!(as.character(data_short$index) %in% missing_docs))
  
  # Combine shortened data with gamma values
  meta_theta_df <- cbind(data_short[pathcolumn], LDAmodel@gamma)
}

# Extract beta matrix (topic-term probabilities)
mybeta <- data.frame(LDAmodel@beta)
colnames(mybeta) <- LDAmodel@terms
mybeta <- t(mybeta)
colnames(mybeta) <- seq(1:ncol(mybeta))
mybeta <- exp(mybeta)

# Create container for top image paths per topic
toppaths <- mybeta[1:nimage, ]

# Populate toppaths with top images for each topic
for (i in 1:LDAmodel@k) {
  library(daiR)
  print(i)
  
  tempframe <- meta_theta_df[order(-meta_theta_df[, i + 1]), ]
  tempframe <- tempframe[1:nimage, ]
  tempvec <- as.vector(tempframe[, 1])
  
  toppaths[, i] <- tempvec
}

# Helper: pad numbers with leading zeros
add0 <- function(input) {
  ifelse(input < 100 & input > 9, paste0("0", input),
         ifelse(input < 10, paste0("00", input), input))
}

############################################################
# IMAGE INTRUSION TASK
# One in 6 images is from a different topic
############################################################

try(dir.create("VALIDATION_IMAGE_INT"))
try(dir.create("VALIDATION_IMAGE_INT/RAWIMAGES"))
dirname <- "VALIDATION_IMAGE_INT/RAWIMAGES/"

topiclist <- seq(1:LDAmodel@k)
log_of_answers <- data.frame(image_num = c("X"),
                             topicnum = c(1),
                             rightanswer = c(1))

for (trialnum in 1:105) {  # CHANGE: Number of trials
  print(trialnum)
  
  myloopnum <- trialnum
  origseed <- 3947 + myloopnum
  set.seed(origseed)
  
  # Sample topic for this trial
  if (length(topiclist) != 0) {
    valtopicnum <- sample(topiclist, 1)
    topiclist <- topiclist[-which(topiclist == valtopicnum)]
  } else {
    topiclist <- seq(1:LDAmodel@k)
    valtopicnum <- sample(topiclist, 1)
    topiclist <- topiclist[-which(topiclist == valtopicnum)]
    print(valtopicnum)
  }
  
  # Select 6 good images from the topic
  goodpics <- sample(toppaths[, valtopicnum], 6)
  
  # Select 1 bad image from a different topic
  valtopicnumBAD <- sample(c(1:LDAmodel@k)[!(1:LDAmodel@k) == valtopicnum], 1)
  badpic <- sample(toppaths[, valtopicnumBAD], 1)
  
  # Randomly choose position for the bad image
  rightanswer <- sample(1:6, 1)
  filenametest <- paste0("TEST_", add0(myloopnum), ".png")
  
  log_of_answers[myloopnum, ] <- c(filenametest, valtopicnum, rightanswer)
  goodpics[rightanswer] <- badpic
  
  # Read and plot images
  testimagelist <- lapply(paste0("../", goodpics), image_read)
  
  png(filename = paste0(dirname, "/TEST_", add0(myloopnum), ".png"),
      width = 1000, height = 800, units = "px")
  par(mfrow = c(3, 2), mar = c(5, 1, 1, 1))
  try(for (j in testimagelist) plot(j))
  mtext(paste0("TEST NUM: ", add0(myloopnum)),
        side = 1, line = -2, outer = TRUE, cex = 3)
  dev.off()
}

# Save answer keys
write.csv(log_of_answers, "VALIDATION_IMAGE_INT/log_of_answers_Image_Int.csv")
log_of_answers_to_fill <- log_of_answers[, -c(2)]
log_of_answers_to_fill[, 2] <- 0
write.csv(log_of_answers_to_fill,
          "VALIDATION_IMAGE_INT/log_of_answers_Image_Int_TOFILL.csv")

# Generate PDF
allimagesforpdf <- list.files(dirname, full.names = TRUE)
image_to_pdf(allimagesforpdf, "VALIDATION_IMAGE_INT/TEST_Image_Int.pdf")
try(dev.off())

############################################################
# TOPIC INTRUSION TASK
# One in 4 topic sets is correct for the given test image
############################################################

try(dir.create("VALIDATION_WHICH_TOPIC"))
try(dir.create("VALIDATION_WHICH_TOPIC/RAWIMAGES"))
dirname <- "VALIDATION_WHICH_TOPIC/RAWIMAGES/"

topiclist <- seq(1:LDAmodel@k)
log_of_answers <- data.frame(image_num = c("X"),
                             topicnum = c(1),
                             rightanswer = c(1))

for (trialnum in 1:105) {  # CHANGE: Number of trials
  print(trialnum)
  
  myloopnum <- trialnum
  origseed <- 7344 + myloopnum
  set.seed(origseed)
  
  # Select topic
  if (length(topiclist) != 0) {
    valtopicnum <- sample(topiclist, 1)
    topiclist <- topiclist[-which(topiclist == valtopicnum)]
  } else {
    topiclist <- seq(1:LDAmodel@k)
    valtopicnum <- sample(topiclist, 1)
    topiclist <- topiclist[-which(topiclist == valtopicnum)]
    print(valtopicnum)
  }
  
  # Select 5 good images (4 for clue set, 1 test image)
  goodpics <- sample(toppaths[, valtopicnum], 5)
  testpic <- paste0("../", goodpics[5])
  goodpics <- paste0("../", goodpics[-5])
  
  # Sample 4 incorrect topic sets
  valtopicnumBAD <- sample(c(1:LDAmodel@k)[!(1:LDAmodel@k) == valtopicnum], 4)
  badpiclist <- list(
    paste0("../", sample(toppaths[, valtopicnumBAD[1]], 4)),
    paste0("../", sample(toppaths[, valtopicnumBAD[2]], 4)),
    paste0("../", sample(toppaths[, valtopicnumBAD[3]], 4)),
    paste0("../", sample(toppaths[, valtopicnumBAD[4]], 4))
  )
  
  rightanswer <- sample(1:4, 1)
  badpiclist[[rightanswer]] <- goodpics
  
  filenametest <- paste0("TEST_", add0(myloopnum), ".png")
  log_of_answers[myloopnum, ] <- c(filenametest, valtopicnum, rightanswer)
  
  # Combine into one image set
  imagestoprint <- c(unlist(badpiclist), testpic)
  testimagelist <- lapply(imagestoprint, image_read)
  
  png(filename = paste0(dirname, "/TEST_", add0(myloopnum), ".png"),
      width = 1000, height = 800, units = "px")
  par(mfrow = c(5, 4), mar = c(5, 1, 1, 1))
  try(for (j in testimagelist) plot(j))
  mtext(paste0("TEST NUM: ", add0(myloopnum)),
        side = 1, line = -2, outer = TRUE, cex = 3)
  dev.off()
}

# Save answer keys
write.csv(log_of_answers, "VALIDATION_WHICH_TOPIC/log_of_answers_WhichTopic.csv")
log_of_answers_to_fill <- log_of_answers[, -c(2)]
log_of_answers_to_fill[, 2] <- 0
write.csv(log_of_answers_to_fill,
          "VALIDATION_WHICH_TOPIC/log_of_answers_WhichTopic_TOFILL.csv")

# Generate PDF
allimagesforpdf <- list.files(dirname, full.names = TRUE)
image_to_pdf(allimagesforpdf, "VALIDATION_WHICH_TOPIC/TEST_WhichTopic.pdf")
try(dev.off())

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

