# ==============================================================================
# project code.R
# ==============================================================================

#Reddit reviews table

# --- SETUP & LIBRARIES ---
# Loading necessary libraries for the project. These cover:
# - Data Scraping (RedditExtractoR)
# - Data Manipulation (tidyverse, dplyr, lubridate)
# - Natural Language Processing / Text Mining (ngram, textclean, udpipe, tm, NLP, SnowballC)
# - Visualization (ggplot2, ggwordcloud, wordcloud, patchwork)
# - Clustering and Topic Modeling (igraph, proxy, cluster, ldatuning, topicmodels, BTM)

library(RedditExtractoR)
library(tidyverse)
library(dplyr) 
library(lubridate)
library(igraph)
library(ngram) 
library(textclean)
library(udpipe) 
library(NLP)
library(tm)
library(openxlsx)
library(ggplot2)
library(ggwordcloud)
library(proxy)
library(cluster)
library(RColorBrewer)
library(wordcloud)
library(SnowballC)
library(lsa)
library(devtools)
devtools::install_github("nikita-moor/ldatuning")
library(ldatuning)
library(topicmodels)
library(BTM)
library(text2vec)
library(syuzhet)
library(svMisc)
library(patchwork)

# --- 1. DATA GATHERING (SCRAPING) ---
# We query Reddit using specific keywords to scrape threads and comments related 
# to various recent movies. We isolate different searches and assign them to dataframes.

red1 <- search_reddit(q = '(title:"No Other Land") OR (selftext:"No Other Land")', 
                      period = "year",
                      lang= "en",
                      sortby = "all")
red1.1 <- search_reddit(q = '(title:"No Other Land") OR (selftext:"No Other Land") NOT subreddit:(oscarrace OR Oscars)', 
                      period = "year",
                      lang= "en",
                      sortby = "all")
red1.2 <- search_reddit(q = '(title:"No Other Land") OR (selftext:"No Other Land")', 
                      period = "year",
                      lang= "en",
                      subreddit = "movies",
                      sortby = "all")

red2 <- search_reddit(q = '(title:"Dune Part Two" OR "Dune: Part Two" OR "Dune Two" OR "Dune 2" NOT "Oscars") OR (selftext:"Dune Part Two" OR "Dune: Part Two" OR "Dune Two" OR "Dune 2" NOT "Oscars")', 
                      period = "year",
                      lang= "en",
                      subreddit = "movies",
                      sortby = "all")
red2.1 <- search_reddit(q = '(title:"Dune Part Two" OR "Dune: Part Two" OR "Dune Two" OR "Dune 2" NOT "Oscars")', 
                      period = "year",
                      lang= "en",
                      subreddit = "movies",
                      sortby = "all")

red3 <- search_reddit(q = 'title:("Spiderman Across the Spiderverse" OR "Spider-man Across the Spider-verse" OR "Spider-man Across the Spiderverse" OR "Spiderman Across the Spider-verse") OR (selftext:"Spiderman Across the Spiderverse" OR "Spider-man Across the Spider-verse" OR "Spider-man Across the Spiderverse" OR "Spiderman Across the Spider-verse") NOT subreddit:(oscarrace OR Oscars)', 
                      period = "year",
                      lang= "en",
                      subreddit = "moviedetails",
                      sortby = "all")
red3.1 <- search_reddit(q = 'title:("Official Discussion" AND "Across the Spider-Verse") OR (selftext:"Official Discussion" AND "Spider-Man: "Across the Spider-Verse")', 
                      period = "year",
                      lang= "en",
                      subreddit = "movies",
                      sortby = "all")

red4 <- search_reddit(q = '(title:"How To Make Millions Before Grandma Dies") OR (selftext:"How To Make Millions Before Grandma Dies")', 
                      period = "year",
                      subreddit = "movies",
                      lang = "en",
                      sortby = "all")
red4.1 <- search_reddit(q = '(title:"How To Make Millions Before Grandma Dies" NOT "Oscar") OR (selftext:"How To Make Millions Before Grandma Dies" NOT "Oscar")', 
                      period = "year",
                      lang = "en",
                      sortby = "all")

red5 <- search_reddit(q = '(title:"One Battle After Another") OR (selftext:"One Battle After Another")', 
                      period = "year",
                      subreddit = "movies",
                      lang="en",
                      sortby = "all")

# Adding a 'movie' column to label which film the scraped data belongs to.
red1.1 <- red1.1 %>% mutate(movie="No Other Land")
red2 <- red2 %>% mutate(movie="Dune: Part Two")
red3 <- red3 %>% mutate(movie="Spiderman: Across the Spiderverse")
red4 <- red4 %>% mutate(movie="How To Make Millions Before Grandma Dies")
red5 <- red5 %>% mutate(movie="One Battle After Another")

# Merging all scraped datasets into a single master dataframe.
red_tot = bind_rows(red1.1,red2,red3,red4,red5)
nrow(red1.1) + nrow(red2) + nrow(red3) + nrow(red4) +nrow(red5)

# Checking for missing values
colSums(is.na(red_tot))

c <- red_tot %>% group_by(subreddit) %>% count() %>% arrange(-n) %>% head(10)
cc <- c %>% pull(subreddit)
cc # top 10 subreddits

# Removing unnecessary metadata columns to keep the dataframe light.
to_eliminate<- c("url","timestamp", "title","score", "upvotes", "downvotes", "up_ratio",
                 "total_awards_received","golds","cross_posts", "comments", "comment_id",
                 "lang", "txt")
  
Text_ecc<-red_tot %>% 
  select(-to_eliminate) %>%
  mutate(row_id= 1:nrow(red_tot))

# --- 2. TEXT PREPROCESSING ---
# Defining a custom function to clean the raw text. In text mining, 
# you have to remove URLs, HTML, non-ascii characters, punctuation, and transform 
# everything to lowercase so algorithms can process the words correctly.

text_clean <- function(text=NULL,number=T,ordinal=T,date_time=T,email=T,hash="S",punct=T,lower=T){
  # clean text using textclean functions
  # x          text vector
  # number     removes numbers
  # ordinal    removes ordinals (1st, 2nd, ...)
  # date_time  removes both dates and times
  # hash       removes hashtags, if 'A' all the hashtag, if 'S' remove only the symbol '#'
  # punt       removes all punctation symbols
  # lower      transform all text in lower case
  # removes urls, non-ascii characters and HTML markup by default
  require(textclean)
  if(is.null(text)) stop("missing text")
  if(!hash %in% c("A","S")) stop("error in hash, valid are 'A' or 'S'")
  text <- iconv(text, from = "", to = "UTF-8", sub = "")
  text <- iconv(text, to = "UTF-8", sub = "byte")
  text <- replace_non_ascii(x = text)
  text <- replace_url(x = text)
  text <- replace_html(x = text)
  if(number==TRUE){
    text <- replace_number(x = text, remove = T)
  }
  if(ordinal==TRUE){
    text <- replace_ordinal(x = text, remove = T)
  }
  if(date_time==T){
    text <- replace_date(x = text,replacement = "")
    text <- replace_time(x = text,replacement = "")
  }
  if(email==T){
    text <- replace_email(x = text,replacement = "")
  }
  if(hash=="S"){
    text <- replace_hash(x = text,replacement = "")
  } else {
    text <- replace_hash(x = text,replacement = '$3')
  }
  if(punct==T){
    text <-gsub("[^\\p{L}\\s']", "", text, perl = TRUE) 
    }
  if(lower==T){
    text <- tolower(text)
  }
  
  
  text <- replace_white(x = text)
  return(text)
}

# Apply the cleaning function to our text column
Text_ecc$cleaned_text<-text_clean(Text_ecc$text)

#dowload a model (suited for the occasion) for my_collocations and for lemmatisation
# udpipe handles advanced linguistic tasks like identifying if a word is a noun or a verb.
language_mod_path<-udpipe::udpipe_download_model(language = "english-ewt")
language_mod <- udpipe_load_model(file = language_mod_path)


# --- 3. COLLOCATIONS (MULTIWORDS DETECTION) ---
# In text analysis, phrases like "part two" should often be treated as a single 
# token ("part_two"). This function uses Parts-Of-Speech (POS) tagging to find 
# frequent patterns (like Noun+Noun) and statistically scores them (PMI, Dice, T-score).

my_collocations_POS <- function(text = NULL, model = NULL,doc_id=NULL,verbose=F,#
                               xlsx.save = T,xlsx.name = "colloc_Movie.xlsx"){

  require(udpipe);require(tidyverse);require(ngram)
  model_list=c("italian-isdt", "italian-partut", "italian-postwita", "italian-twittiro",
               "italian-vit","english-ewt", "english-gum", "english-lines", "english-partut")
  if(is.null(text)) stop("missing text")
  if(is.null(doc_id)) doc_id=1:length(text)
  if(is.null(model)){stop("invalid argument")}
  if(class(text) != "character"){stop("invalid text")}
  if(class(model) != "udpipe_model"){
    if(class(model)=="character" & !model %in% model_list) {stop("invalid model")} else {
      if(class(model)!="character") stop("invalid model")}
  }
  if(verbose==T) cat("tokenisation\n")
  vtoken <- c();vdocid<- c();vtokid <- c()
  nwrd=sapply(text,ngram::wordcount) %>% as.vector()
  text <- text[nwrd>1]
  for(i in 1:length(text)){
    vspl=strsplit(text[i],split = " ") %>% unlist()
    vtoken <- c(vtoken,vspl)
    vdocid <- c(vdocid,rep(i,length(vspl)))
    vtokid <- c(vtokid,1:length(vspl))
  }
  tokentext <- data.frame(doc_id=vdocid,token_id=vtokid,token=vtoken) %>% as_tibble()
  if(verbose==T) cat("pos tagging\n")
  tmp=udpipe(x = tokentext$token,object = model,doc_id=tokentext$doc_id)
  tmpl=tmp %>% select(doc_id,token,lemma,upos,xpos)
  results <- data.frame(tok=as.character(),upo=as.character(),nn=as.numeric())
  ndoc=tmpl %>% group_by(doc_id) %>% count() %>% mutate(doc_id.n=as.numeric(gsub("doc","",doc_id))) %>% arrange(doc_id.n)
  if(verbose==T) cat("collocations ")
  sssq=seq(from=1,to=nrow(ndoc),by=floor(nrow(ndoc)/10))
  for(i in 1:nrow(ndoc)){
    if(verbose==T) {if(i %in% sssq) cat(".")}
    tmpd <- tmpl %>% filter(doc_id==ndoc$doc_id[i]) %>% filter(!is.na(upos))
    if(i==1){dfpos=tmpd} else {dfpos=bind_rows(dfpos,tmpd)}
  }
  
  mmm=collocation(x = dfpos,term = "token",group = "doc_id",ngram_max = 3,n_min = 0)
  
  tempo2=data.frame(
    bigram = txt_nextgram(dfpos$token, n = 2),
    bigramPOS = txt_nextgram(dfpos$upos, n = 2)) %>% group_by(bigram,bigramPOS) %>% count()
  tempo3=data.frame(trigram = txt_nextgram(dfpos$token, n = 3, sep = " "),
                    trigramPOS = txt_nextgram(dfpos$upos, n = 3, sep = " ")) %>% group_by(trigram,trigramPOS) %>% count()
  
  prv=bind_rows(
    inner_join(tempo2,mmm,by=c("bigram"="keyword")) %>% 
      filter(bigramPOS %in% c("NOUN NOUN","ADJ NOUN","PROPN PROPN","NOUN ADJ","VERB ADV","ADJ ADV")) %>% 
      as_tibble() %>% 
      select(keyword=bigram,ngram,pos=bigramPOS,freq:lfmd),
    
    inner_join(tempo3,mmm,by=c("trigram"="keyword")) %>% 
      filter(trigramPOS %in% c("NOUN NOUN NOUN","ADJ NOUN NOUN","ADJ ADJ NOUN","PROPN PROPN PROPN",
                               "NOUN ADP NOUN","PROPN ADP PROPN")) %>% 
      as_tibble() %>% 
      select(keyword=trigram,ngram,pos=trigramPOS,freq:lfmd)) %>% arrange(-freq)
  
  N=nrow(tokentext)
  prv <- prv %>% mutate(idc=1:nrow(prv),.before=1)
  prv <- prv %>% mutate(t.score=(freq-((freq_left*freq_right)/N))/sqrt(freq)) %>% 
    mutate(Dice=((2*freq)/(freq_left+freq_right))) %>% 
    mutate(LogDice=14+log2((2*freq)/(freq_left+freq_right)))
  
  results=prv
  if(xlsx.save==T){
    results <- results %>% select(1:10) %>% mutate(edit=NA) %>% mutate_at(8:10,~round(.,digits = 4))
    if(is.null(xlsx.name)==TRUE){xlsx.name <- "outMultiWordsPOS.xlsx"}
    openxlsx::write.xlsx(results,file = xlsx.name,rowNames = F,showNA = FALSE)
  }
  return(results=prv)
}

Colloc.text<-my_collocations_POS(Text_ecc$cleaned_text, "english-ewt", xlsx.save = T,
                                 xlsx.name = "colloc_Movie.xlsx")

arrange(Colloc.text, desc(freq))
head(Colloc.text)

# We filter the multi-word collocations using the Pointwise Mutual Information (PMI) score
to_correct<- Colloc.text[Colloc.text$pmi> 16,] # assuming Df_text.try was a typo for Colloc.text
to_correct
to_correct.id<- as.numeric(1:nrow(to_correct))

# This function links multi-words using underscores (e.g., "spider man" -> "spider_man")
# Correcting the MultiWords
corMultWord.xlsx <- function(x = NULL, xlsx.file = TRUE, replace.char = "_",verbose=F){
  # read the xlsx file produced by visNGram or multWordPOS functions and
  # replace multiwords defined in the field 'edit' of the xlsx file
  # if in edit "=" the spaces will be replaced with the replace.char
  # otherwise with the string reported in edit
  # x = vector of texts
  # xlsx.file = xlsx file produced by visNGram function with edit to multiwords
  # replace.char = replacement chararacter to use 
  if(is.null(x) | is.null(xlsx.file)){stop("invalid argument")}
  if(class(x) != "character" | class(xlsx.file) != "character"){stop("invalid argument")}
  require(openxlsx); require(dplyr)
  corrw <- read.xlsx(xlsx.file,sheet = 1)
  if(class(corrw) != "data.frame"){stop("invalid xlsx file")}
  corr1 <- corrw %>% arrange(-ngram) %>% filter(!is.na(edit)) %>% arrange(-freq)
  if(nrow(corr1)>0){
    for(i in 1:nrow(corr1)){
      if(verbose==T) print(corr1[i,2])
      if(trimws(tolower(corr1[i,"edit"]))=="x" | trimws(tolower(corr1[i,"edit"]))=="="){
        xcor <- gsub(" ",replace.char,corr1[i,2])
      } else {
        xcor <- trimws(corr1[i,"edit"])
        # xcor <- gsub(" ",replace.char,xcor)
      }
      x <- gsub(corr1[i,2],xcor,x)
    }
  }
  return(x)
}

Cor_text<- corMultWord.xlsx(x = Text_ecc$cleaned_text,
                                  xlsx.file= "colloc_Movie.xlsx")
Text_ecc$Cor_text<-Cor_text


# --- 4. LEMMATIZATION ---
# Lemmatization transforms words to their dictionary root form (e.g., "movies" -> "movie", "ran" -> "run").
# This creates a more standardized vocabulary. We also remove "stopwords" (common filler words).

custom_stopwords <- c(tm::stopwords("en"), "say", "also",  "also", "just", "see",
                                           "t", "dont", "movie", "fuck", "film",
                                                      "go", "get", "way", "like")

lemmatize_UDP <- function(x=NULL, model=NULL, stopwords=tm::stopwords("en"),
                          doc_id=NULL, multiw.sep="_", verbose=FALSE){
  # Lemmatizes with udpipe but first splits/tokenizes the text 
  # taking into account the symbols used to join compound forms
  if(is.null(x)) stop("missing text")
  if(is.null(model)) stop("missing lenguage model")
  if(class(model) != "udpipe_model") stop("invalid model")
  if(is.null(doc_id)) doc_id=paste0("docid",length(x))
  
  # Check if there is punctuation or only multi-word symbols
  patter = paste0("(?![",multiw.sep,"])[[:punct:]]")
  ll <- length(grep(patter,x,perl = T))
  if(ll == 0){
    xs <- strsplit(x,split = " ")
  } else {
    pttr <- paste0("(?!",multiw.sep,")[[:space:][:punct:][:digit:]]+")
    xs <- stringr::str_split(string = x,pattern = pttr)
  }
  names(xs) <- doc_id
  res <-  udpipe(x = xs,object = model,trace=verbose)
  res$STOP <- ifelse(tolower(res$lemma) %in% tolower(stopwords) | tolower(res$token) %in% tolower(stopwords),TRUE,FALSE)
  return(res)
}


outLem <- lemmatize_UDP(x = Text_ecc$Cor_text,
                    model = language_mod,
                    doc_id = Text_ecc$id, 
                    stopw = custom_stopwords,
                    verbose = T)

# Filtering out stopwords and keeping only meaningful POS (Nouns, Adjectives, etc.)
Text_Lem <- outLem %>%
  filter(!is.na(lemma) & STOP == FALSE & 
           upos %in% c("NOUN","ADJ","ADV","VERB","PROPN")) %>%
  select(doc_id, lemma) %>%
  group_by(doc_id = as.numeric(doc_id)) %>%
  summarise(textLem = paste(lemma, collapse = " "))
Text_Lem

nrow(Text_Lem); nrow(Text_ecc) #we lost nearly 50% observations

Final_text <- inner_join(Text_ecc, Text_Lem,by=c("row_id"="doc_id"), keep=NULL)

# --- 5. LEXICOMETRIC ANALYSIS ---
# Lexicometrics quantify statistical features of text (e.g., measuring text richness).
# We calculate these features for each movie individually.

##LEXICOMETRICS BASE ON MOVIES###

Df.byMovieRed <- Final_text %>%
  group_split(movie)

names(Df.byMovieRed) <- c("Dune: Part Two","How To Make Millions Before Grandma Dies",
                       "No Other Land", "One Battle After Another", 
                                          "Spiderman: Across the Spiderverse" )

# This function runs your analysis on A single text
Lexicometric_measures<- function(input_text) {
  
  print("--- Esecuzione funzione per un nuovo film ---")
  print(paste("Lunghezza del testo ricevuto:", nchar(input_text)))
  
  # 1. Create Corpus and TDM (Term-Document Matrix)
  # A Corpus is a collection of documents. A TDM is a matrix where rows are words, columns are docs.
  Corpus_test <- Corpus(VectorSource(input_text))
  TerDocMatrix <- TermDocumentMatrix(Corpus_test,
                                     control = list(wordLengths = c(2, Inf)))
  TerDocMatrix <- as.matrix(TerDocMatrix)
  
  # 2. Create dfTerms (with check for empty TDM)
  if (nrow(TerDocMatrix) == 0) {
    # If the corpus is empty (e.g. only stopwords), create an empty df
    dfTerms <- tibble(term = character(), Freq = integer())
  } else {
    dfTerms <- data.frame(term = rownames(TerDocMatrix)) %>%
      mutate(Freq = rowSums(TerDocMatrix)) %>%
      as_tibble()
  }
  
  # 3. Calculate metrics
  N <- sum(dfTerms$Freq) # corpus size
  V <- nrow(dfTerms)     # vocabulary size 
  H <- nrow(dfTerms %>% filter(Freq == 1)) # hapax (words appearing exactly once)
  
  # 4. Calculate indices (with zero division check)
  TTR <- if (N == 0) 0 else V / N # Type-Token-Ratio (lexical variety)
  HTR <- if (V == 0) 0 else H / V # Hapax-Type-Ratio
  TMF <- if (V == 0) 0 else N / V # Type mean frequency
  G   <- if (N == 0) 0 else V / sqrt(N) # Guiraud index (normalizes TTR for corpus size)
  
  # 5. Return a TIBBLE (not a cbind)
  # This is essential to make it work with map()
  return(tibble(N, V, H, TTR, HTR, TMF, G))
}


Final_result <- map(Df.byMovieRed, ~ {
  
  # .x is the single tibble (e.g. all rows of "Dune: Part Two")
  
  # Step A: Collapse all rows of text into a SINGLE string
  # We use 'paste' with 'collapse = " "'
  full.text <- .x %>%
    summarise(text = paste(textLem, collapse = " ")) %>%
    pull(text) # Extracts the single string from the tibble
  
  # Step B: Execute your analysis function on that string
  Lexicometric_measures(full.text)
  
}) %>%
  
  list_rbind(names_to = "Title")

print(Final_result)


# --- 6. DESCRIPTIVE TEXT ANALYSIS ---
# Creating frequency tables and word clouds to visually understand what words dominate.

##Descriptive analysis####

# (as you can see, if you want to eliminate some world from this groups, you can do it)

Full_text<-list()

for (i in 1:5) {
Full_text[i] <- Df.byMovieRed[[i]] %>%
  summarise(text = paste(Df.byMovieRed[[i]]$textLem, collapse = " "))
}

# In case you want to remove a specific word, here is the command
#Full_text <- gsub("jacket","",dfNFHHj$txt.p)

# We create the Term-Document Matrix again to find word frequencies
crps <- Corpus(VectorSource(Full_text))
tdm <- TermDocumentMatrix(crps,control=list(wordLengths = c(2,Inf)))
tdm_rst<- removeSparseTerms(tdm, sparse = 0.99) # Removes words that appear too rarely
inspect(tdm_rst)
mtdm_rs <- as.matrix(tdm_rst)
summary(mtdm_rs)

#Example NOTE:

#You can also identify and exclude documents with a small number of terms:
#I exclude the document 5 with only 401 terms (sum(mtdm_rs[ ,5])), obviuosly
# 401 is a number that I obtained from reddit when I personally used the code, 
#so this same code used in another moment will surely have a different result.
#inspect(tdm_rst)
#tdm_top<-tdm_rst[,-5]
#inspect(tdm_top)
#mtdm_top<-as.matrix(tdm_top)

# Create an Adjacency Matrix to model word-to-word connections
adjr <- tcrossprod(mtdm_rs)
g <- graph_from_adjacency_matrix(adjr,
                                 weighted = T,
                                 mode = "undirected")
g <- simplify(g)

##Descriptive analysis and plots####
dfTerms <- data.frame(term=rownames(mtdm_rs)) %>%
                        mutate(Freq=rowSums(mtdm_rs)) %>%
                                                  as_tibble()

topTerms <- dfTerms %>%
  arrange(desc(Freq)) %>%
  slice(1:10)
topTerms

highFreqTerms <- dfTerms %>%
  filter(Freq > 500) %>% 
  arrange(desc(Freq))

# PLOT for the most frequent
ggplot(highFreqTerms, aes(x = reorder(term, -Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "#94b39b", color = "#225c3d", size = 1.2) + 
  geom_text(aes(label = Freq), vjust = -0.5, color = "#225c3d", size = 4, fontface = "bold") +  
  theme_minimal() + 
  labs(#title = "Most Frequent Terms",
    x = "Terms",
    y = "Frequency") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),  
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),  
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

### Word Cloud ---------------------------------------------------------------
dfTerms_reduced <- dfTerms %>%
  arrange(desc(Freq)) %>%
  slice(1:200)  
ggplot(dfTerms_reduced, aes(label = term, size = Freq, colour = Freq)) +
  geom_text_wordcloud_area() +
  theme_minimal() +
  scale_size_area(max_size = 35) +
  ggtitle("Word cloud of most used terms") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  scale_color_gradient(low = "#FF808A", high = "#E50914")


# --- 7. CLUSTERING ---
# Clustering groups words that often appear together into topics/clusters.
## We can finally move forward to the next steps ####

# In case you wanted to sum all textLem into a single unique text
#all.Full_text <- purrr::map_dfr(1:5, function(i) {
# Df.byMovieRed[[i]] %>%
#    summarise(text = paste(textLem, collapse = " "))
#}) %>% summarise(text= paste(text, collapse= " "))


crps <- Corpus(VectorSource(Full_text))
tdm <- TermDocumentMatrix(crps,control=list(wordLengths = c(2,Inf)))
tdm_rst<- removeSparseTerms(tdm, sparse = 0.99)
inspect(tdm_rst)
mtdm_rs <- as.matrix(tdm_rst)

# Hierarchical clustering based on Cosine Distance and Ward's method.
dst.cs.rs <- dist(mtdm_rs,method = "cosine")
h.cl.cs.rs <- hclust(dst.cs.rs,method = "ward.D2")
par(mar=c(0,2,2,0))
plot(h.cl.cs.rs,hang = -1, cex = 0.5, main = "Hierarchical Cluster\ncosine - Ward",
     cex.main = 0.9, xlab = "", cex.axis = 0.6, cex.lab = 0.2) #This way is probably better

# Now I choose the number of clusters following the Silhouette Score
# The Silhouette score helps us mathematically decide how many clusters are optimal.
lim.clu <- c(2:20)
vSilIn <- c()
for(i in lim.clu){
  vSilIn <- c(vSilIn,mean(silhouette(x = cutree(h.cl.cs.rs,k = i),dist = dst.cs.rs)[,3]))
}
data.frame(n.clust=lim.clu,silhouette=vSilIn) %>% 
  ggplot(aes(x=n.clust,y=silhouette))+
  geom_line(color="#CF5C37",linewidth =1.5)+
  geom_point(size=2,color="#CF5C37")+
  theme_light()+
  scale_x_continuous(breaks = lim.clu)+
  ggtitle("Silhouette score")+
  theme(panel.grid = element_blank())

# In this case, the silhouette index slightly increase for increasing values 
# of the number of clusters, but we have to encounter some synthesis requirements,
# so we choose the local maximum of 4 clusters

par(mar=c(0,2,2,0))
plot(h.cl.cs.rs,hang = -1,cex=0.5, main="Hierarchical Cluster\ncosine - Ward",
     cex.main=0.9, xlab="", cex.axis=0.6, cex.lab=0.2)
rect.hclust(h.cl.cs.rs, k = 4)

clu4 <- cutree(h.cl.cs.rs, k = 4)
dfclu4 <- data.frame(term=rownames(as.matrix(mtdm_rs)),clu4) %>% 
  group_by(clu4) %>% 
  summarise(n.terms=n(),temrs=paste(term,collapse = "; "))

# Comparison Cloud --------------------------------------------------------
dfclu4 <- data.frame(term = rownames(mtdm_rs), cluster = clu4)
dfclu4_agg <- dfclu4 %>%
  group_by(cluster) %>%
  summarise(text = paste(term, collapse = " "))  

crps.clusters <- Corpus(VectorSource(dfclu4_agg$text))
tdm_clusters <- TermDocumentMatrix(crps.clusters, control = list(wordLengths = c(2,Inf)))
mtdm_clusters <- as.matrix(tdm_clusters)
colnames(mtdm_clusters) <- paste("cl.", dfclu4_agg$cluster, sep = "")  
par(mar = c(0, 0, 0, 0))  
comparison.cloud(mtdm_clusters,
                 scale = c(1.3, 0.5),
                 random.order = FALSE,
                 rot.per = 0.1,
                 max.words = 150,    
                 title.size = 1.5,   
                 match.colors = TRUE)
text(0.5, 1, "Comparison Cloud Terms by Cluster")


##############################Letterbox part#################################
# --- 8. REPLICATION FOR LETTERBOXD DATA ---
# This entire section replicates the NLP pipeline above (cleaning, 
# lemmatization, metrics, clusters) on a new dataset imported from CSV.

library(tidyverse)
library(textclean)
library(udpipe)
library(ngram)
library(openxlsx)

TableMovies<-read_csv2("C:/R/Project Data scraping/TableMovies.csv")
View(TableMovies)


TableComment<-read_csv2("C:/R/Project Data scraping/CommentTable_Final.csv")
View(TableComment)


# Vector of columns to eliminate
cols_to_eliminate <- c("language", "word_count")

# 1. DATA SELECTION
# 'TableMovies' replaces 'red_tot'
# 'MovieData_Working' replaces 'Text_ecc'
MovieData_Working <- TableComment %>% select(-cols_to_eliminate)

# --- FUNCTION 1: text_clean ---
# This function is defined as in your script
text_clean <- function(text=NULL,number=T,ordinal=T,date_time=T,email=T,hash="S",punct=T,lower=T){
  require(textclean)
  if(is.null(text)) stop("missing text")
  if(!hash %in% c("A","S")) stop("error in hash, valid are 'A' or 'S'")
  text <- iconv(text, from = "", to = "UTF-8", sub = "")
  text <- iconv(text, to = "UTF-8", sub = "byte")
  text <- replace_non_ascii(x = text)
  text <- replace_url(x = text)
  text <- replace_html(x = text)
  if(number==TRUE){
    text <- replace_number(x = text, remove = T)
  }
  if(ordinal==TRUE){
    text <- replace_ordinal(x = text, remove = T)
  }
  if(date_time==T){
    text <- replace_date(x = text,replacement = "")
    text <- replace_time(x = text,replacement = "")
  }
  if(email==T){
    text <- replace_email(x = text,replacement = "")
  }
  if(hash=="S"){
    text <- replace_hash(x = text,replacement = "")
  } else {
    text <- replace_hash(x = text,replacement = '$3')
  }
  if(punct==T){
    text <-gsub("[^\\p{L}\\s']", "", text, perl = TRUE) 
  }
  if(lower==T){
    text <- tolower(text)
  }
  
  text <- replace_white(x = text)
  return(text)
}

# 2. TEXT CLEANING
# Apply the function to our new dataframe 'MovieData_Working'
MovieData_Working$cleaned_text <- text_clean(MovieData_Working$review)

# 3. LINGUISTIC MODEL
lang_model_path <- udpipe::udpipe_download_model(language = "english-ewt")
lang_model <- udpipe_load_model(file = lang_model_path)


# --- FUNCTION 2: my_collocations_POS ---
# This function is defined as in your script
my_collocations_POS <- function(text = NULL, model = NULL,doc_id=NULL,verbose=F,
xlsx.save = T,xlsx.name = TRUE){
  require(udpipe);require(tidyverse);require(ngram)
  model_list=c("italian-isdt", "italian-partut", "italian-postwita", "italian-twittiro",
               "italian-vit","english-ewt", "english-gum", "english-lines", "english-partut")
  if(is.null(text)) stop("missing text")
  if(is.null(doc_id)) doc_id=1:length(text)
  if(is.null(model)){stop("invalid argument")}
  if(class(text) != "character"){stop("invalid text")}
  if(class(model) != "udpipe_model"){
    if(class(model)=="character" & !model %in% model_list) {stop("invalid model")} else {
      if(class(model)!="character") stop("invalid model")}
  }
  if(verbose==T) cat("tokenisation\n")
  vtoken <- c();vdocid<- c();vtokid <- c()
  nwrd=sapply(text,ngram::wordcount) %>% as.vector()
  text <- text[nwrd>1]
  for(i in 1:length(text)){
    vspl=strsplit(text[i],split = " ") %>% unlist()
    vtoken <- c(vtoken,vspl)
    vdocid <- c(vdocid,rep(i,length(vspl)))
    vtokid <- c(vtokid,1:length(vspl))
  }
  tokentext <- data.frame(doc_id=vdocid,token_id=vtokid,token=vtoken) %>% as_tibble()
  if(verbose==T) cat("pos tagging\n")
  tmp=udpipe(x = tokentext$token,object = model,doc_id=tokentext$doc_id)
  tmpl=tmp %>% select(doc_id,token,lemma,upos,xpos)
  results <- data.frame(tok=as.character(),upo=as.character(),nn=as.numeric())
  ndoc=tmpl %>% group_by(doc_id) %>% count() %>% mutate(doc_id.n=as.numeric(gsub("doc","",doc_id))) %>% arrange(doc_id.n)
  if(verbose==T) cat("collocations ")
  sssq=seq(from=1,to=nrow(ndoc),by=floor(nrow(ndoc)/10))
  for(i in 1:nrow(ndoc)){
    if(verbose==T) {if(i %in% sssq) cat(".")}
    tmpd <- tmpl %>% filter(doc_id==ndoc$doc_id[i]) %>% filter(!is.na(upos))
    # ... (commented parts omitted for brevity, as in the original) ...
    if(i==1){dfpos=tmpd} else {dfpos=bind_rows(dfpos,tmpd)}
  }
  
  mmm=collocation(x = dfpos,term = "token",group = "doc_id",ngram_max = 3,n_min = 0)
  
  tempo2=data.frame(
    bigram = txt_nextgram(dfpos$token, n = 2),
    bigramPOS = txt_nextgram(dfpos$upos, n = 2)) %>% group_by(bigram,bigramPOS) %>% count()
  tempo3=data.frame(trigram = txt_nextgram(dfpos$token, n = 3, sep = " "),
                    trigramPOS = txt_nextgram(dfpos$upos, n = 3, sep = " ")) %>% group_by(trigram,trigramPOS) %>% count()
  
  prv=bind_rows(
    inner_join(tempo2,mmm,by=c("bigram"="keyword")) %>% 
      filter(bigramPOS %in% c("NOUN NOUN","ADJ NOUN","PROPN PROPN","NOUN ADJ","VERB ADV","ADJ ADV")) %>% 
      as_tibble() %>% 
      select(keyword=bigram,ngram,pos=bigramPOS,freq:lfmd),
    
    inner_join(tempo3,mmm,by=c("trigram"="keyword")) %>% 
      filter(trigramPOS %in% c("NOUN NOUN NOUN","ADJ NOUN NOUN","ADJ ADJ NOUN","PROPN PROPN PROPN",
                               "NOUN ADP NOUN","PROPN ADP PROPN")) %>% 
      as_tibble() %>% 
      select(keyword=trigram,ngram,pos=trigramPOS,freq:lfmd)) %>% arrange(-freq)
  
  N=nrow(tokentext)
  prv <- prv %>% mutate(idc=1:nrow(prv),.before=1)
  prv <- prv %>% mutate(t.score=(freq-((freq_left*freq_right)/N))/sqrt(freq)) %>% 
    mutate(Dice=((2*freq)/(freq_left+freq_right))) %>% 
    mutate(LogDice=14+log2((2*freq)/(freq_left+freq_right)))
  
  results=prv
  if(xlsx.save==T){
    results <- results %>% select(1:10) %>% mutate(edit=NA) %>% mutate_at(8:10,~round(.,digits = 4))
    if(is.null(xlsx.name)==TRUE){xlsx.name <- "outMultiWordsPOS.xlsx"}
    openxlsx::write.xlsx(results,file = xlsx.name,rowNames = F,showNA = FALSE)
  }
  return(results=prv)
}

# 4. COLLOCATIONS EXECUTION
MovieData_Collocations <- my_collocations_POS(MovieData_Working$cleaned_text, 
                                              "english-ewt", xlsx.save = T,
                                             xlsx.name = "Review_file.xlsx") 
arrange(MovieData_Collocations, desc(freq))
head(MovieData_Collocations)


# --- FUNCTION 3: corMultWord.xlsx   ---
# This function is defined as in your script
corMultWord.xlsx <- function(x = NULL, xlsx.file = TRUE, replace.char = "_",verbose=F){
  if(is.null(x) | is.null(xlsx.file)){stop("invalid argument")}
  if(class(x) != "character" | class(xlsx.file) != "character"){stop("invalid argument")}
  require(openxlsx); require(dplyr)
  corrw <- read.xlsx(xlsx.file,sheet = 1)
  if(class(corrw) != "data.frame"){stop("invalid xlsx file")}
  corr1 <- corrw %>% arrange(-ngram) %>% filter(!is.na(edit)) %>% arrange(-freq)
  if(nrow(corr1)>0){
    for(i in 1:nrow(corr1)){
      if(verbose==T) print(corr1[i,2])
      if(trimws(tolower(corr1[i,"edit"]))=="x" | trimws(tolower(corr1[i,"edit"]))=="="){
        xcor <- gsub(" ",replace.char,corr1[i,2])
      } else {
        xcor <- trimws(corr1[i,"edit"])
      }
      x <- gsub(corr1[i,2],xcor,x)
    }
  }
  return(x)
} # <--- Bracket added to fix the syntax error from the original code


# 5. MULTIWORD CORRECTION
# 'MovieData_CorrectedTextVector' replaces 'Cor_text' (it is a vector)
MovieData_CorrectedTextVector <- corMultWord.xlsx(x = MovieData_Working$cleaned_text,
                                                  xlsx.file= "Review_file.xlsx"
                                                  #v.mod = to_correct.id
)
# Add the vector to our working dataframe
MovieData_Working$CorrectedText <- MovieData_CorrectedTextVector

# 7. LEMMATIZATION
movie_custom_stopwords <- c(tm::stopwords("en"), "say", "also",  "also", "just", "see"
                            ,"movie", "fuck", "film",
                            "go", "get", "way", "like")

# --- FUNCTION 4: lemmatize_UDP   ---
lemmatize_UDP <- function(x=NULL, model=NULL, stopwords=tm::stopwords("en"),
                          doc_id=NULL, multiw.sep="_", verbose=FALSE){
  # Lemmatizes with udpipe but first splits/tokenizes the text 
  # taking into account the symbols used to join compound forms
  if(is.null(x)) stop("missing text")
  if(is.null(model)) stop("missing lenguage model")
  if(class(model) != "udpipe_model") stop("invalid model")
  if(is.null(doc_id)) doc_id=paste0("docid",length(x))
  
  # Check if there is punctuation or only multi-word symbols
  patter = paste0("(?![",multiw.sep,"])[[:punct:]]")
  ll <- length(grep(patter,x,perl = T))
  if(ll == 0){
    xs <- strsplit(x,split = " ")
  } else {
    pttr <- paste0("(?!",multiw.sep,")[[:space:][:punct:][:digit:]]+")
    xs <- stringr::str_split(string = x,pattern = pttr)
  }
  names(xs) <- doc_id
  res <-  udpipe(x = xs,object = model,trace=verbose)
  res$STOP <- ifelse(tolower(res$lemma) %in% tolower(stopwords) | tolower(res$token) %in% tolower(stopwords),TRUE,FALSE)
  return(res)
}


# I notice that an "id" column is missing in the main df, so I am adding it now 
# since it's needed; this choice shouldn't have repercussions
MovieData_Working$id<- 1:nrow(MovieData_Working)

# 8. LEMMATIZATION EXECUTION
MovieData_Lemma_Raw <- lemmatize_UDP(x = MovieData_Working$CorrectedText,
                                     model = lang_model,
                                     doc_id = MovieData_Working$id, 
                                     stopw = movie_custom_stopwords,
                                     verbose = T)


MovieData_Lemmatized <- MovieData_Lemma_Raw %>%
  filter(!is.na(lemma) & STOP == FALSE & 
           upos %in% c("NOUN","ADJ","ADV","VERB","PROPN")) %>%
  select(doc_id, lemma) %>%
  group_by(doc_id = as.numeric(doc_id)) %>%
  summarise(textLem = paste(lemma, collapse = " "))

MovieData_Lemmatized

# Rows check
nrow(MovieData_Lemmatized); nrow(MovieData_Working) 


# 9. FINAL JOIN
MovieData_Final <- inner_join(MovieData_Working, MovieData_Lemmatized, by=c("id"="doc_id"))

# View final output
MovieData_Final


##LEXICOMETRICS BASE ON MOVIES####

Df.byMovieLet <- MovieData_Final %>%
  group_by(film) %>%
  group_split()

names(Df.byMovieLet) <- c("Dune: Part Two","How To Make Millions Before Grandma Dies",
                          "No Other Land", "One Battle After Another", 
                          "Spiderman: Across the Spiderverse" )

# This function runs your analysis on A single text
Lexicometric_measures<- function(input_text) {
  
  print("--- Esecuzione funzione per un nuovo film ---")
  print(paste("Lunghezza del testo ricevuto:", nchar(input_text)))
  
  # 1. Create Corpus and TDM (Term-Document Matrix)
  Corpus_test <- Corpus(VectorSource(input_text))
  TerDocMatrix <- TermDocumentMatrix(Corpus_test,
                                     control = list(wordLengths = c(2, Inf)))
  TerDocMatrix <- as.matrix(TerDocMatrix)
  
  # 2. Create dfTerms (with check for empty TDM)
  if (nrow(TerDocMatrix) == 0) {
    # If the corpus is empty (e.g. only stopwords), create an empty df
    dfTerms <- tibble(term = character(), Freq = integer())
  } else {
    dfTerms <- data.frame(term = rownames(TerDocMatrix)) %>%
      mutate(Freq = rowSums(TerDocMatrix)) %>%
      as_tibble()
  }
  
  # 3. Calculate metrics
  N <- sum(dfTerms$Freq) # corpus size
  V <- nrow(dfTerms)     # vocabulary size 
  H <- nrow(dfTerms %>% filter(Freq == 1)) # hapax
  
  # 4. Calculate indices (with zero division check)
  TTR <- if (N == 0) 0 else V / N # Type-Token-Ratio
  HTR <- if (V == 0) 0 else H / V # Hapax-Type-Ratio
  TMF <- if (V == 0) 0 else N / V # Type mean frequency
  G   <- if (N == 0) 0 else V / sqrt(N) # Guiraud index
  
  # 5. Return a TIBBLE (not a cbind)
  # This is essential to make it work with map()
  return(tibble(N, V, H, TTR, HTR, TMF, G))
}

Final_resultL <- map(Df.byMovieLet, ~ {
  
  # .x is the single tibble (e.g. all rows of "Dune: Part Two")
  
  # Step A: Collapse all rows of text into a SINGLE string
  # We use 'paste' with 'collapse = " "'
  full.text <- .x %>%
    summarise(text = paste(textLem, collapse = " ")) %>%
    pull(text) # Extracts the single string from the tibble
  
  # Step B: Execute your analysis function on that string
  Lexicometric_measures(full.text)
  
}) %>%
  
  list_rbind(names_to = "Title")

print(Final_resultL)

Full_textL <- list()

for (i in 1:5) {
  Full_textL[i] <- Df.byMovieLet[[i]] %>%
    summarise(text = paste(Df.byMovieLet[[i]]$textLem, collapse = " "))
}

# In case you want to remove a specific word, here is the command
#Full_textL <- gsub("jacket","",dfNFHHjL$txt.p)

crpsL <- Corpus(VectorSource(Full_textL))
tdmL <- TermDocumentMatrix(crpsL, control=list(wordLengths = c(2,Inf)))
tdm_rstL <- removeSparseTerms(tdmL, sparse = 0.99)
inspect(tdm_rstL)
mtdm_rsL <- as.matrix(tdm_rstL)
summary(mtdm_rsL)

# You can also identify and exclude documents with a small number of terms
# but in our case there aren't so we are not gonna do it

#inspect(tdm_rstL)
#tdm_topL <- tdm_rstL[,-5]
#inspect(tdm_topL)
#mtdm_topL <- as.matrix(tdm_topL)

adjrL <- tcrossprod(mtdm_rsL)

gL <- graph_from_adjacency_matrix(adjrL,
                                  weighted = T,
                                  mode = "undirected")
gL <- simplify(gL)

##Descriptive analysis and plots####
dfTermsL <- data.frame(term=rownames(mtdm_rsL)) %>%
  mutate(Freq=rowSums(mtdm_rsL)) %>%
  as_tibble()

topTermsL <- dfTermsL %>%
  arrange(desc(Freq)) %>%
  slice(1:10)
topTermsL

highFreqTermsL <- dfTermsL %>%
  filter(Freq > 100) %>% 
  arrange(desc(Freq))

# PLOT for the most frequent
ggplot(highFreqTermsL, aes(x = reorder(term, -Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "#94b39b", color = "#225c3d", size = 1.2) + 
  geom_text(aes(label = Freq), vjust = -0.5, color = "#225c3d", size = 4, fontface = "bold") +  
  theme_minimal() + 
  labs(#title = "Most Frequent Terms",
    x = "Terms",
    y = "Frequency") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),  
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),  
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

### Word Cloud ---------------------------------------------------------------
dfTerms_reducedL <- dfTermsL %>%
  arrange(desc(Freq)) %>%
  slice(1:200)  
ggplot(dfTerms_reducedL, aes(label = term, size = Freq, colour = Freq)) +
  geom_text_wordcloud_area() +
  theme_minimal() +
  scale_size_area(max_size = 35) +
  ggtitle("Word cloud of most used terms") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  scale_color_gradient(low = "#FF808A", high = "#E50914") 


## We can finally move forward to the next steps ####

# In case you wanted to sum all textLem into a single unique text
#all.Full_textL <- purrr::map_dfr(1:5, function(i) {
# Df.byMovieLet[[i]] %>%
#    summarise(text = paste(textLem, collapse = " "))
#}) %>% summarise(text= paste(text, collapse= " "))


crpsL <- Corpus(VectorSource(Full_textL))
tdmL <- TermDocumentMatrix(crpsL, control=list(wordLengths = c(2,Inf)))
tdm_rstL <- removeSparseTerms(tdmL, sparse = 0.99)
inspect(tdm_rstL)
mtdm_rsL <- as.matrix(tdm_rstL)

dst.cs.rsL <- dist(mtdm_rsL, method = "cosine")
h.cl.cs.rsL <- hclust(dst.cs.rsL, method = "ward.D2")
par(mar=c(0,2,2,0))
x11()
plot(h.cl.cs.rsL, hang = -1, cex = 0.5, main = "Hierarchical Cluster\ncosine - Ward",
     cex.main = 0.9, xlab = "", cex.axis = 0.6, cex.lab = 0.2) #This way is probably better

# Now I choose the number of clusters following the Silhouette Score
lim.cluL <- c(2:20)
vSilInL <- c()
for(i in lim.cluL){
  vSilInL <- c(vSilInL, mean(silhouette(x = cutree(h.cl.cs.rsL, k = i), dist = dst.cs.rsL)[,3]))
}
data.frame(n.clust=lim.cluL, silhouette=vSilInL) %>% 
  ggplot(aes(x=n.clust, y=silhouette))+
  geom_line(color="#CF5C37", linewidth =1.5)+
  geom_point(size=2, color="#CF5C37")+
  theme_light()+
  scale_x_continuous(breaks = lim.cluL)+
  ggtitle("Silhouette score")+
  theme(panel.grid = element_blank())

# In this case, the silhouette index slightly increase for increasing values 
# of the number of clusters, but we have to encounter some synthesis requirements,
# so we choose the local maximum of 5 clusters

par(mar=c(0,2,2,0))
plot(h.cl.cs.rsL, hang = -1, cex=0.5, main="Hierarchical Cluster\ncosine - Ward",
     cex.main=0.9, xlab="", cex.axis=0.6, cex.lab=0.2)
rect.hclust(h.cl.cs.rsL, k = 5)

clu5L <- cutree(h.cl.cs.rsL, k = 5)
dfclu5L <- data.frame(term=rownames(as.matrix(mtdm_rsL)), clu5L) %>% 
  group_by(clu5L) %>% 
  summarise(n.terms=n(), terms=paste(term, collapse = "; "))

# Comparison cloud
dfclu5L <- data.frame(term = rownames(mtdm_rsL), cluster = clu5L)
dfclu5L_agg <- dfclu5L %>%
  group_by(cluster) %>%
  summarise(text = paste(term, collapse = " "))