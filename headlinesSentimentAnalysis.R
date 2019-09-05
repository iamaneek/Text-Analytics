install.packages("ggplot2")
install.packages("readxl")
install.packages("tidytext")
install.packages("lattice")
install.packages("udpipe")  #a substitute for pattern.nlp
install.packages("ggrepel")
install.packages("syuzhet")
install.packages("igraph")
install.packages("ggraph")




library("ggplot2")
library("readxl")
library("tidytext")
library("lattice")
library("udpipe")
library("ggrepel")
library("syuzhet")
library("igraph")
library("ggraph")



require("sqldf")
require("tidyverse")
require("gcookbook")
require("dplyr")
require("reshape2")



#load the data from excel

headlines_data=read_excel("finalHeadlines.xlsx",1)
socialmedia_impactdata=read_excel("socialmediaImpact.xlsx",1)

#pre-processing

#use replace to change the values in 'Company' to 'Others' that corresponds to Company==N/A

headlines_data=headlines_data %>%
  mutate(Company=replace(Company, Company=="N/A", "Others")) %>%
  as.data.frame()


#Remove Others from the headlines_data
headlines_data=headlines_data %>%filter(!grepl(".*Others.*",Company))


attach(headlines_data)
str(headlines_data)

#top 20 occurences

head(headlines_data$Company)
cat_total=unlist(headlines_data$Company)
headlines_sort <- sort(table(cat_total), decreasing = TRUE)
head(headlines_sort,n = 20)


#number of occurences per company

frequency=sqldf("select Company,COUNT(Company) AS Frequency from headlines_data group by Company")

ggplot(frequency, aes(x=reorder(Company,Frequency), y=Frequency)) + geom_point(size=3) + theme_bw() +  
  theme(axis.text.x = element_text(angle=60, hjust=1),        
        panel.grid.major.y = element_blank(),       
        panel.grid.minor.y = element_blank(),        
        panel.grid.major.x = element_line(colour="grey60", linetype="dashed"))+
 ggtitle("Occurences Of Companies In A Month")



#frequency of headlines in a month
headlines_perday=sqldf("Select Date,count(Date) as headlines_frequency from headlines_data group by Date")

headlines_perday %>% group_by(Date) %>% count() %>% ggplot() +
  geom_line(aes(Date,headlines_perday$headlines_frequency, group = 1))+coord_flip()+
  ylab("Publishing Date Frequency")


#Top 7 mentions in a month

top7=sqldf("select Company,Frequency from frequency where Frequency>=7 order by Frequency Desc")

ggplot(top7, aes(x=Company, y=Frequency))+geom_bar(stat="identity",fill="lightblue", colour="black")+
  ylab("Number of Occurences")+xlab("Company Name")+
  ggtitle("Companies Which Were Mentioned The Most")

#Number of Verbs and Adjectives

headlines_data=headlines_data %>%
  mutate(POS=replace(POS, POS=="N/A", "Others")) %>%
  as.data.frame()


pos_stat=sqldf("select POS,COUNT(POS) as POS_STAT from headlines_data group by POS")


pos_stat=pos_stat %>%
  mutate(POS=replace(POS, POS=="V", "Verbs")) %>%
  as.data.frame()

pos_stat=pos_stat %>%
  mutate(POS=replace(POS, POS=="A", "Adjectives")) %>%
  as.data.frame()

hist(pos_stat$POS)

ggplot(pos_stat, aes(x = POS, y=POS_STAT, fill = POS)) +
  scale_fill_manual("POS", values = c("Verbs" = "#1270c9", "Adjectives" = "#f09124", "Others" = "#750a0a"))+
  # do it as a stacked bar chart first
  geom_bar(width = 1, position="identity", stat="identity", color="black") + 
  ylab("Occurences") + xlab("Parts of Speech")+ coord_polar(start=3*pi/2)
  


#TEXT ANALYTICS

#selecting distinct headlines

headline_text=sqldf("Select DISTINCT(Headline) from headlines_data")
str(headline_text)

#since headline_text was in character, attach is used to mask so that unnest_token can be performed
attach(headline_text)


#The `unnest_tokens` package is used to split each row to one token (word) in each row.
#stop words is removed with an `anti_join` function.

tidy_headline_data <- headline_text %>%
  unnest_tokens(word, Headline) %>%
  anti_join(stop_words)

#number of occurences for each word

group_tidy_data=sqldf("Select word,count(word) as word_frequency from tidy_headline_data group by word order by word_frequency desc")
top_34_word_occurences=sqldf("Select word,word_frequency from group_tidy_data where word_frequency>3")


ggplot(top_34_word_occurences,aes(y=sort(word_frequency),x=sort(word)))+ coord_flip()+
  geom_bar(stat="identity",fill="lightblue", colour="black")+
  xlab("Top 34 Occuring Words") + ylab("Number of Occurences")


#sentiment of words in terms of bing ie positive or negative output

bing_word_counts <- group_tidy_data %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

ggplot(bing_word_counts,aes(x=word,y=sentiment,fill=sentiment))+geom_bar(stat="identity",position="dodge")+
  coord_flip()+
  ylab("Type of Sentiment")+xlab("Words")

#sentiment of words in terms of nrc ie The NRC lexicon can also be "anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", or "trust"

nrc_word_counts <- group_tidy_data %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

ggplot(nrc_word_counts,aes(x=word,y=sentiment,fill=sentiment))+geom_bar(stat="identity",position="dodge")+
  coord_flip()+
  ylab("Type of Sentiment")+xlab("Words")

#nrc in categories: Visualization 1

  ggplot(nrc_word_counts,aes(word, 1, label = word, fill = sentiment )) +
  geom_point(color = "transparent") +
  geom_label_repel(force = 1,nudge_y = .5,  
                   direction = "y",
                   box.padding = 0.05,
                   segment.color = "transparent",
                   size = 3) +
  facet_grid(~sentiment) +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
        axis.title.x = element_text(size = 6),
        panel.grid = element_blank(), panel.background = element_blank(),
        panel.border = element_rect("lightgray", fill = NA),
        strip.text.x = element_text(size = 9)) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("NRC Sentiments of Headline Words") +
  coord_flip()



#nrc sentiment of words: Visualization 2
  
  ggplot(nrc_word_counts,aes(x = word, fill = sentiment)) +
    facet_grid(~sentiment) +
    geom_bar() + #Create a bar for each word per sentiment
    theme(panel.grid.major.x = element_blank(),
          axis.text.x = element_blank()) + #Place the words on the y-axis
    xlab(NULL) + ylab(NULL) +
    ggtitle("NRC Sentiment of Headline Words") +
    coord_flip()

  
  #SENTIMENT ANALYSIS USING NRC
  
  
  #Using Headlines and corresponding companies
  
  headline_and_company=sqldf("Select Company,Headline from headlines_data")
  
  #generating nrc sentiment and creating a sentiment matrix
  sentiment_emotion=get_nrc_sentiment(headline_and_company$Headline)
  headline_sentiment=cbind(headline_and_company,sentiment_emotion)
  
  #sentiment score of each headlines
  sentiment_score=get_sentiment(headline_and_company$Headline)
  
  #most positive and negative headline
  most.positive=headline_and_company$Headline[sentiment_score == max(sentiment_score)]
  most.negative=headline_and_company$Headline[sentiment_score == min(sentiment_score)]
  
  #categorizing sentiment w.r.t headlines
  
  positive.headlines=headline_and_company$Headline[sentiment_score>0]
  negative.headlines=headline_and_company$Headline[sentiment_score<0]
  neutral.headlines=headline_and_company$Headline[sentiment_score=0]
  
  
  
#categorizing headlines and adding the sentiment analysis to respective company
  
  category_headlines_sentiments=ifelse(sentiment_score<0,"Negative",ifelse(sentiment_score>0,"Positive","Neutral"))
  companywise_headline_sentiment=cbind(headline_and_company,category_headlines_sentiments)
  
  #SENTIMENT ANALYSIS VISUALIZATION
  
  #sentiment occurences
  
  sentiment_frequency=sqldf("Select category_headlines_sentiments,count(Company) as Frequency from companywise_headline_sentiment group by category_headlines_sentiments")
  
  sentiment_pie = ggplot(sentiment_frequency, aes(x=Frequency, y=category_headlines_sentiments, fill=category_headlines_sentiments)) + geom_bar(stat="identity", width=1)+ 
    coord_polar("y", start=0)+
    scale_fill_manual(values=c("#F26419", "#33658A", "#F6AE2D", "#2F4858"))+
    labs(x = NULL, y = NULL, fill = NULL, title = "Sentiment Distribution In Headlines")+
    theme_classic() + theme(axis.line = element_blank(),
                                      axis.text = element_blank(),
                                      axis.ticks = element_blank(),
                                      plot.title = element_text(hjust = 0.5, color = "#666666"))
  
  
  
  #Companywise headlines and respective score
  
  
  
  
  companywise_headline_score=cbind(headline_and_company,sentiment_score)
  total_company_score=sqldf("Select Company,total(sentiment_score) as total_score from companywise_headline_score group by company")
  
  #Top 7 Positive Companies
  top7_positive_companies=sqldf("Select Company,total_score from total_company_score where total_score>1.0 order by total_score desc")
  
  #Bottom 7 Positive Companies
  bottom7_negative_companies=sqldf("Select Company,total_score from total_company_score where total_score<-1.25 order by total_score asc")
  
  #Top 7 occuring companies with Score
  top7_occuring_scores=sqldf("Select Company,total_score from total_company_score where (Company=='Facebook' or Company=='Huawei' or Company=='Amazon' or Company=='Tesla' or Company=='Boeing' or Company=='Ethiopian Airlines' or Company=='Airbnb') order by total_score desc")
  
  
  #Worst Headlines for Ethiopian Airlines
  worst_ethiopianAirlines_headline=sqldf("select Company,Headline,min(sentiment_score) from companywise_headline_score where Company=='Ethiopian Airlines' ")
  
  
  
  #POS TAGGING ANALYTICS
  
  
  #Getting Language Model ready ie udpipe model
  
  model <- udpipe_download_model(language = "english")
  
  #IMPORTANT: used model$file_model instead of file = "english-ud-2.0-170801.udpipe"
  udmodel_english <- udpipe_load_model(model$file_model)
  
  #annoting the given text
  s <- udpipe_annotate(udmodel_english, headlines_data$Headline)
  x=data.frame(s)
  
  #frequency of pos stats
  stats <- txt_freq(x$upos)
  stats$key <- factor(stats$key, levels = rev(stats$key))
  barchart(key ~ freq, data = stats, col = "grey", 
           main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
           xlab = "Freq")
  
  #Co occurence of Verbs and Adjectives
  cooc <- cooccurrence(x = subset(x, upos %in% c("VERB", "ADJ")), 
                       term = "lemma", 
                       group = c("doc_id", "paragraph_id", "sentence_id"))
  head(cooc)
  
  wordnetwork <- head(cooc, 30)
  wordnetwork <- graph_from_data_frame(wordnetwork)
  ggraph(wordnetwork, layout = "fr") +
    geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
    geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
    theme_graph(base_family = "Arial Narrow") +
    theme(legend.position = "none") +
    labs(title = "Cooccurrences within sentence", subtitle = "Verbs & Adjective")
  
  
  
  #POS per sentence
  
  h1 <- udpipe_annotate(udmodel_english, headlines_data$Headline[1])
  x1=data.frame(h1)
  stats_h1=txt_freq(x1$upos)
  

  
  
  test1=dcast(x,doc_id~upos)
  attach(test1)
  str(test1)
  list(test1)
  
  
  #Verb and Adjective percentage wrt sentiment score
  test_total=sqldf("Select *, (ADJ+ADP+ADV+AUX+CCONJ+DET+INTJ+NOUN+NUM+PART+PRON+PROPN+PUNCT+SCONJ+SYM+VERB+X) as total from test1 ")
  docid_and_total=sqldf("Select doc_id, total from test_total group by doc_id")
  docid_and_verbs_adj=sqldf("Select doc_id,VERB,ADJ from test1")
  verb_adj_total=cbind(docid_and_verbs_adj,docid_and_total)
  verb_adj_total <- verb_adj_total[, !duplicated(colnames(verb_adj_total))]
  verb_adj_pct=sqldf("Select replace(doc_id, 'doc', '') as doc_id, VERB*100/total as verb_pct,ADJ*100/total as adj_pct from verb_adj_total WHERE  doc_id LIKE 'doc%' order by doc_id")
  verb_adj_pct$doc_id <- as.numeric(as.character(verb_adj_pct$doc_id))
  arrange_va_pct=sqldf("Select * from verb_adj_pct group by doc_id  ")
  correlate_va_data=cbind(arrange_va_pct,sentiment_score)
  
  scatter.smooth(x=correlate_va_data$verb_pct,y=correlate_va_data$sentiment_score)+
    xlab("Adjectives Frequency Percentage")+ylab("Sentiment Score")
  scatter.smooth(y=correlate_va_data$sentiment_score,x=correlate_va_data$adj_pct)
  
  ggplot(correlate_va_data, aes(x = adj_pct, y = sentiment_score)) +
    geom_point(aes(color = factor(sentiment_score))) +
    stat_smooth(method = "lm",
                col = "#C42126",
                se = FALSE,
                size = 1)
  
  ggplot(correlate_va_data, aes(x = verb_pct, y = sentiment_score)) +
    geom_point(aes(color = factor(sentiment_score))) +
    stat_smooth(method = "lm",
                col = "#C42126",
                se = FALSE,
                size = 1)
  
  ggplot(correlate_va_data, aes(x = adj_pct, y = sentiment_score)) + geom_area()
  ggplot(correlate_va_data, aes(x = verb_pct, y = sentiment_score)) + geom_area()
  
  #Accuracy
  
  
  file_conllu <- system.file(package = "udpipe", "dummydata", "traindata.conllu")
  metrics <- udpipe_accuracy(udmodel_english, file_conllu)
  metrics$accuracy
  metrics <- udpipe_accuracy(udmodel_english, file_conllu, tokenizer = "none", tagger = "default", parser = "default")
  metrics$accuracy
  metrics <- udpipe_accuracy(udmodel_english, file_conllu, tokenizer = "none", tagger = "none", parser = "default")
  metrics$accuracy
  metrics <- udpipe_accuracy(udmodel_english, file_conllu, tokenizer = "default", tagger = "none", parser = "none")
  metrics$accuracy
  
  metrics_testdata=udpipe_accuracy(udmodel_english,x)
  
  
  m <- udpipe_train(file = "model$file_model", files_conllu_training = file_conllu, 
                    annotation_tokenizer = list(dimension = 16, 
                                                epochs = 1, 
                                                batch_size = 100, 
                                                dropout = 0.7),
                    annotation_tagger = list(iterations = 1, 
                                             models = 1, 
                                             provide_xpostag = 1, 
                                             provide_lemma = 0, 
                                             provide_feats = 0), 
                    annotation_parser = "none")
  
  params <- list()
  
  ## Tokenizer training parameters
  params$tokenizer <- list(dimension = 24, 
                           epochs = 1, #epochs = 100, 
                           initialization_range = 0.1, 
                           batch_size = 100, learning_rate = 0.005, 
                           dropout = 0.1, early_stopping = 1)
  
  ## Tagger training parameters
  params$tagger <- list(models = 2, 
                        templates_1 = "tagger", 
                        guesser_suffix_rules_1 = 8, guesser_enrich_dictionary_1 = 6, 
                        guesser_prefixes_max_1 = 0, 
                        use_lemma_1 = 0, use_xpostag_1 = 1, use_feats_1 = 1, 
                        provide_lemma_1 = 0, provide_xpostag_1 = 1, 
                        provide_feats_1 = 1, prune_features_1 = 0, 
                        templates_2 = "lemmatizer", 
                        guesser_suffix_rules_2 = 6, guesser_enrich_dictionary_2 = 4, 
                        guesser_prefixes_max_2 = 4, 
                        use_lemma_2 = 1, use_xpostag_2 = 0, use_feats_2 = 0, 
                        provide_lemma_2 = 1, provide_xpostag_2 = 0, 
                        provide_feats_2 = 0, prune_features_2 = 0)
  
  ## Dependency parser training parameters
  params$parser <- list(iterations = 1, 
                        #iterations = 30, 
                        embedding_upostag = 20, embedding_feats = 20, embedding_xpostag = 0, 
                        embedding_form = 50, 
                        #embedding_form_file = "../ud-2.0-embeddings/nl.skip.forms.50.vectors", 
                        embedding_lemma = 0, embedding_deprel = 20, 
                        learning_rate = 0.01, learning_rate_final = 0.001, l2 = 0.5, hidden_layer = 200, 
                        batch_size = 10, transition_system = "projective", transition_oracle = "dynamic", 
                        structured_interval = 10)
  
  ## Train the model
  m <- udpipe_train(file = "model$file_model", 
                    files_conllu_training = file_conllu, 
                    annotation_tokenizer = params$tokenizer,
                    annotation_tagger = params$tagger,
                    annotation_parser = params$parser)
  
  
  #MyModel Accuracy Test
  
  mymodel <- udpipe_load_model("toymodel.udpipe")
  x <- udpipe_annotate(
    object = mymodel, 
    x=data.frame(s), 
    parser = "none")
  str(as.data.frame(x))
  
  
  m <- udpipe_train(file = "toymodel.udpipe", files_conllu_training = file_conllu, 
                    annotation_tokenizer = "default",
                    annotation_tagger = "default",
                    annotation_parser = "default")
  
  
  
  
  
 #SOCIAL MEDIA DATA ANALYSIS
  
  social_impact=sqldf("Select Company,Impact from socialmedia_impactdata")
  social_distribution=dcast(social_impact,Company~Impact)
  
  #renaming column
  colnames(social_distribution)[colnames(social_distribution)=="0"] <- "impact"
  colnames(social_distribution)[colnames(social_distribution)=="1"] <- "non_impact"
  
  #total impact and non impact companywise
  top7_impact=sqldf("Select Company,impact,non_impact from social_distribution where (Company=='Facebook' or Company=='Huawei' or Company=='Amazon' or Company=='Tesla' or Company=='Boeing' or Company=='Ethiopian Airlines' or Company=='Airbnb') order by impact desc")
  
  top6_mostimpact=sqldf("Select Company,impact from social_distribution where impact>2 order by impact desc")
  
  #mentions wrt social media
  top_socialmedia_mentions=sqldf("Select Company,Day1to5 from socialmedia_impactdata group by Company order by Day1to5 desc")
  
  
  top7_mostmentions=sqldf("select Company,Day1 from socialmedia_impactdata where Day1>=255675 group by Company order by Day1 desc")
  
  ggplot(top7_mostmentions,aes(x=Company,y=Day1,fill=Company))+geom_bar(stat="identity",position="dodge")+
    coord_flip()+
    xlab("Company")+ylab("Mentions On The Day of Publishing A Headline")
  
  ggplot(top6_mostimpact,aes(x=Company,y=impact,fill=Company))+geom_bar(stat="identity",position="dodge")+
    coord_flip()+
    ylab("Impactness")+xlab("Companies")
  
  
  
 #optional RAKE implementation