#NLP functions
#---------------------------------------
library(udpipe)
library(uuid)

udpipe = list.files(path = "utils/r/", pattern = "\\.udpipe$",full.names = T)

if(length(udpipe)== 0){
  udpipe_download_model(language = c("english"),model_dir = "utils/r/")
}

#text = "Alice took up the fan and gloves, and, as the hall was very hot, she kept fanning herself all the time she went on talking: `Dear, dear! How queer everything is to-day! And yesterday things went on just as usual. I wonder if I've been changed in the night? Let me think: was I the same when I got up this morning? I almost think I can remember feeling a little different. But if I'm not the same, the next question is, Who in the world am I? Ah, that's the great puzzle!' And she began thinking over all the children she knew that were of the same age as herself, to see if she could have been changed for any of them."
#text = "Apple of my eye"

udmodel <- udpipe_load_model(file = udpipe)


text = "I went go Greece for a few days after a trip for work; ended up going to one of the many small island called Ydra, but I call it the island of cats as there are so many cats on this island (I found out later that there is even a book published about them!); everything on that island was wonderful, but the cats were such a nice bonus as they are (mostly) wonderful creatures.	" 


get_morphological_features <- function(text, id=NA) {
  
  text = gsub(text, pattern = ";", replacement = "\\.")
  
  if(is.na(id)[1]) id <- UUID_assign(Change_vector = seq(1:length(text)))[[1]]
  
  x <-  udpipe_annotate(object = udmodel,
                        x = text,
                        doc_id = id)
  
  x_df <- as.data.frame(x, detailed = TRUE)
  x_df <- cbind_morphological(x_df, term = "feats")
  
  x_df <- x_df %>% mutate(token_length = end - start + 1,)
  
  filter_tokens=c("PUNCT","SYM")
  #Get general statistics----------------
  x_summary_general = x_df %>%
    filter(!upos %in% filter_tokens) %>%
    group_by(doc_id) %>%
    summarise(
      count_sentence = max(sentence_id),
      avg_sentence_size =  length(token)/ count_sentence,
      avg_token_size = mean(token_length),
      count_char = sum(token_length)
    )
  
  #Get statistics on tokens in general--------------
  x_summary_token = x_df %>%
    filter(!upos %in% filter_tokens) %>%
    group_by(doc_id) %>%
    summarise(
      count_tokens = length(token),
      count_unique_tokens = len_unique(token),
      count_unique_lemmas = len_unique(lemma),
      count_long_tokens = length(token[token_length > 6]),
    )
  
  #Sometimes if document does not contain any value for a column, that column is deleted making unios difficult later
  if(!"morph_person" %in% colnames(x_df)) x_df= x_df %>% add_columns("morph_person")
  if(!"morph_voice" %in% colnames(x_df)) x_df= x_df %>% add_columns("morph_voice")
  if(!"morph_tense" %in% colnames(x_df)) x_df= x_df %>% add_columns("morph_tense")
  
  #Get POS general Statistics-------------------------
  x_summary_POS = x_df %>%
    filter(!upos %in% filter_tokens) %>%
    group_by(doc_id,upos) %>% 
    summarise(
      count=n()
    )
  
  x_summary_POS = x_summary_POS %>% pivot_wider(
    id_cols = doc_id,
    names_from = upos,
    names_prefix = "count_",
    values_from = count
  )
  
  # #Get puntctuation summaries----------------
  # x_summary_syn = x_df %>%
  #   group_by(doc_id) %>%
  #   summarise(
  #     count_punc = len_which(upos == "PUNCT"),
  #     #   count_sym = len_which(upos == "SYM"),
  #   )
  #Get statistics on various pronouns---------------------------
  x_summary_pronouns = x_df %>%
    group_by(doc_id) %>% 
    filter(upos == "PRON") %>%
    summarise(
      count_unique_pro = len_unique(lemma),
      count_first_sin_pro = length(which( morph_person == "1" & morph_number == "Sing")),
      count_first_plur_pro = length(which(morph_person == "1" & morph_number == "Plur")),
      count_first_pro = length(which(morph_person == "1")),
      count_second_person_pro = length(which(morph_person == "2")),
      count_third_person_pro = length(which(morph_person == "2"))
    )
  
  #Statistics around verbs----------------------
  x_summary_verbs = x_df %>%
    group_by(doc_id) %>% 
    filter(upos == "VERB")%>%
    summarise(
      count_unique_verb = len_unique(lemma),
      count_passive_verb = length(which(morph_voice == "Pass")),
      count_present_tense = length(which(morph_tense == "Pres")),
      count_past_tense = length(which(morph_tense == "Past"))
    )
  #Quantifier Statistics------------------------
  x_summary_desc = x_df %>%
    group_by(doc_id) %>% 
    summarise(
      count_quantifier=length(which(upos=="DET" & is.na(feats))),
    )
  #Join all tables------------------------------------------
  x_summary_joint = Reduce(
    function(...)
      merge(..., by = "doc_id", all = TRUE),
    list(
      x_summary_token,
      x_summary_verbs,
      x_summary_pronouns,
      x_summary_desc,
      x_summary_POS
    )
  )
  
  x_summary_joint[is.na(x_summary_joint)] <- 0
  
  scale = function(x) x/x_summary_joint$count_tokens
  
  x_summary_final <- x_summary_joint %>%
    mutate_at(
      .vars = vars(
        -doc_id,
        -count_tokens
      ),
      .funs = scale
    )
  #Rename cols to reflect scaled metrics
  colnames(x_summary_final)[-c(1,2)] = gsub(pattern = "count",replacement = "ratio",x = colnames(x_summary_final)[-c(1,2)])
  
  x_summary_final <-  x_summary_general %>% left_join(y = x_summary_final,by = "doc_id")
  return(x_summary_final)
  
  #return(as.list(x_summary_final))
}

#QA----------------------
 #text = "Four score and seven years ago our fathers brought forth on this continent a new nation, conceived in liberty, and dedicated to the proposition that all men are created equal. Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure."
# id="abc"
#t = get_morphological_features(text = "","abc")
#get_morphological_features(text = "love",id="d")
