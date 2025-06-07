#' # ANALIZA TEXT MINING


#' #### 📌 Przetwarzanie i oczyszczanie tekstu <br>*(Text Preprocessing and Text Cleaning)*
#'
#' - wczytanie tekstu z odpowiednim kodowaniem (UTF-8)
#' - normalizacja (ujednolicenie) wielkości liter (zamiana na małe litery = lowercase)
#' - normalizacja (ujednolicenie) rozbieżnych kodowań znaków (apostrofy, cudzysłowy)
#' - normalizacja (ujednolicenie) form skróconych (I'm, I've, don't) przez usunięcie lub rozwinięcie
#' - normalizacja (ujednolicenie) różnych akcentów ("café" na "cafe") przez usunięcie akcentów
#' - normalizacja (ujednolicenie) popularnych skrótów ("btw" na "by the way", "b4" na "before") przez rozwinięcie
#' - usunięcie zbędnych ciągów znaków (adresy URL, tagi HTML)
#' - usunięcie zbędnych znaków specjalnych (*, &, #, @, $)
#' - usunięcie zbędnych białych znaków (spacja, tabulacja, znak przejścia do nowej linii "enter")
#' - usunięcie wybranych słów nie wnoszących znaczenia dla tekstu (stopwords)
#' - usunięcie cyfr i liczb
#' - usunięcie interpunkcji
#' - tokenizacja (podział tekstu na słowa = tokeny)
#' - usunięcie stopwords (słów o małej wartości semantycznej, np. "the", "and")
#' - usunięcie pustych elementów (rozważenie problemu brakujących/niekompletnych danych )
#' - stemming (sprowadzenie słów do ich rdzenia/formy podstawowej)
#'


#' #### 📌 Zliczanie częstości słów i prezentacja danych <br>
#' - zliczanie częstości słów i prezentacja wyników np. za pomoca chmury słów


#' #### 📌 Analiza sentymentu i prezentacja danych <br>
#' - analiza i dopasowanie sentymentu do konkrennych słó dla słowników Loughran i NRC
#' - wykresy słupkowe pokazujące rozkład sentymentu
#' - analiza sentymentu dla kawałków tekstu tzn. np. podział na negatywny i pozytywny sentyment za pomocą słowników GI, HE, LM
#' - wykres słupkowy przedstawiający podział sentymentu w całym tekście dla trzech wspomnianych słowników na raz
#' - analiza zmieniającego się sentymentu w tekście w zależności od momentu tekstu dla słowników GI, HE, LM
#' - prezentacja zmieniającego się sentymentu na wykresie
#' - dołączenie zdjęcia zachowania rynku podaczas wystąpienia

#' #### 📌 Topic modeling i prezentacja danych <br>
#' - podział słów różne kategorie tematyczne wg algorytmu LDA
#' - przedstawienie obszarów tematycznych na wykresach z podziałem na odpowiednio 2, 3 i 4 tematy


library(tm)
library(tidytext)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(SnowballC)
library(SentimentAnalysis)
library(ggthemes)
library(tidyverse)
library(topicmodels)
library(dplyr)


#Wybór dokumentu do przetwarzania
#wektor z ścieżkami dokumentów
docs <- c(
  "fomcpresconf20220615.txt",
  "fomcpresconf20240918.txt",
  "fomcpresconf20250319.txt",
  "fomcpresconf20250507.txt"
)

pics <- c(
  "220615D1.png",
  "240918H1.png",
  "250319M15.png",
  "250507M5.png"
)

# pytanie uzytkownika o wybor
if (interactive()) { #zeby w kompilowaniu raportu dzialalo
  cat("Wybierz dokument 1–4:\n"); for(i in seq_along(docs)) cat(i,":",docs[i],"\n")
  choice <- as.integer(readline("Podaj numer: "))
} else {
  message("Tryb nieinteraktywny – wybieram domyślnie dokument 1")
  choice <- 1
}

#sprawdzanie czy wybor jest poprawny
if (is.na(choice) || choice<1 || choice>length(docs)) {
  stop("Niepoprawny numer – musi być od 1 do 4.")
}

# wczytanie wybranego pliku
file_path <- docs[choice]
zdjecie <- pics[choice]


process_text <- function(file_path) {
  text <- tolower(readLines(file_path, encoding = "UTF-8"))
  text <- gsub("[\u2019\u2018\u0060\u00B4]", "'", text) #jednolite apostrofy
  text <- removeNumbers(text)
  words <- unlist(strsplit(text, "\\s+")) #splitowanie na spacjach
  words <- words[words != ""]
  words <- words[!str_detect(words, "'")] #usuwa slowa z apostrofami
  words <- str_replace_all(words, "[[:punct:]]", "") #usuwa znaki interpunkcyjne
  words <- words[words != ""]
  words <- str_trim(words) #usuwa znaki biale 'przy' słowach
  
  tidy_stopwords <- tolower(stop_words$word)
  tidy_stopwords <- gsub("[\u2019\u2018\u0060\u00B4]", "'", tidy_stopwords)
  tm_stopwords <- tolower(stopwords("en"))
  tm_stopwords <- gsub("[\u2019\u2018\u0060\u00B4]", "'", tm_stopwords)

  words <- words[!(words %in% tidy_stopwords)]
  words <- words[!(words %in% tm_stopwords)]
  
  stemmed_doc <- stemDocument(words)
  completed_doc <- stemCompletion(stemmed_doc, dictionary=words, type="prevalent")
  completed_doc <- completed_doc[completed_doc != ""]
  
  completed_doc
}

word_frequency <- function(words) {
  freq <- table(words)
  freq_df <- data.frame(word = names(freq), freq = unclass(freq))
  freq_df <- freq_df[order(-freq_df$freq), ] #malejąco
  return(freq_df)
}


plot_wordcloud <- function(freq_df, title, color_palette = "Dark2") {
  wordcloud(words = freq_df$word, freq = freq_df$freq, min.freq = 14,
            colors = brewer.pal(8, color_palette), random.order = FALSE) #zeby najwieksze slowo bylo na srodku
  title(title, cex.main = 2)
}

words <- process_text(file_path)

custom_stopwords <- c("$", "chair", "powell", "page", "november", "march", "june", "september", "press", "conference", "question", "michelle", "smith")
words <- words[!words %in% custom_stopwords]

freq_df <- word_frequency(words)
plot_wordcloud(freq_df, "Chmura słów")

loughran <- read.csv("loughran.csv", stringsAsFactors = FALSE)
nrc <- read.csv("nrc.csv", stringsAsFactors = FALSE)


tidy_tokeny <- as_tibble(freq_df) #ładniejsze freq_df


#' # Analiza sentymentu przy użyciu słownika Loughran
#zostaną tylko słowa ktore wystepuja w slowniku

# Zliczanie sentymentu
sentiment_review <- tidy_tokeny %>%
  inner_join(loughran, relationship = "many-to-many")

# Zliczanie, które słowa są najczęstsze
# dla danego sentymentu
sentiment_review %>%
  group_by(sentiment) %>%
  arrange(desc(freq)) %>%
  ungroup()

# Filtrowanie analizy sentymentu

sentiment_review2 <- sentiment_review %>%
  filter(sentiment %in% c("positive", "negative", "uncertainty")) #wybieramy niektóre sentymenty, poza tym innych jest malo


word_counts <- sentiment_review2 %>%
  group_by(sentiment) %>%
  top_n(20, freq) %>%
  ungroup() %>%
  arrange(desc(freq), word) %>%
  mutate(
    word2 = factor(word, levels = rev(unique(word))) #dodajemy te kolumne, zeby potem na wykresie rysowac zgodnie z rosnaca kolejnoscia
  )

# Wizualizacja sentymentu
ggplot(word_counts[1:30,], aes(x=word2, y=freq, fill=sentiment)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free") +
  coord_flip() +
  labs(x = "Słowa", y = "Liczba wystąpień") +
  theme_gdocs() + 
  ggtitle("Liczba słów wg sentymentu (Loughran)") +
  scale_fill_manual(values = c("firebrick", "darkolivegreen4","lightblue"))


#' # Analiza sentymentu przy użyciu słownika NRC

# Zliczanie sentymentu
sentiment_review_nrc <- tidy_tokeny %>%
  inner_join(nrc, relationship = "many-to-many")

# Zliczanie, które słowa są najczęstsze
# dla danego sentymentu
sentiment_review_nrc %>%
  group_by(sentiment) %>%
  arrange(desc(freq)) %>%
  ungroup()

# Filtrowanie analizy sentymentu

sentiment_review_nrc2 <- sentiment_review_nrc %>%
  filter(sentiment %in% c("positive", "negative", "anticipation", "fear"))

word_counts_nrc2 <- sentiment_review_nrc2 %>%
  group_by(sentiment) %>%
  top_n(20, freq) %>%
  ungroup() %>%
  arrange(desc(freq), word) %>%
  mutate(
    word2 = factor(word, levels = rev(unique(word)))
  )

# Wizualizacja sentymentu
ggplot(word_counts_nrc2[1:30,], aes(x=word2, y=freq, fill=sentiment)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free") +
  coord_flip() +
  labs(x = "Słowa", y = "Liczba wystąpień") +
  theme_gdocs() + 
  ggtitle("Liczba słów wg sentymentu (NRC)")


#' # Analiza sentymentu w czasie o ustalonej długości linii

# Połączenie wszystkich słów words w jeden ciąg znaków oddzielony spacjami
full_text <- paste(words, collapse = " ")


# Funkcja do dzielenia tekstu na segmenty o określonej długości
split_text_into_chunks <- function(text, chunk_size) {
  start_positions <- seq(1, nchar(text), by = chunk_size)
  chunks <- substring(text, start_positions, start_positions + chunk_size - 1)
  
  chunks
}

# Podzielenie tekstu na segmenty
text_chunks <- split_text_into_chunks(full_text, 30)

#' # Analiza sentymentu przy użyciu pakietu SentimentAnalysis
sentiment <- analyzeSentiment(text_chunks)


#' # Słownik GI (General Inquirer)
# Słownik ogólnego zastosowania
# zawiera listę słów pozytywnych i negatywnych
# zgodnych z psychologicznym słownikiem harwardzkim Harvard IV-4


# Konwersja ciągłych wartości sentymentu 
# na odpowiadające im wartości kierunkowe 
# zgodnie ze słownikiem GI
sentimentGI <- convertToDirection(sentiment$SentimentGI) #zamienia liczby reprezentujace sentyment na np. positive / negative

# Konwersja do ramki danych (ggplot wizualizuje ramki danych)
df_GI <- data.frame(index = seq_along(sentimentGI), value = sentimentGI, Dictionary = "GI")
df_GI <- na.omit(df_GI)



#' # Słownik HE (Henry’s Financial dictionary)
#
# zawiera listę słów pozytywnych i negatywnych
# zgodnych z finansowym słownikiem "Henry 2008"
# pierwszy, jaki powstał w wyniku analizy komunikatów prasowych 
# dotyczących zysków w branży telekomunikacyjnej i usług IT


# Konwersja ciągłych wartości sentymentu 
# na odpowiadające im wartości kierunkowe 
# zgodnie ze słownikiem HE
sentimentHE <- convertToDirection(sentiment$SentimentHE)


df_HE <- data.frame(index = seq_along(sentimentHE), value = sentimentHE, Dictionary = "HE")
df_HE <- na.omit(df_HE)



#' # Słownik LM (Loughran-McDonald Financial dictionary)
#
# zawiera listę słów pozytywnych i negatywnych oraz związanych z niepewnością
# zgodnych z finansowym słownikiem Loughran-McDonald
# DictionaryLM


# Konwersja ciągłych wartości sentymentu 
# na odpowiadające im wartości kierunkowe 
# zgodnie ze słownikiem LM
sentimentLM <- convertToDirection(sentiment$SentimentLM)

df_LM <- data.frame(index = seq_along(sentimentLM), value = sentimentLM, Dictionary = "LM")
df_LM <- na.omit(df_LM)


# Połączenie poszczególnych ramek w jedną ramkę
df_all <- bind_rows(df_GI, df_HE, df_LM)

# Tworzenie wykresu z podziałem na słowniki
ggplot(df_all, aes(x = value, fill = Dictionary)) +
  geom_bar(alpha = 0.4) + 
  labs(title = "Skumulowany sentyment według słowników", x = "Sentyment", y = "Liczba") +
  theme_bw() +
  facet_wrap(~Dictionary) +  # Podział na cztery osobne wykresy
  scale_fill_manual(values = c("GI" = "green", "HE" = "blue", "LM" = "orange"))



#' # Agregowanie sentymentu z różnych słowników w czasie
# Utworzenie ramki danych
df_all <- data.frame(sentence=1:length(sentiment[,1]),
                     GI=sentiment$SentimentGI, 
                     HE=sentiment$SentimentHE, 
                     LM=sentiment$SentimentLM)


# USUNIĘCIE BRAKUJĄCYCH WARTOŚCI
# gdyż wartości NA (puste) uniemożliwiają generowanie wykresu w ggplot
df_all <- df_all[complete.cases(df_all), ]

#' # Wykresy przedstawiające ewolucję sentymentu w czasie
ggplot(df_all, aes(x=sentence, y=LM)) + 
  geom_smooth(color="red") +
  geom_smooth(aes(x=sentence, y=GI), color="green") +
  geom_smooth(aes(x=sentence, y=HE), color="blue") +
  labs(x = "Oś czasu zdań", y = "Sentyment") +
  theme_gdocs() + 
  ggtitle("Zmiana sentymentu w czasie")

knitr::include_graphics(zdjecie)



# Funkcja do modelowania tematów LDA
top_terms_by_topic_LDA <- function(input_text, plot = TRUE, k = number_of_topics) {    
  # Utwórz korpus z oczyszczonego tekstu
  corpus <- VCorpus(VectorSource(input_text)) #1 ciag tekstu = 1 dokument
  DTM <- DocumentTermMatrix(corpus)
  
  # Usuń puste wiersze w macierzy DTM
  unique_indexes <- unique(DTM$i) #kolumna 'i' zawiera indeksy wierszy w ktorych sa jakies dane
  DTM <- DTM[unique_indexes,] #czyli wybieramy te wierzse w ktorych cos jest
  
  # Sprawdź, czy DTM nie jest pusty
  if (nrow(DTM) == 0) {
    stop("Macierz DTM jest pusta. Sprawdź dane wejściowe.")
  }
  
  # Wykonaj LDA
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  
  # Pobierz 10 najczęstszych słów dla każdego tematu
  top_terms <- topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  # Rysuj wykres, jeśli plot = TRUE
  if(plot) {
    top_terms %>%
      mutate(term = reorder(term, beta)) %>% #sortuje terminy wg ważności
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      labs(x = "Terminy", y = "β (ważność słowa w temacie)") +
      coord_flip() +
      theme_minimal() +
      scale_fill_brewer(palette = "Set1")
  } else {
    top_terms
  }
}

# Modelowanie tematów LDA dla różnej liczby tematów
number_of_topics <- 2
cat("\nLDA dla", number_of_topics, "tematów:\n")
top_terms_by_topic_LDA(full_text)

number_of_topics <- 3
cat("\nLDA dla", number_of_topics, "tematów:\n")
top_terms_by_topic_LDA(full_text)

number_of_topics <- 4
cat("\nLDA dla", number_of_topics, "tematów:\n")
top_terms_by_topic_LDA(full_text)
