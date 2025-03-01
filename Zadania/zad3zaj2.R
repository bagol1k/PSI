#tokenizacja - wyodrębnianie elementów zdania
text <- readLines(file.choose())

library(qdap)

frequent_terms <- freq_terms(text, stopwords = Top200Words)
frequent_terms
plot(frequent_terms)


install.packages("wordcloud")
library(wordcloud)

?wordcloud
?brewer.pal
brewer.pal.info

wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4, colors = brewer.pal(8,"Dark2"))
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, max.words = 5, colors = brewer.pal(8,"Accent"))

text_1 <- readLines(file.choose())

frequent_terms_1 <- freq_terms(text_1, stopwords = Top200Words)
frequent_terms_1
plot(frequent_terms_1)

wordcloud(frequent_terms_1$WORD, frequent_terms_1$FREQ, min.freq = 4, colors = brewer.pal(9,"Greens"))
wordcloud(frequent_terms_1$WORD, frequent_terms_1$FREQ, max.words = 5, colors = brewer.pal(8,"Accent"))

#pierwsze przemowienie bardziej skupia się na kwestiach życia obywateli, pracy natomiast drugie na roli prezydenta i kwestiach finansowych państwa