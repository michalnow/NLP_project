#załadowanie bibliotek
library(tm)
library(hunspell)
library(stringr)
library(corrplot)
library(dendextend)
library(proxy)
library(flexclust)
library(wordcloud)
library(topicmodels)
library(lsa)
library(slowraker)

#zmiana katalogu roboczego
workDir <- "C:\\Users\\micha\\Desktop\\NLP_projekt\\projekt"
setwd(workDir)

#definicja lokalizacji katalogów funkcjonalnych

inputDir <- ".\\14_Dokumenty"
outputDir <- ".\\results"
workspaceDir <- ".\\workspaces"

#utworzenie korpusu dokumentów
corpusDir <- paste(
  inputDir,
  "teksty",
  sep = "\\"
)
corpus <- VCorpus(
  DirSource(
    corpusDir,
    "UTF-8",
    "*.txt"
  ),
  readerControl = list(
    language = "pl_PL"
  )
)

not_processed_corpus <- corpus

#wstępne przetwarzanie
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))

#usunięcie słów ze stoplisty
stoplistFile <- paste(
  inputDir,
  "stopwords_pl.txt",
  sep = "\\"
)
stoplist <- readLines(stoplistFile, encoding = "UTF-8")
corpus <- tm_map(corpus, removeWords, stoplist)
corpus <- tm_map(corpus, stripWhitespace)

#usunięcie em dash i 3/4
removeChar <- function(x,char) gsub(char, "", x)
corpus <- tm_map(corpus, content_transformer(removeChar), intToUtf8(8722))
corpus <- tm_map(corpus, content_transformer(removeChar), intToUtf8(190))

#usunięcie w dokumentach podziałów na akapity
pasteParagraphs <- content_transformer(
  function(x, char) paste(x, collapse=char)
)
corpus <- tm_map(corpus, pasteParagraphs, " ")

#usunięcie z nazw dokumentów rozszerzeń
cutExtensions <- function(document){
  meta(document, "id") <- gsub(
    "\\.txt$",
    "",
    meta(document, "id")
  )
  return(document)
}
corpus <- tm_map(corpus, cutExtensions)

#lematyzacja
polish <- dictionary("pl_PL")
lemmatize <- content_transformer(
  function(text){
    simpleText <- str_trim(as.character(text))
    vectorizedText <- hunspell_parse(simpleText, dict = polish)
    lemmatizedText <- hunspell_stem(vectorizedText[[1]], dict = polish)
    for (i in 1:length(lemmatizedText)) {
      if(length(lemmatizedText[[i]]) == 0) lemmatizedText[i] <- vectorizedText[[1]][i]
      if(length(lemmatizedText[[i]])  > 1) lemmatizedText[i] <- lemmatizedText[[i]][1]
    }
    newText <- paste(lemmatizedText, collapse = " ")
    return(newText)
  }
)
corpus <- tm_map(corpus, lemmatize)

#eksport wstępnie przetworzonego korpusu dokumentów
preprocessedDir <- paste(
  inputDir,
  "14_Dokumenty_przetworzone",
  sep = "\\"
)
dir.create(preprocessedDir,showWarnings = F)
writeCorpus(corpus, path = preprocessedDir)

#/////////////////////////////////////////////////////////////////////////////////////////////////////
#utworzenie korpusu przetworzonych dokumentów
corpusDir <- paste(
  inputDir,
  "14_Dokumenty_przetworzone",
  sep = "\\"
)
corpus <- VCorpus(
  DirSource(
    corpusDir,
    "CP1250",
    "*.txt"
  ),
  readerControl = list(
    language = "pl_PL"
  )
)

#usunięcie z nazw dokumentów rozszerzeń
cutExtensions <- function(document){
  meta(document, "id") <- gsub(
    "\\.txt$",
    "",
    meta(document, "id")
  )
  return(document)
}
corpus <- tm_map(corpus, cutExtensions)

#//////////////// ///////////MACIERZE CZĘŚTOŚĆI ///////////////////////////////

tdmTfAll <- TermDocumentMatrix(corpus)
tdmTfIdfAll <- TermDocumentMatrix(
  corpus, 
  control = list(
    weighting = weightTfIdf
  )
)
tdmTfBounds <- TermDocumentMatrix(
  corpus, 
  control = list(
    bounds = list(
      global = c(1,7)
    )
  )
)
tdmTfIdfBounds <- TermDocumentMatrix(
  corpus, 
  control = list(
    weighting = weightBin,
    bounds = list(
      global = c(4,9)
    )
  )
)
dtmTfAll <- DocumentTermMatrix(corpus)
dtmTfIdfAll <- DocumentTermMatrix(
  corpus, 
  control = list(
    weighting = weightSMART
  )
)
dtmTfBounds <- DocumentTermMatrix(
  corpus, 
  control = list(
    bounds = list(
      global = c(2,11)
    )
  )
)
dtmTfIdfBounds <- DocumentTermMatrix(
  corpus, 
  control = list(
    weighting = weightTf,
    bounds = list(
      global = c(4,18)
    )
  )
)

dtmBinAll <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf
  )
)

#konwersja macierzy rzadkich do macierzy klasycznych
tdmTfAllMatrix <- as.matrix(tdmTfAll)
tdmTfIdfAllMatrix <- as.matrix(tdmTfIdfAll)
tdmTfBoundsMatrix <- as.matrix(tdmTfBounds)
tdmTfIdfBoundsMatrix <- as.matrix(tdmTfIdfBounds)
dtmTfAllMatrix <- as.matrix(dtmTfAll)
dtmTfIdfAllMatrix <- as.matrix(dtmTfIdfAll)
dtmTfBoundsMatrix <- as.matrix(dtmTfBounds)
dtmTfIdfBoundsMatrix<- as.matrix(dtmTfIdfBounds)
dtmBinAllMatrix <- as.matrix(dtmBinAll)

#eksport macierzy częstości do pliku .csv
matrixFile <- paste(outputDir, "tdmTfAll.csv", sep = "\\")
write.table(tdmTfAllMatrix, file = matrixFile, sep = ";", dec = ",", col.names = NA)

matrixFile <- paste(outputDir, "dtmTfAllMatrix.csv", sep = "\\")
write.table(dtmTfAllMatrix, file = matrixFile, sep = ";", dec = ",", col.names = NA)

matrixFile <- paste(outputDir, "dtmTfIdfBoundsMatrix.csv", sep = "\\")
write.table(dtmTfIdfBoundsMatrix, file = matrixFile, sep = ";", dec = ",", col.names = NA)

matrixFile <- paste(outputDir, "tdmTfIdfBoundsMatrix.csv", sep = "\\")
write.table(tdmTfIdfBoundsMatrix, file = matrixFile, sep = ";", dec = ",", col.names = NA)


#/////////////////////////////////////////////////////////////////////////////////////////////////////
#analiza skupień
#przygotowanie
par(mai = c(1,2,1,1))
nDocuments <- 20
docNames <- rownames(dtmTfAll)
legend <- paste(
  paste("d",1:20, sep = ""),
  rownames(dtmTfAll),
  sep = " - "
)
clusterPattern <- c(3,3,3,3,3,3,3,3,1,1,1,1,1,1,1,2,2,2,2,2)
colors <- c("orange", "violet", "turquoise")
colorsPattern <- c()
for (i in 1:nDocuments) {
  colorsPattern[i] <- colors[clusterPattern[i]]
}
names(colorsPattern) <- paste("d",1:20, sep = "")

#eksperyment 1
rownames(dtmTfAllMatrix) <- paste("d",1:20, sep = "")
distMatrix1 <- dist(dtmTfAllMatrix, method = "euclidean")
hclust1 <- hclust(distMatrix1, method = "complete")
plot(hclust1)
barplot(hclust1$height, names.arg = 19:1)
nClusters1 <- 5
clusters1 <- cutree(hclust1, k = nClusters1)
clustersMatrix1 <- matrix(0,nDocuments,nClusters1)
rownames(clustersMatrix1) <- docNames
for (i in 1:nDocuments) {
  clustersMatrix1[i,clusters1[i]] <- 1
}
corrplot(clustersMatrix1)
dendrogram1 <- as.dendrogram(hclust1)
coloredDendrogram1 <- color_branches(dendrogram1, h = 100)
plot(coloredDendrogram1)
find_k(dendrogram1)
#kolorowanie gałęzi wg "oryginalnych" klas
coloredDendrogram1 <- color_branches(dendrogram1, col = colorsPattern[dendrogram1 %>% labels])
plot(coloredDendrogram1)

#eksperyment 2
rownames(dtmTfIdfBoundsMatrix) <- paste("d",1:20, sep = "")
distMatrix2 <- dist(dtmTfIdfBoundsMatrix, method = "cosine")
hclust2 <- hclust(distMatrix2, method = "ward.D2")
plot(hclust2)
barplot(hclust2$height, names.arg = 19:1)
nClusters2 <- 3
clusters2 <- cutree(hclust2, k = nClusters2)
clustersMatrix2 <- matrix(0,nDocuments,nClusters2)
rownames(clustersMatrix2) <- docNames
for (i in 1:nDocuments) {
  clustersMatrix2[i,clusters2[i]] <- 1
}
corrplot(clustersMatrix2)
dendrogram2 <- as.dendrogram(hclust2)
coloredDendrogram2 <- color_branches(dendrogram2, h = 1.3)
plot(coloredDendrogram2)
find_k(dendrogram2)

#eksperyment 3
rownames(dtmBinAllMatrix) <- paste("d",1:20, sep = "")
distMatrix3 <- dist(dtmBinAllMatrix, method = "jaccard")
hclust3 <- hclust(distMatrix3, method = "single")
plot(hclust3)
barplot(hclust3$height, names.arg = 19:1)
nClusters3 <- 5
clusters3 <- cutree(hclust3, k = nClusters3)
clustersMatrix3 <- matrix(0,nDocuments,nClusters3)
rownames(clustersMatrix3) <- docNames
for (i in 1:nDocuments) {
  clustersMatrix3[i,clusters3[i]] <- 1
}
corrplot(clustersMatrix3)
dendrogram3 <- as.dendrogram(hclust3)
coloredDendrogram3 <- color_branches(dendrogram3, h = 0.8)
plot(coloredDendrogram3)
find_k(dendrogram3)

#eksperyment 4
rownames(dtmTfAllMatrix) <- paste("d",1:20, sep = "")
distMatrix4 <- dist(dtmTfAllMatrix, method = "jaccard")
hclust4 <- hclust(distMatrix4, method = "ward.D2")
plot(hclust4)
barplot(hclust4$height, names.arg = 19:1)
nClusters4 <- 7
clusters4 <- cutree(hclust4, k = nClusters4)
clustersMatrix4 <- matrix(0,nDocuments,nClusters4)
rownames(clustersMatrix4) <- docNames
for (i in 1:nDocuments) {
  clustersMatrix4[i,clusters4[i]] <- 1
}
corrplot(clustersMatrix4)
dendrogram4 <- as.dendrogram(hclust4)
coloredDendrogram4 <- color_branches(dendrogram4, h = 100)
plot(coloredDendrogram4)
find_k(dendrogram4)
#kolorowanie gałęzi wg "oryginalnych" klas
coloredDendrogram4 <- color_branches(dendrogram4, col = colorsPattern[dendrogram4 %>% labels])
plot(coloredDendrogram4)

#porównanie wyników eksperymentów
Bk_plot(
  dendrogram1,
  dendrogram2,
  add_E = F,
  rejection_line_asymptotic = F,
  main = "Fawlkes-Mallows Index",
  ylab = "Fawlkes-Mallows Index"
)
Bk_plot(
  dendrogram1,
  dendrogram3,
  add_E = F,
  rejection_line_asymptotic = F,
  main = "Fawlkes-Mallows Index",
  ylab = "Fawlkes-Mallows Index"
)
Bk_plot(
  dendrogram2,
  dendrogram3,
  add_E = F,
  rejection_line_asymptotic = F,
  main = "Fawlkes-Mallows Index",
  ylab = "Fawlkes-Mallows Index"
)

randEx1Ex2 <- comPart(clusters1,clusters2)
randEx1Ex3 <- comPart(clusters1,clusters3)
randEx2Ex3 <- comPart(clusters2,clusters3)
randEx1Pattern <- comPart(clusters1,clusterPattern)
randEx2Pattern <- comPart(clusters2,clusterPattern)
randEx3Pattern <- comPart(clusters3,clusterPattern)

##metoda niehierarchiczna

#eksperyment 5
nClusters5 <- 5
kmeans5 <- kmeans(dtmTfBounds, centers = nClusters5)
clusters5 <- kmeans5$cluster
clustersMatrix5 <- matrix(0,nDocuments,nClusters5)
rownames(clustersMatrix5) <- docNames
for (i in 1:nDocuments) {
  clustersMatrix5[i,clusters5[i]] <- 1
}
corrplot(clustersMatrix5)

randEx5Pattern <- comPart(clusters5,clusterPattern)
randEx1Ex5 <- comPart(clusters1,clusters5)
randEx2Ex5 <- comPart(clusters2,clusters5)
randEx3Ex5 <- comPart(clusters3,clusters5)

#eksperyment 6
nClusters6 <- 5
kmeans6 <- kmeans(dtmTfBounds, centers = nClusters6)
clusters6 <- kmeans6$cluster
clustersMatrix6 <- matrix(0,nDocuments,nClusters6)
rownames(clustersMatrix6) <- docNames
for (i in 1:nDocuments) {
  clustersMatrix6[i,clusters6[i]] <- 1
}
corrplot(clustersMatrix6)

randEx6Pattern <- comPart(clusters6,clusterPattern)
randEx1Ex6 <- comPart(clusters1,clusters6)
randEx2Ex6 <- comPart(clusters2,clusters6)
randEx3Ex6 <- comPart(clusters3,clusters6)
randEx5Ex6 <- comPart(clusters5,clusters6)

#/////////////////////////////////////////////////////////////////////////////////////////////////////
#analiza ukrytej alokacji Dirichlet'a
nTopics <- 5
lda <- LDA(
  dtmTfAll,
  k = nTopics,
  method = "Gibbs",
  control = list(
    burnin = 2000,
    thin = 100,
    iter = 3000
  )
)
results <- posterior(lda)

#przygotowanie parametrów wykresów
par(mai = c(1,2,1,1))
colors <- c("orange", "violet", "lightskyblue", "darkseagreen")

#prezentacja tematów
topic1 <- head(sort(results$terms[1,], decreasing = T), 20)
barplot(
  rev(topic1),
  horiz = T,
  las = 1,
  main = "Kolarstwo",
  xlab = "Prawdopodobieństwo",
  col = colors[1]
)
topic2 <- head(sort(results$terms[2,], decreasing = T), 20)
barplot(
  rev(topic2),
  horiz = T,
  las = 1,
  main = "Muzyka",
  xlab = "Prawdopodobieństwo",
  col = colors[2]
)
topic3 <- head(sort(results$terms[3,], decreasing = T), 20)
barplot(
  rev(topic3),
  horiz = T,
  las = 1,
  main = "Piłka Nożna",
  xlab = "Prawdopodobieństwo",
  col = colors[3]
)
topic4 <- head(sort(results$terms[3,], decreasing = T), 20)
barplot(
  rev(topic4),
  horiz = T,
  las = 1,
  main = "Sprzęt elektroniczny",
  xlab = "Prawdopodobieństwo",
  col = colors[4]
)

topic5 <- head(sort(results$terms[5,], decreasing = T), 20)
barplot(
  rev(topic5),
  horiz = T,
  las = 1,
  main = "Szachy",
  xlab = "Prawdopodobieństwo",
  col = colors[4]
)

#prezentacja dokumentów
document1 <- results$topics[1,]
barplot(
  document1,
  horiz = T,
  las = 1,
  main = rownames(results$topics)[1],
  xlab = "Prawdopodobieństwo",
  col = colors
)
document4 <- results$topics[4,]
barplot(
  document4,
  horiz = T,
  las = 1,
  main = rownames(results$topics)[4],
  xlab = "Prawdopodobieństwo",
  col = colors
)
document11 <- results$topics[11,]
barplot(
  document11,
  horiz = T,
  las = 1,
  main = rownames(results$topics)[11],
  xlab = "Prawdopodobieństwo",
  col = colors
)
document17 <- results$topics[17,]
barplot(
  document17,
  horiz = T,
  las = 1,
  main = rownames(results$topics)[17],
  xlab = "Prawdopodobieństwo",
  col = colors
)

#udział tematów w słowach
options(scipen = 5)

words1 <- c("szachy", "geforce", "mat")
round(results$terms[,words1],4)

words2 <- c("gol", "piłka", "moc")
round(results$terms[,words2],4)

words3 <- c("muzyka", "dźwięk", "instrument")
round(results$terms[,words3],4)


#/////////////////////////////////////////////////////////////////////////////////////////////////////
#analiza głównych składowych
pca <- prcomp(dtmTfIdfBounds)

#przygotowanie danych do wykresu
x <- pca$x[,1]
y <- pca$x[,2]
legend <- paste(
  paste("d",1:20, sep = ""),
  rownames(dtmTfIdfBounds),
  sep = " - "
)

#eksport wykresu do pliku .png
plotFile <- paste(
  outputDir,
  "pca.png",
  sep = "\\"
)
png(filename = plotFile)

#wykres dokumentów w przestrzeni dwuwymiarowej
options(scipen = 5)
plot(
  x,
  y,
  main = "Analiza głównych składowych",
  xlab = "PC1",
  ylab = "PC2",
  col = "purple4",
  #xlim = c(-0.005,0.005),
  #ylim = c(0.03,0.055),
  pch = 18
)
text(
  x,
  y,
  paste("d",1:20, sep = ""),
  col = "purple4",
  pos = 4,
  cex = 0.8
)
legend(
  "bottom",
  legend,
  cex = 0.6,
  text.col = "purple4"
)

#zamknięcie pliku .png
dev.off()

#/////////////////////////////////////////////////////////////////////////////////////////////////////
#analiza ukrytych wymiarów semantycznych
#dekompozycja (rozkład) wg wartości osobliwych
lsa <- lsa(tdmTfBoundsMatrix)

#przygotowanie danych do wykresu
coordDocs <- lsa$dk%*%diag(lsa$sk)
coordTerms <- lsa$tk%*%diag(lsa$sk)

termsImportance <- diag(lsa$tk%*%diag(lsa$sk)%*%t(diag(lsa$sk))%*%t(lsa$tk))
importantTerms <- names(tail(sort(termsImportance), 30))
coordImportantTerms <- coordTerms[importantTerms,]
#ownTerms <- c("dieta", "szachy", "muzyka", "dźwięk", "piłka", "mat")
#coordOwnTerms <- coordTerms[ownTerms,]

legend <- paste(
  paste("d",1:20, sep = ""),
  rownames(coordDocs),
  sep = " - "
)

x1 <- coordDocs[,1]
y1 <- coordDocs[,2]
x2 <- coordImportantTerms[,1]
y2 <- coordImportantTerms[,2]
#x2 <- coordOwnTerms[,1]
#y2 <- coordOwnTerms[,2]

#eksport wykresu do pliku .png
plotFile <- paste(
  outputDir,
  "lsa.png",
  sep = "\\"
)
png(filename = plotFile)

#wykres dokumentów w przestrzeni dwuwymiarowej
options(scipen = 5)
plot(
  x1,
  y1,
  main = "Analiza krytych wymiarów semantycznych",
  xlab = "SD1",
  ylab = "SD2",
  col = "purple4",
  xlim = c(-16,0),
  ylim = c(0,10),
  pch = 18
)
text(
  x1,
  y1,
  paste("d",1:20, sep = ""),
  col = "purple4",
  pos = 4,
  cex = 0.8
)
points(
  x2,
  y2,
  col = "magenta4",
  pch = 15
)
text(
  x2,
  y2,
  rownames(coordOwnTerms),
  col = "magenta4",
  pos = 2,
  cex = 0.8
)
legend(
  "topleft",
  legend,
  cex = 0.6,
  text.col = "purple4"
)

#zamknięcie pliku .png
dev.off()

#/////////////////////////////////////////////////////////////////////////////////////////////////////
#waga tf jako miara ważności słów
#dla pierwszego dokumentu
keywordsTf1 <- head(sort(dtmTfAllMatrix[1,],decreasing = T))
keywordsTf1

##waga tfidf jako miara ważności słów
keywordsTfIdf1 <- head(sort(dtmTfIdfAllMatrix[1,],decreasing = T))
keywordsTfIdf1

##prawdopodobieństwo w LDA jako miara ważności słów
termsImportance1 <- c(results$topics[1,]%*%results$terms)
names(termsImportance1) <- colnames(results$terms)
keywordsLda1 <- head(sort(termsImportance1,decreasing = T))
keywordsLda1

##chmury tagów
par(mai = c(0,0,0,0))
wordcloud(corpus[1], max.words = 200, color = brewer.pal(8,"PuOr"))

keywordsTf2 <- head(sort(dtmTfAllMatrix[2,],decreasing = T))
keywordsTf2

keywordsTfIdf2 <- head(sort(dtmTfIdfAllMatrix[2,],decreasing = T))
keywordsTfIdf2

termsImportance2 <- c(results$topics[2,]%*%results$terms)
names(termsImportance2) <- colnames(results$terms)
keywordsLda2 <- head(sort(termsImportance2,decreasing = T))
keywordsLda2

par(mai = c(0,0,0,0))
wordcloud(corpus[2], max.words = 200, color = brewer.pal(8,"PuOr"))

##3
keywordsTf3 <- head(sort(dtmTfAllMatrix[3,],decreasing = T))
keywordsTf3

keywordsTfIdf3 <- head(sort(dtmTfIdfAllMatrix[3,],decreasing = T))
keywordsTfIdf3

termsImportance3 <- c(results$topics[3,]%*%results$terms)
names(termsImportance3) <- colnames(results$terms)
keywordsLda3 <- head(sort(termsImportance3,decreasing = T))
keywordsLda3

for(idx in 4:20) {
  termsImportance1 <- c(results$topics[idx,]%*%results$terms)
  names(termsImportance1) <- colnames(results$terms)
  keywordsLda1 <- head(sort(termsImportance1,decreasing = T))
  print(rownames(results$topics)[idx])
  print(keywordsLda1)
}



par(mai = c(0,0,0,0))
wordcloud(corpus[3], max.words = 200, color = brewer.pal(8,"PuOr"))

file <- "C:/Users/micha/Desktop/NLP_projekt/projekt/14_Dokumenty/teksty/kolarstwo1.txt"
text <- paste(readLines(file, encoding = "UTF-8"), collapse = " ")
rk <- slowrake(txt = text, stem=FALSE, stop_pos = NULL, stop_words = stoplist)
print(rk[[1]])

file <- "C:/Users/micha/Desktop/NLP_projekt/projekt/14_Dokumenty/teksty/kolarstwo2.txt"
text <- paste(readLines(file2, encoding = "UTF-8"), collapse = " ")
rk <- slowrake(txt = text, stem=FALSE, stop_pos = NULL, stop_words = stoplist)
print(rk[[1]])

file <- "C:/Users/micha/Desktop/NLP_projekt/projekt/14_Dokumenty/teksty/kolarstwo3.txt"
text <- paste(readLines(file, encoding = "UTF-8"), collapse = " ")
rk <- slowrake(txt = text, stem=FALSE, stop_pos = NULL, stop_words = stoplist)
print(rk[[1]])

file <- "C:/Users/micha/Desktop/NLP_projekt/projekt/14_Dokumenty/teksty/kolarstwo4.txt"
text <- paste(readLines(file, encoding = "UTF-8"), collapse = " ")
rk <- slowrake(txt = text, stem=FALSE, stop_pos = NULL, stop_words = stoplist)
print(rk[[1]])

file <- "C:/Users/micha/Desktop/NLP_projekt/projekt/14_Dokumenty/teksty/muzyka1.txt"
text <- paste(readLines(file, encoding = "UTF-8"), collapse = " ")
rk <- slowrake(txt = text, stem=FALSE, stop_pos = NULL, stop_words = stoplist)
print(rk[[1]])

file <- "C:/Users/micha/Desktop/NLP_projekt/projekt/14_Dokumenty/teksty/muzyka2.txt"
text <- paste(readLines(file, encoding = "UTF-8"), collapse = " ")
rk <- slowrake(txt = text, stem=FALSE, stop_pos = NULL, stop_words = stoplist)
print(rk[[1]])

file <- "C:/Users/micha/Desktop/NLP_projekt/projekt/14_Dokumenty/teksty/muzyka3.txt"
text <- paste(readLines(file, encoding = "UTF-8"), collapse = " ")
rk <- slowrake(txt = text, stem=FALSE, stop_pos = NULL, stop_words = stoplist)
print(rk[[1]])

file <- "C:/Users/micha/Desktop/NLP_projekt/projekt/14_Dokumenty/teksty/muzyka4.txt"
text <- paste(readLines(file, encoding = "UTF-8"), collapse = " ")
rk <- slowrake(txt = text, stem=FALSE, stop_pos = NULL, stop_words = stoplist)
print(rk[[1]])

file <- "C:/Users/micha/Desktop/NLP_projekt/projekt/14_Dokumenty/teksty/PiłkaNożna1.txt"
text <- paste(readLines(file, encoding = "UTF-8"), collapse = " ")
rk <- slowrake(txt = text, stem=FALSE, stop_pos = NULL, stop_words = stoplist)
print(rk[[1]])
file <- "C:/Users/micha/Desktop/NLP_projekt/projekt/14_Dokumenty/teksty/PiłkaNożna2.txt"
text <- paste(readLines(file, encoding = "UTF-8"), collapse = " ")
rk <- slowrake(txt = text, stem=FALSE, stop_pos = NULL, stop_words = stoplist)
print(rk[[1]])

file <- "C:/Users/micha/Desktop/NLP_projekt/projekt/14_Dokumenty/teksty/PiłkaNożna3.txt"
text <- paste(readLines(file, encoding = "UTF-8"), collapse = " ")
rk <- slowrake(txt = text, stem=FALSE, stop_pos = NULL, stop_words = stoplist)
print(rk[[1]])

file <- "C:/Users/micha/Desktop/NLP_projekt/projekt/14_Dokumenty/teksty/kolarstwo1.txt"
text <- paste(readLines(file, encoding = "UTF-8"), collapse = " ")
rk <- slowrake(txt = text, stem=FALSE, stop_pos = NULL, stop_words = stoplist)
print(rk[[1]])

file <- "C:/Users/micha/Desktop/NLP_projekt/projekt/14_Dokumenty/teksty/PiłkaNożna4.txt"
text <- paste(readLines(file, encoding = "UTF-8"), collapse = " ")
rk <- slowrake(txt = text, stem=FALSE, stop_pos = NULL, stop_words = stoplist)
print(rk[[1]])

file <- "C:/Users/micha/Desktop/NLP_projekt/projekt/14_Dokumenty/teksty/SprzętElektroniczny1.txt"
text <- paste(readLines(file, encoding = "UTF-8"), collapse = " ")
rk <- slowrake(txt = text, stem=FALSE, stop_pos = NULL, stop_words = stoplist)
print(rk[[1]])
file <- "C:/Users/micha/Desktop/NLP_projekt/projekt/14_Dokumenty/teksty/SprzętElektroniczny2.txt"
text <- paste(readLines(file, encoding = "UTF-8"), collapse = " ")
rk <- slowrake(txt = text, stem=FALSE, stop_pos = NULL, stop_words = stoplist)
print(rk[[1]])

file <- "C:/Users/micha/Desktop/NLP_projekt/projekt/14_Dokumenty/teksty/SprzętElektroniczny3.txt"
text <- paste(readLines(file, encoding = "UTF-8"), collapse = " ")
rk <- slowrake(txt = text, stem=FALSE, stop_pos = NULL, stop_words = stoplist)
print(rk[[1]])
file <- "C:/Users/micha/Desktop/NLP_projekt/projekt/14_Dokumenty/teksty/SprzętElektroniczny4.txt"
text <- paste(readLines(file, encoding = "UTF-8"), collapse = " ")
rk <- slowrake(txt = text, stem=FALSE, stop_pos = NULL, stop_words = stoplist)
print(rk[[1]])

file <- "C:/Users/micha/Desktop/NLP_projekt/projekt/14_Dokumenty/teksty/Szachy1.txt"
text <- paste(readLines(file, encoding = "UTF-8"), collapse = " ")
rk <- slowrake(txt = text, stem=FALSE, stop_pos = NULL, stop_words = stoplist)
print(rk[[1]])

file <- "C:/Users/micha/Desktop/NLP_projekt/projekt/14_Dokumenty/teksty/Szachy2.txt"
text <- paste(readLines(file, encoding = "UTF-8"), collapse = " ")
rk <- slowrake(txt = text, stem=FALSE, stop_pos = NULL, stop_words = stoplist)
print(rk[[1]])

file <- "C:/Users/micha/Desktop/NLP_projekt/projekt/14_Dokumenty/teksty/Szachy3.txt"
text <- paste(readLines(file, encoding = "UTF-8"), collapse = " ")
rk <- slowrake(txt = text, stem=FALSE, stop_pos = NULL, stop_words = stoplist)
print(rk[[1]])

file <- "C:/Users/micha/Desktop/NLP_projekt/projekt/14_Dokumenty/teksty/Szachy4.txt"
text <- paste(readLines(file, encoding = "UTF-8"), collapse = " ")
rk <- slowrake(txt = text, stem=FALSE, stop_pos = NULL, stop_words = stoplist)
print(rk[[1]])




