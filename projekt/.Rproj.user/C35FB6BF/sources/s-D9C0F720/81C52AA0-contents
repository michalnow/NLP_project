#load libraries
library(tm)
library(hunspell)
library(stringr)
library(wordcloud)
library(corrplot)
library(dendextend)
library(proxy)
library(flexclust)
library(topicmodels)
library(lsa)


#change work dir
workDir <- "C:\\Users\\micha\\Desktop\\NLP_projekt\\projekt"
setwd(workDir)

#definicja lokalizacji katalogóW funkcjonalnych
inputDir <- ".\\14_Dokumenty"
outputDir <- ".\\results"
workspaceDir <- ".\\workspaces"

#utworzenie katalogów wyjściowych
dir.create(outputDir, showWarnings = F)
dir.create(workspaceDir, showWarnings = F)

#utworzenie katalogów wyjściowych
dir.create(outputDir, showWarnings = F)
dir.create(workspaceDir, showWarnings = F)

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
    language = "pl_PL "
  )
)

corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))

stoplistFile <- paste(
  inputDir,
  "stopwords_pl.txt",
  sep ="\\"
)

stoplist <- readLines(stoplistFile, encoding = "UTF-8")
corpus <- tm_map(corpus, removeWords, stoplist)
corpus <- tm_map(corpus, stripWhitespace)

#remove em dash and 3/4
removeChar <- function(x, char) gsub(char, "", x)
corpus <- tm_map(corpus, content_transformer(removeChar), intToUtf8(8722))
corpus <- tm_map(corpus, content_transformer(removeChar), intToUtf8(190))

#remove paragraphs division in doc
pasteParagraphs <-content_transformer(function(x, char) paste(x, collapse = char))
corpus <- tm_map(corpus, pasteParagraphs, " ")

#remove extension from file names
cutExtensions <- function(document) {
  meta(document, "id") <- gsub("\\.txt$", "", meta(document, "id"))
  
  return(document)
}

corpus <- tm_map(corpus, cutExtensions)

#lemmatization
polish <- dictionary("pl_PL")

lemmatize <- content_transformer(
  function(text){
    simpleText <- str_trim(as.character(text))
    VectorizedText <- hunspell_parse(simpleText, dict = polish)
    lemmatizedText <- hunspell_stem(VectorizedText[[1]], dict = polish)
    for(i in 1:length(lemmatizedText)){
      if(length(lemmatizedText[[i]]) == 0) lemmatizedText[i] <- VectorizedText[[1]][i]
      if(length(lemmatizedText[[i]]) > 1) lemmatizedText[i] <- lemmatizedText[[i]][1]
    }
    newText <- paste(lemmatizedText, collapse = " ")
    return(newText)
  }
)

corpus <- tm_map(corpus, lemmatize)

#export doc corpus
preProcessedDir <- paste(
  inputDir,
  "14_Dokumenty_przetworzone",
  sep = "\\"
)
dir.create(preProcessedDir, showWarnings =  F)
writeCorpus(corpus, path = preProcessedDir)

# //////////////////////////// MACIERZE CZĘŚTOŚCI ///////////////////////////////////


#utworzenie korpusu dokumentów
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
    language = "pl_PL "
  )
)

#usunięcie z nazw dokumentóW rozszerzeń
cutExtensions <- function(document){
  meta(document, "id") <- gsub(
    "\\.txt$",
    replacement = "",
    meta(document, "id") 
  )
  return(document)
}
corpus <- tm_map(corpus, cutExtensions)

#tworzenie macierzy częstości
tdmTfAll <-TermDocumentMatrix(corpus)
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
      global = c(2,16)
    )
  )
)

tdmTfIdfBounds <-
  TermDocumentMatrix(
    corpus,
    control = list(
      weighting = weightSMART,
      bounds = list(
        global = c(2,16)
      )
    )
  )

dtmTfAll <- DocumentTermMatrix(corpus)
dtmTfIdfAll <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = weightTf
  )
)

dtmTfBounds <- DocumentTermMatrix(
  corpus,
  control = list(
    bounds = list(
      global = c(2,16)
    )
  )
)

dtmTfIdfBounds <-
  DocumentTermMatrix(
    corpus,
    control = list(
      weighting = weightTfIdf,
      bounds = list(
        global = c(2,16)
      )
    )
  )

dtmBinAll <- TermDocumentMatrix(corpus)
dtmBinAll <- TermDocumentMatrix(corpus, control = list(weighting = weightTfIdf))

#konwersje macierzy rzadkich do macierzy klasycznych
tdmTfAllMatrix <- as.matrix(tdmTfAll)
tdmTfIdfAllMatrix <- as.matrix(tdmTfIdfAll)
tdmTfBoundsMatrix <- as.matrix(tdmTfBounds)
tdmTfIdfBoundsMatrix <- as.matrix(tdmTfIdfBounds)
dtmTfAllMatrix <- as.matrix(dtmTfAll)
dtmTfIdfAllMatrix <- as.matrix(dtmTfIdfAll)
dtmTfBoundsMatrix <- as.matrix(dtmTfBounds)
dtmTfIdfBoundsMatrix <- as.matrix(dtmTfIdfBounds)
dtmBinAllMAtrix <- as.matrix(dtmBinAll)


#eksport macierzy tdmTfAllMatrix częstości do pliku
matrixFile <- paste(
  outputDir,
  "tdmTfAllMatrix.csv",
  sep = "\\"
)

write.table(
  tdmTfAllMatrix,
  file = matrixFile, 
  sep = ";",
  dec =",", 
  col.names = NA)

#eksport macierzy dtmTfAllMatrix częstości do pliku
matrixFile <- paste(
  outputDir,
  "dtmTfAllMatrix.csv",
  sep = "\\"
)

write.table(
  dtmTfAllMatrix,
  file = matrixFile, 
  sep = ";",
  dec =",", 
  col.names = NA)

#eksport macierzy dtmTfIdfBoundsMatrix częstości do pliku
matrixFile <- paste(
  outputDir,
  "dtmTfIdfBoundsMatrix.csv",
  sep = "\\"
)

write.table(
  dtmTfIdfBoundsMatrix,
  file = matrixFile, 
  sep = ";",
  dec =",", 
  col.names = NA)

#eksport macierzy tdmTfIdfBoundsMatrix częstości do pliku
matrixFile <- paste(
  outputDir,
  "tdmTfIdfBoundsMatrix.csv",
  sep = "\\"
)

write.table(
  tdmTfIdfBoundsMatrix,
  file = matrixFile, 
  sep = ";",
  dec =",", 
  col.names = NA)

#////////////////////////////////////// ANALIZA SKUPIEŃ /////////////////////////////////////

#analiza skupień

##metoda hierarchiczna
#parametry metody:
# 1. macierz częstości
# a. waga (weighting)
# b. zakres wykorzystanych zmiennych (bounds)
# 2. miara odległości (euclidean, jaccard, cosine)
# 3. metoda wyznaczania odległości skupień (single, complete, ward. D2)

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

#eksperyment 1
rownames(dtmTfAllMatrix) <- paste("d",1:20, sep = "")
distMatrix1 <- dist(dtmTfAllMatrix, method = "euclidean")
hclust1 <- hclust(distMatrix1, method = "complete")
plot(hclust1)
barplot(hclust1$height, names.arg = 19:1)
nClusters1 <- 3
clusters1 <- cutree(hclust1, k = nClusters1)
clustersMatrix1 <- matrix(0, nDocuments, nClusters1)
rownames(clustersMatrix1) <- docNames
for (i in 1:nDocuments) {
  clustersMatrix1[i, clusters1[i]] <- 1
}
corrplot(clustersMatrix1)
dendrogram1 <- as.dendrogram(hclust1)
coloredDendrogram1 <- color_branches(dendrogram1, h = 100)
plot(coloredDendrogram1)
find_k(dendrogram1)
#coloredDendrogram1 <- color_branches(dendrogram1, col = colorsPattern)
#plot(coloredDendrogram1)

#eksperyment 2
rownames(dtmTfIdfBoundsMatrix) <- paste("d",1:20, sep = "")
distMatrix2 <- dist(dtmTfIdfBoundsMatrix, method = "cosine")
hclust2 <- hclust(distMatrix2, method = "ward.D2")
plot(hclust2)
barplot(hclust2$height, names.arg = 19:1)
nClusters2 <- 3
clusters2 <- cutree(hclust2, k = nClusters2)
clustersMatrix2 <- matrix(0, nDocuments, nClusters2)
rownames(clustersMatrix2) <- docNames
for (i in 1:nDocuments) {
  clustersMatrix2[i, clusters2[i]] <- 1
}
corrplot(clustersMatrix2)
dendrogram2 <- as.dendrogram(hclust2)
coloredDendrogram2 <- color_branches(dendrogram2, h = 1.3)
plot(coloredDendrogram2)
find_k(dendrogram2)


#porównanie wyników eksperymentów
Bk_plot(
  dendrogram1,
  dendrogram2,
  add_E = F,
  rejection_line_asymptotic = F,
  main = "Fawlkes-Mallows Index",
  ylab = "Fawlkes-Mallows Index"
)

#Bk_plot(
#  dendrogram1,
#  dendrogram3,
#  add_E = F,
#  rejection_line_asymptotic = F,
#  main = "Fawlkes-Mallows Index",
#  ylab = "Fawlkes-Mallows Index"
#)

#Bk_plot(
#  dendrogram2,
#  dendrogram3,  add_E = F,
#  rejection_line_asymptotic = F,
#  main = "Fawlkes-Mallows Index",
#  ylab = "Fawlkes-Mallows Index"
#)

randEx1Ex2 <- comPart(clusters1, clusters2)
#randEx1Ex3 <- comPart(clusters1, clusters3)
#randEx2Ex3 <- comPart(clusters2, clusters3)
randEx1Pattern <- comPart(clusters1, clusterPattern)
randEx2Pattern <- comPart(clusters2, clusterPattern)
#randEx3Pattern <- comPart(clusters3, clusterPattern)

##metoda niehierarchiczna (k-średnich)

#eksperyment 4
nClusters4 <- 3
kmeans4 <- kmeans(dtmTfBounds, centers = nClusters4)
clusters4 <- kmeans4$cluster
clustersMatrix4 <- matrix(0, nDocuments, nClusters2)
rownames(clustersMatrix4) <- docNames
for (i in 1:nDocuments) {
  clustersMatrix4[i, clusters4[i]] <- 1
}
corrplot(clustersMatrix4)

randEx4Pattern <- comPart(clusters4, clusterPattern)
randEx1Ex4 <- comPart(clusters1, clusters4)
randEx2Ex4 <- comPart(clusters2, clusters4)
#randEx3Ex4 <- comPart(clusters3, clusters4)

# ////////////////////////////////////// Analiza alokacji Dirchlet'a /////////////////////////////////////////////

nTopics <- 4
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

#reprezentacja tematów
topic1 <- head(sort(results$terms[1,],decreasing = T), 20)
barplot(
  rev(topic1),
  horiz = T,
  las = 1,
  main = "Temat 1",
  xlab = "Prawdopodobieństwo",
  col = colors[1]
)

topic2 <-head(sort(results$terms[2,],decreasing = T), 20)
barplot(
  rev(topic2),
  horiz = T,
  las = 1,
  main = "Temat 2",
  xlab = "Prawdopodobieństwo",
  col = colors[2]
)

topic3 <- head(sort(results$terms[3,],decreasing = T), 20)
barplot(
  rev(topic3),
  horiz = T,
  las = 1,
  main = "Temat 3",
  xlab = "Prawdopodobieństwo",
  col = colors[3]
)

topic4 <-head(sort(results$terms[4,],decreasing = T), 20)
barplot(
  rev(topic4),
  horiz = T,
  las = 1,
  main = "Temat 4",
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

#prezentacja dokumentów
document4 <- results$topics[4,]
barplot(
  document4,
  horiz = T,
  las = 1,
  main = rownames(results$topics)[],
  xlab = "Prawdopodobieństwo",
  col = colors
)

#prezentacja dokumentów
document11 <- results$topics[11,]
barplot(
  document11,
  horiz = T,
  las = 1,
  main = rownames(results$topics)[11],
  xlab = "Prawdopodobieństwo",
  col = colors
)


#prezentacja dokumentów
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

# ////////////////////////// SŁOWA KLUCZOWE ////////////////////////////////////

keywordsTf1 <- head(sort(dtmTfAllMatrix[1,], decreasing = T))
keywordsTf1

#prawdopodobienstwo w LDA jako miara waznosci slow
termsImportance <- c(results$topics[1,]%*%results$terms)
names(termsImportance) <- colnames(results$terms)
keywordsTf1 <- head(sort(termsImportance, decreasing = T))
keywordsTf1

#chmury tagow

par(mai = c(0,0,0,0))
wordcloud(corpus[1], max.words = 200, color = brewer.pal(8,"PuOr"))

# ///////////////////////////// ANALIZA GŁÓWNYCH SKŁĄDOWYCH ///////////////////////////
pca <- prcomp(dtmTfIdfBounds)

#przygotowanie danych do wykresu
x <- pca$x[,1]
y <- pca$y[,2]
legend <- paste(
  paste("d", 1:20, sep =""),
  rownames(dtmTfIdfBounds),
  sep =" - "
)

#export do png
plotFile <- paste(
  outputDir,
  "pca.png",
  sep = "\\"
)

png(filename =  plotFile)

#wykres dokumentów w przestrzeni 2d
options(scipen = 5)
plot(
  x,
  y,
  main = "Analiza głównych składowych",
  xlab = "PC1",
  ylab = "PC2",
  col = "purple4",
  xlim = c(-10,10),
  ylim = c(-10,10),
  pch = 18,
)


text(
  x,
  y,
  paste("d", 1:20, sep =""),
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

#zamkniecie pliku png
dev.off()


# /////////////////////////////////// Dekompozycja według wartości osobliwych ///////////////////////

#analiza ukrytych wymiarów semantycznych
#dekompyuzcja (rozkład) wg wartosci osobliwych
lsa <- lsa(tdmTfBoundsMatrix)

#przygootowanie danych do wykresu
coordDocs <- lsa$dk%*%diag(lsa$sk)
coordTerms <- lsa$tk%*%diag(lsa$sk)

termsImportance <- diag(lsa$tk%*%diag(lsa$sk)%*%t(diag(lsa$sk))%*%t(lsa$tk))
importantTerms <-names(tail(sort(termsImportance), 30))
coorsImportanceTerms <- coordTerms[importantTerms,]

ownTerms <- c("dieta", "szachy", "muzyka", "dźwięk")
coordOwnTerms <- coordTerms[ownTerms,]

legend <- paste(
  paste("d", 1:20, sep =""),
  rownames(coordDocs),
  sep =" - "
)

#przygotowanie danych do wykresu
x1 <- coordDocs[,1]
y1 <- coordDocs[,2]
#x2 <- coorsImportanceTerms[,1]
#y2 <- coorsImportanceTerms[,2]
x2 <- coordOwnTerms[,1]
y2 <- coordOwnTerms[,2]


#export do png
plotFile <- paste(
  outputDir,
  "lsa.png",
  sep = "\\"
)

png(filename =  plotFile)

#wykres dokumentów w przestrzeni 2d
options(scipen = 5)
plot(
  x1,
  y1,
  main = "Analiza ukrytych wymiarów semantycznych",
  xlab = "SD1",
  ylab = "SD2",
  col = "purple4",
  #xlim = c(-0.005,0.005),
  #ylim = c(0.3,0.055),
  pch = 18,
)


text(
  x,
  y,
  paste("d", 1:20, sep =""),
  col = "purple4",
  pos = 4,
  cex = 0.8
)

points(
  x2,
  y2,
  col = "violetred4",
  pch = 15
)

text(
  x,
  y,
  rownames(coordOwnTerms),
  col = "purple4",
  pos = 4,
  cex = 0.8
)



legend(
  "topleft",
  legend,
  cex = 0.6,
  text.col = "purple4"
)


#zamkniecie pliku png
dev.off()



