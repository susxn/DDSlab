library(dplyr)
library(tm)
library(caret)
library(ggplot2)

# Read XML document
raw.file = "../../data/qualys/latest.qkdb.xml.zip"
doc <- xml2::read_xml(raw.file)

# Extract QID, SEVERITY_LEVEL and DIAGNOSIS
kdb_txt <- rvest::html_text(rvest::html_elements(doc, xpath="//VULN[DIAGNOSIS]/*[self::QID or self::SEVERITY_LEVEL or self::DIAGNOSIS]"))
kdb_txt <- matrix(kdb_txt, nrow = length(kdb_txt)/3, ncol = 3, byrow = TRUE)
kdb_txt <- as.data.frame.matrix(kdb_txt)
names(kdb_txt) <- c("qid", "severity", "diagnosis")

# Tidy data frame
kdb_txt$qid <- as.integer(kdb_txt$qid)
kdb_txt$severity <- as.integer(kdb_txt$severity)
kdb_txt$diagnosis <- textclean::replace_html(kdb_txt$diagnosis)
kdb_txt$critical <- ifelse(test = kdb_txt$severity < 5, yes = "NO", no = "YES")
kdb_txt$criticalb <- kdb_txt$severity == 5

# Text analysis
## Stopwords
freq_word <- sort(table(unlist(strsplit(kdb_txt$diagnosis, " "))), decreasing = TRUE)
kdb_words <- names(freq_word)[(which(!(names(freq_word) %in% stopwords::stopwords())))]
## Characters
freq_char <- sort(table(unlist(strsplit(kdb_txt$diagnosis, ""))), decreasing = TRUE)

kdb_txt$descr <- textclean::replace_symbol(kdb_txt$diagnosis)
freq_char2 <- sort(table(unlist(strsplit(kdb_txt$descr, ""))), decreasing = TRUE)
freq_char2

# Prepare data for training
kdb_critical <- kdb_txt %>% filter(critical == "YES")
kdb_other <- kdb_txt %>% filter(critical == "NO")

total <- 2000
porcentajes <- c(0.05, 0.1, 0.15, 0.20, 0.25)
accuracies <- numeric()
sensitivities <- numeric()
Pos_Pred_Values <- numeric()

for (x in porcentajes){
 
  print(x*total)
  print((1-x)*total)
  
  kdb_ml <- bind_rows(kdb_critical %>% sample_n(x*total),
                      kdb_other %>% sample_n((1-x)*total)) %>%
    sample_n(total) %>%
    select(descr, critical)
  
  
  #*******************************************************************
  #                         Classification
  #*******************************************************************
  # install.packages("tm")
  #-------------------------------------------------------------------
  #                  4.2.: Preparing data for Classification
  #-------------------------------------------------------------------
  #Load up the corpus
  # course_raw = scan("data/Course-Descriptions.txt", what="", sep="\n")
  course_raw <- kdb_ml$descr
  course_corpus <- VCorpus(VectorSource(course_raw))
  inspect(course_corpus[[1]])
  #Convert to lower case
  course_corpus2 <- tm_map(course_corpus, content_transformer(tolower))
  #Remove punctuations
  course_corpus3 <- tm_map(course_corpus2, removePunctuation)
  #Remove stopwords
  course_corpus4 <- tm_map(course_corpus3, removeWords, stopwords())
  
  #Creamos una función para reemplazar palabras con Expresion Regular
  remplazo_palabras_re <- function(x,expresion_regular, palabra_reemplazar){
    gsub(expresion_regular,palabra_reemplazar,x, perl = TRUE)
  }
  
  #No interesa un CVE especifico sino simplemente que esta asociado a un CVE.
  course_corpus5 <- tm_map(course_corpus4, content_transformer(remplazo_palabras_re),expresion_regular="\\bcve\\w*\\b",palabra_reemplazar="cve_id")
  
  
  #Generate TF-IDF matrix
  course_dtm <- DocumentTermMatrix(course_corpus4)
  
  findFreqTerms(course_dtm,5)
  #Remove terms not in 90% of the documents. Only have those that are there
  #in atleast 2 documents
  dense_course_dtm <- removeSparseTerms(course_dtm, .85)
  #Inspect to TF-IDF
  
  #Convert continuous values to classes = { Yes, No }
  conv_counts <- function(x) {
    x <- ifelse(x > 0, 1, 0)
    x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
  }
  class_dtm <- apply(dense_course_dtm, MARGIN = 2, conv_counts)
  
  #-------------------------------------------------------------------
  #                  4.3.: Building the model
  #-------------------------------------------------------------------
  #Load the classifications for the descriptions
  # course_classes = scan("data/Course-Classification.txt", what="", sep="\n")
  course_classes <- kdb_ml$critical
  #install.packages("caret")
  #Random split of training and testing sets
  train_set <- createDataPartition(y=course_classes, p=.7,list=FALSE)
  #spliting the dtm
  train_dtm <- class_dtm[train_set,]
  test_dtm <-class_dtm[-train_set,]
  #split the course_classes
  train_classes <- course_classes[train_set]
  test_classes <- course_classes[-train_set]
  #train the model using naive bayes
  course_model <- train( data.frame(train_dtm), train_classes, method="nb")
  
  #-------------------------------------------------------------------
  #                  4.3.: Predictions for Text
  #-------------------------------------------------------------------
  #Predict for the test data
  course_predictions <- predict(course_model,test_dtm)
  
  # Guardar la salida de la función confusionMatrix() en una variable
  cm <- confusionMatrix(table(course_predictions, test_classes))
  
  # Extraer los valores de la matriz de confusión
  # Guardar la salida de la función confusionMatrix() en una variable
  
  
  #-------------------------------------------------------------------
  accuracies <- c(accuracies, cm$overall["Accuracy"])
  sensitivities <- c(sensitivities, cm$byClass["Sensitivity"])
  Pos_Pred_Values <- c(Pos_Pred_Values, cm$byClass["Pos Pred Value"])
}

# Crea un dataframe con los datos
df <- data.frame(porcentajes = porcentajes, y1 = accuracies, y2=sensitivities, y3=Pos_Pred_Values)

# Crea el gráfico con ggplot2
ggplot(df, aes(x = porcentajes, y1 = y1, y2 = y2, y3= y3)) +
  geom_line(aes(y = y1, col = "Accuracies")) +
  geom_line(aes(y = y2, col = "Sensitivities")) +
  geom_line(aes(y = y3, col = "% True Positive")) +
  scale_color_manual(values = c("blue", "red", "green"), name = "Legend") +
  labs(title = paste("Evaluación Accuracy/Sensitivity/True Positive,  vs Porcentaje muestras criticas. N=", total))
name <- paste("plots3variables/myplot", total, ".png", sep = "")
ggsave(name, width = 7, height = 5, dpi = 300)
