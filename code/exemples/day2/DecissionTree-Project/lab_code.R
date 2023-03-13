#downloadDataset <- function(url, zip = FALSE) {
#  filename <- "tmp.zip"
#  download.file(url, filename)
#  if (zip)
#    unzip(zipfile = filename)
#}

#require(rpart)
#require(rpart.plot)

loadCSVDataSet <- function(filename) {
  ds <- read.csv(file=filename, header=TRUE, sep=",")
  return(ds)
}

selectAttributes <- function(df, attributes_list) {
  df <- df[,attributes_list ]
  return(df)
}

removeFuzzingSamples <- function(df) {
  df <- df[!(df$attack_cat == "Fuzzers"),]
  return(df)
}

binaryClassification <- function(df) {
  df$attack_cat <- as.character(df$attack_cat)
  df$attack_cat[df$attack_cat != "Normal"] <- "Attack"
  df$attack_cat <- as.factor(df$attack_cat)
  return(df)
}

generateAndPlotBinaryDecissionTree <- function(df) {
  fit <- rpart(attack_cat~., data = df, method = 'class')
  #rpart.plot(fit, extra = 106)
  return(fit)
}

generateAndPlotMulticlassBinaryDecissionTree <- function(df) {
  fit <- rpart(attack_cat~., data = df, method = 'class')
  #rpart.plot(fit, extra = 104)
  return(fit)
}

calculatePrecission <- function(fit, df) {
  predict_unseen <- predict(fit, df, type = 'class')
  t <- table(predict_unseen, df$attack_cat)
  results <- as.data.frame.matrix(t)
  results$attack_percentage <- (results$Attack * 100) / sum(results$Attack) 
  results$normal_percentage <- (results$Normal * 100) / sum(results$Normal)
results$false_neg <- results$attack_percentage[2]
  results$false_pos <- results$normal_percentage[1]
  return(results)
}

calculateMulticlassPrecission <- function(fit, df) {
  predict_unseen <- predict(fit, df, type = 'class')
  t <- table(predict_unseen, df$attack_cat)
  results <- as.data.frame.matrix(t)
  tmp_a <- 
  results$Exploits <- (results$Exploits * 100) / sum(results$Exploits) 
  results$Generic <- (results$Generic * 100) / sum(results$Generic)
  results$Normal <- (results$Normal * 100) / sum(results$Normal) 
  results$Reconnaissance <- (results$Reconnaissance * 100) / sum(results$Reconnaissance)
  #results$false_neg <- ((results$Exploits[3] + results$Generic[3] + results$Reconnaissance[3])*100)/nrow(df)
  #results$false_pos <- results$normal_percentage[1] + results$normal_percentage[2] + results$normal_percentage[4]
  return(results)
}

calculateMulticlassPrecissionUn <- function(fit, df) {
  predict_unseen <- predict(fit, df, type = 'class')
  t <- table(predict_unseen, df$attack_cat)
  results <- as.data.frame.matrix(t)
  results$Analysis <- (results$Analysis * 100) / sum(results$Analysis) 
  results$Backdoor <- (results$Backdoor * 100) / sum(results$Backdoor)
  results$DoS <- (results$DoS * 100) / sum(results$DoS) 
  results$Exploits <- (results$Exploits * 100) / sum(results$Exploits) 
  results$Fuzzers <- (results$Fuzzers * 100) / sum(results$Fuzzers)
  results$Generic <- (results$Generic * 100) / sum(results$Generic)
  results$Normal <- (results$Normal * 100) / sum(results$Normal)
  results$Reconnaissance <- (results$Reconnaissance * 100) / sum(results$Reconnaissance)
  results$Shellcode <- (results$Shellcode * 100) / sum(results$Shellcode) 
  results$Worms <- (results$Worms * 100) / sum(results$Worms)
  return(results)
}
joinClasses <- function(df, attributes_list, multiclass_name) {
  df$attack_cat <- as.character(df$attack_cat)
  #data_test$attack_cat[data_test$attack_cat %in% c("Analysis", "Backdoor", "DoS", "Exploits")] <- "EXPL/BD/FZR/DoS"
  df$attack_cat[df$attack_cat %in% attributes_list] <- multiclass_name
  #Si en exploits mas precision un poco mas de falsos neg -> Generic
  df$attack_cat <- as.factor(df$attack_cat)
  return(df)
}

processDataMulticlass <- function(df) {
  attr_list = c(8, 28, 13, 9, 29, 10, 36, 2, 33, 12, 37 ,11, 35, 7, 14, 34, 44)
  #, 12, 14, 15, 16, 17, 18, 19, 20, 21,
  df <- selectAttributes(df, attr_list)
  df <- removeFuzzingSamples(df)
  df <- joinClasses(df,c("Shellcode", "Worms", "Analysis", "Backdoor", "DoS"), "Exploits")
  return(df)
}
processData <- function(df) {
  attr_list = c(8, 28, 13, 9, 29, 10, 36, 2, 33, 12, 37 ,11, 35, 7, 14, 34, 44)
  df <- selectAttributes(df, attr_list)
  df <- removeFuzzingSamples(df)
  df <- binaryClassification(df)
  return(df)
}

plotBinaryResults <- function(results) {
  slices <- c(results$attack_percentage[1], results$attack_percentage[2], results$normal_percentage[2], results$normal_percentage[1])
  label_ac <- paste("% Correctas Attack " ,as.character(results$attack_percentage[1]))
  #label_ac <- substr(label_ac, 1, nchar(label_ac)-21)
  label_ai <- paste("% Incorrectas Attack " ,as.character(results$attack_percentage[2]))
  #label_ai <- substr(label_ai, 1, nchar(label_ai)-21)
  label_nc <- paste("% Correctas Normal " ,as.character(results$normal_percentage[2]))
  #label_nc <- substr(label_nc, 1, nchar(label_nc)-21)
  label_ni <- paste("% Incorrectas Normal " ,as.character(results$normal_percentage[1]))
  #label_ni <- substr(label_ni, 1, nchar(label_ni)-21)
  lbls <- c(label_ac, label_ai, label_nc, label_ni)
  pie(slices, labels = lbls, main="Precision del modelo")
}

plotMulticlassResults <- function(results) {
  slices <- c(results$exploit_percentage[1], results$generic_percentage[2], results$normal_percentage[3], results$reconnaissance_percentage[4])
  label_ac <- paste("% Correctas Exploits " ,as.character(results$exploit_percentage[1]))
  #label_ac <- substr(label_ac, 1, nchar(label_ac)-21)
  label_ai <- paste("% Correctas Generic " ,as.character(results$generic_percentage[2]))
  #label_ai <- substr(label_ai, 1, nchar(label_ai)-21)
  label_nc <- paste("% Correctas Normal " ,as.character(results$normal_percentage[3]))
  #label_nc <- substr(label_nc, 1, nchar(label_nc)-21)
  label_ni <- paste("% Correctas Reconnaissance " ,as.character(results$reconnaissance_percentage[4]))
  #label_ni <- substr(label_ni, 1, nchar(label_ni)-21)
  lbls <- c(label_ac, label_ai, label_nc, label_ni)
  pie(slices, labels = lbls, main="Precision del modelo")
}

plotMulticlassResultsUn <- function(results) {
  slices <- c(results$analysis_percentage[1], results$backdoor_percentage[2], results$dos_percentage[3], results$exploit_percentage[4], results$fuzzers_percentage[5], results$generic_percentage[6], results$normal_percentage[7], results$reconnaissance_percentage[8], results$shellcode_percentage[9], results$worms_percentage[10])
  label_ac <- paste("% Analysis Analysis " ,as.character(results$analysis_percentage[1]))
  #label_ac <- substr(label_ac, 1, nchar(label_ac)-21)
  label_ai <- paste("% Correctas Backdoor " ,as.character(results$backdoor_percentage[2]))
  #label_ai <- substr(label_ai, 1, nchar(label_ai)-21)
  label_nc <- paste("% Correctas DoS " ,as.character(results$dos_percentage[3]))
  #label_nc <- substr(label_nc, 1, nchar(label_nc)-21)
  label_ni <- paste("% Correctas Exploits " ,as.character(results$exploits_percentage[4]))
  label_ac1 <- paste("% Correctas Fuzzers " ,as.character(results$fuzzers_percentage[5]))
  #label_ac <- substr(label_ac, 1, nchar(label_ac)-21)
  label_ai1 <- paste("% Correctas Generic " ,as.character(results$generic_percentage[6]))
  #label_ai <- substr(label_ai, 1, nchar(label_ai)-21)
  label_nc1 <- paste("% Correctas Normal " ,as.character(results$normal_percentage[7]))
  #label_nc <- substr(label_nc, 1, nchar(label_nc)-21)
  label_ni1 <- paste("% Correctas Reconnaissance " ,as.character(results$reconnaissance_percentage[8]))
  label_nc2 <- paste("% Correctas Shellcode " ,as.character(results$shellcode_percentage[9]))
  #label_nc <- substr(label_nc, 1, nchar(label_nc)-21)
  label_ni2 <- paste("% Correctas Worms " ,as.character(results$worms_percentage[10]))
  #label_ni <- substr(label_ni, 1, nchar(label_ni)-21)
  lbls <- c(label_ac, label_ai, label_nc, label_ni, label_ac1, label_ai1, label_nc1, label_ni1, label_nc2, label_ni2)
  pie(slices, labels = lbls, main="Precision del modelo")
}

plotSamplesDistribution <- function(data, colors_list) {
  barplot(height = table(data), col = colors_list,
          main = "Distribucion de las muestras")
}

plotResults <- function(df) {
  
  #Arrange them as you want with grid.arrange
  grid.table(df)
}

plotResultsOfUnModDataSet <- function(data_train, data_test) {
  #LOAD TRAINING DATASET
  data_train_unmodified <- data_train
  #print("Trainning set Attack Category Samples")
  #print(table(data_train_unmodified$attack_cat))
  #print("Total Samples")
  #print(nrow(data_train_unmodified))
  #CLASSIFY ATTACKS
  data_train_unmodified <- binaryClassification(data_train_unmodified)
  #print("Samples Processed and binary classified distribution")
  #table(data_train_unmodified$attack_cat)
  #CLEAN DATA SET FROM PROBLEMATIC ATTRIBUTES
  data_train_unmodified <- data_train_unmodified[,-c(3, 5, 45)]
  plotSamplesDistribution(data_train_unmodified$attack_cat, c("red","Blue"))
  #GENERATE DECISSION TREES AND PLOT RESULTS
  fit_data_unmod <- generateAndPlotBinaryDecissionTree(data_train_unmodified)
  #LOAD TESTING DATA SET
  data_test_unmodified <- data_test
  #CLASSIFY BY ATTACK/NORMAL
  data_test_unmodified <- binaryClassification(data_test_unmodified)
  data_test_unmodified <- data_test_unmodified[,-c(3, 5, 45)]
  table(data_test_unmodified$attack_cat)
  #plotSamplesDistribution(data_test_unmodified$attack_cat, c("red","Blue"))
  #TEST THE MODEL BY CLASSIFYNG THE SAMPLES OF THE TESTING DATA SET
  res_unmod <- calculatePrecission(fit_data_unmod, data_test_unmodified)
  #plotBinaryResults(res_unmod)
  return(res_unmod)
}


plotResultsOfModDataSet <- function(data_train, data_test) {
  #LOAD TRAINING DATASET
  data_train_modified = data_train

  #DELETE UNWANTED ATTRIBUTES, SAMPLES AND CLASSIFY BY ATTACK/NORMAL
  data_train_modified <- processData(data_train_modified)
  #CLEAN DATA SET FROM PROBLEMATIC ATTRIBUTES
  data_train_modified <- data_train_modified[,-c(3, 5, 45)]
  #GENERATE DECISSION TREES AND PLOT RESULTS
  fit_data_mod <- generateAndPlotBinaryDecissionTree(data_train_modified)
  #LOAD TESTING DATA SET
  data_test_modified <- data_test
  #DELETE UNWANTED ATTRIBUTES, SAMPLES AND CLASSIFY BY ATTACK/NORMAL
  data_test_modified <- processData(data_test_modified)
  data_test_modified <- data_test_modified[,-c(3, 5, 45)]
  #TEST THE MODEL BY CLASSIFYNG THE SAMPLES OF THE TESTING DATA SET
  res_mod <- calculatePrecission(fit_data_mod, data_test_modified)
  #plotBinaryResults(res_mod)
  return(res_mod)
}

plotResultsWithMultiClass <- function(data_train, data_test) {
  data_train_modified = data_train
  #DELETE UNWANTED ATTRIBUTES, SAMPLES AND CLASSIFY BY ATTACK/NORMAL
  data_train_modified <- processDataMulticlass(data_train_modified)
  plotSamplesDistribution(data_train_modified$attack_cat, c("red","red","blue","red"))
  #CLEAN DATA SET FROM PROBLEMATIC ATTRIBUTES
  data_train_modified <- data_train_modified[,-c(3, 5, 45)]
  #GENERATE DECISSION TREES AND PLOT RESULTS
  fit_data_mod <- generateAndPlotMulticlassBinaryDecissionTree(data_train_modified)
  #LOAD TESTING DATA SET
  data_test_modified <- data_test
  #DELETE UNWANTED ATTRIBUTES, SAMPLES AND CLASSIFY BY ATTACK/NORMAL
  data_test_modified <- processDataMulticlass(data_test_modified)
  data_test_modified <- data_test_modified[,-c(3, 5, 45)]
  #TEST THE MODEL BY CLASSIFYNG THE SAMPLES OF THE TESTING DATA SET
  res_mod <- calculateMulticlassPrecission(fit_data_mod, data_test_modified)
  #plotMulticlassResults(res_mod)
  return(res_mod)
}
plotResultsWithMultiClassUnmod <- function(data_train, data_test) {
  
  data_train_modified = data_train
  #print("Trainning set Attack Category Samples")
  #print(table(data_train_modified$attack_cat))
  #plotSamplesDistribution(data_train_modified$attack_cat, c("red","Blue","red","Blue","red","Blue","red","Blue","red"))
  #print("Total Samples")
  #print(nrow(data_train_modified))
  #CLEAN DATA SET FROM PROBLEMATIC ATTRIBUTES
  data_train_modified <- data_train_modified[,-c(3, 5, 45)]
  #GENERATE DECISSION TREES AND PLOT RESULTS
  fit_data_mod <- generateAndPlotMulticlassBinaryDecissionTree(data_train_modified)
  #LOAD TESTING DATA SET
  data_test_modified <- data_test
  data_test_modified <- data_test_modified[,-c(3, 5, 45)]
  #print("Testing Samples Processed and multiclass classified distribution")
  #plot(table(data_test_modified$attack_cat))
  #TEST THE MODEL BY CLASSIFYNG THE SAMPLES OF THE TESTING DATA SET
  return(calculateMulticlassPrecissionUn(fit_data_mod, data_test_modified))
}



#LOAD TRAINING DATASET
#data_train = loadCSVDataSet("./UNSW_NB15_training-set.csv")
#LOAD TESTING DATA SET
#data_test <- loadCSVDataSet("./UNSW_NB15_testing-set.csv")
#plotResultsOfUnModDataSet(data_train, data_test)
#plotResultsOfModDataSet(data_train, data_test)
#plotResultsWithMultiClass(data_train, data_test)
#plotResultsWithMultiClassUnmod(data_train, data_test)


