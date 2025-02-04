library(tidyverse)
library(tidymodels)
library(randomForest)
library(caret)
library(car)
library(mlpack) #this changes the kmeans function substantially, double check help menu
library(rpart)
library(rpart.plot)
library(parsnip)
library(ROSE)
library(modelr)
library(pROC)
set.seed(24)
#This file is sensitive to altering to the order of columns in original data!

#I believe the diagnosis codes are ICD-9 codes
#https://www.hipaaspace.com/medical.coding.library/icd-9.codes/
#https://www.hipaaspace.com/medical_billing/coding/international_classification_of_diseases/icd9_codes_lookup.aspx

Train_Beneficiary <- read_csv("Train_Beneficiary.csv")
#recode factors to 0/1 instead of 1/2
Train_Beneficiary[4][Train_Beneficiary[4] == 2] <- 0
Train_Beneficiary[11:21][Train_Beneficiary[11:21] == 1] <- 0
Train_Beneficiary[11:21][Train_Beneficiary[11:21] == 2] <- 1
#change columns to factors
Train_Beneficiary <- Train_Beneficiary |> mutate(across(c(4:6,11:21), as_factor))



Train_Inpatient <- read_csv("Train_Inpatient.csv")
Train_Outpatient <- read_csv("Train_Outpatient.csv")

TrainY <- read_csv("TrainY.csv")
TrainY$PotentialFraud <- as.factor(TrainY$PotentialFraud)

#colSums(is.na(Train_Beneficiary)) #this is just to list how many nulls are in each row
#colSums(is.na(Train_Inpatient)) #print these so you can scribble lines all over
#colSums(is.na(Train_Outpatient))

#outpatient dataset
outpatient <- unite(Train_Outpatient, DiagnosisCodes, starts_with('ClmDiagnosis'), sep = ",", remove = TRUE, na.rm = TRUE)
outpatient <- outpatient |> unite(ProcedureCodes, starts_with('ClmProcedure'), sep = ",", remove = TRUE, na.rm = TRUE)
outpatient <- outpatient |> mutate(ProcCount = str_count(outpatient$ProcedureCodes, ",") + 1) #currently not being used
outpatient <- outpatient |> mutate(DiagCount = str_count(outpatient$DiagnosisCodes, ",") + 1)
outpatient <- outpatient |> mutate(ClaimTime = as.numeric(difftime(outpatient$ClaimEndDt,outpatient$ClaimStartDt,  units = "days")))
outpatient <- left_join(outpatient, TrainY, by = "Provider") |> select(c(c("BeneID", "Provider","InscClaimAmtReimbursed", "DeductibleAmtPaid", "DiagCount", "ClaimTime", "PotentialFraud")))

#inpatient
inpatient <- unite(Train_Inpatient, DiagnosisCodes, starts_with('ClmDiagnosis'), sep = ",", remove = TRUE, na.rm = TRUE)
inpatient <- inpatient |> unite(ProcedureCodes, starts_with('ClmProcedure'), sep = ",",remove = TRUE, na.rm = TRUE)
inpatient <- inpatient |> mutate(ProcCount = if_else(inpatient$ProcedureCodes == "",0, str_count(inpatient$ProcedureCodes, ",") + 1))
#DiagCount counts number of diagnosis
inpatient <- inpatient |> mutate(DiagCount = str_count(inpatient$DiagnosisCodes, ",") + 1)
inpatient <- inpatient |> mutate(ClaimTime = as.numeric(difftime(inpatient$ClaimEndDt,inpatient$ClaimStartDt,  units = "days")))
inpatient <- inpatient |> mutate(Stay = as.numeric(difftime(inpatient$DischargeDt, inpatient$AdmissionDt, units = "days")))
#editdown
inpatient <- left_join(inpatient, TrainY, by = "Provider") |> select(c(c("BeneID", "Provider", "InscClaimAmtReimbursed", "DeductibleAmtPaid", "DiagCount", "ProcCount", "ClaimTime", "PotentialFraud", "Stay")))

#https://www.r-bloggers.com/2021/05/class-imbalance-handling-imbalanced-data-in-r/
#fixing class imbalance with ROSE package


#provider level data
prozd <- TrainY #new dataset for provider level data

#counting unique patients for each doctor
inpatientcounts <- Train_Inpatient |> select("BeneID", "Provider") |> left_join(TrainY, by = "Provider")
outpatientcounts <- Train_Outpatient |> select("BeneID", "Provider") |> left_join(TrainY, by = "Provider")

inpatientcounts |> group_by(Provider) |> count(sort = TRUE)
inpatientcounts |> group_by(Provider) |> distinct()|> count(sort = TRUE)

#counting unique inpatients/outpatients per provider
inpatientcounts[inpatientcounts$PotentialFraud == "1",] |> group_by(Provider) |> distinct()|> count()
inpatientcounts[inpatientcounts$PotentialFraud == "0",] |> group_by(Provider) |> distinct()|> count()

outpatientcounts[outpatientcounts$PotentialFraud == "1",] |> group_by(Provider) |> distinct()|> count()
outpatientcounts[outpatientcounts$PotentialFraud == "0",] |> group_by(Provider) |> distinct()|> count()

#patient count per provider
dd1 <- inpatientcounts |> group_by(Provider) |> distinct() |> count() |> rename(inpatients = n) 
dd2 <- outpatientcounts |> group_by(Provider) |> distinct() |> count() |> rename(outpatients = n)

#average diagnosis
dd3 <- inpatient |> group_by(Provider) |> summarise(AvgDiagi = mean(DiagCount)) 
dd4 <- outpatient |> group_by(Provider) |> summarise(AvgDiago = mean(DiagCount))
#average claim time and Stay
dd5 <-  inpatient |> group_by(Provider) |> summarise(AvgClaimi = mean(ClaimTime))
dd6 <-  outpatient |> group_by(Provider) |> summarise(AvgClaimo = mean(ClaimTime))
dd7 <-  inpatient |> group_by(Provider) |> summarise(AvgStayi = mean(Stay))
#Money columns averaged " InscClaimAmtReimbursed " " DeductibleAmtPaid "
dd8 <- inpatient |> group_by(Provider) |> summarise(avgReimbursei = mean(InscClaimAmtReimbursed))
dd9 <- outpatient |> group_by(Provider) |> summarise(avgReimburseo = mean(InscClaimAmtReimbursed))
dd10 <- inpatient |> group_by(Provider) |> summarise(avgDeducti = mean(DeductibleAmtPaid)) #either 1068 or NA
dd11 <- outpatient |> group_by(Provider) |> summarise(avgDeducto = mean(DeductibleAmtPaid))#either 1068 or NA
#merge everything and clear useless objects
prozd <- prozd |> left_join(dd1, by = "Provider") |> left_join(dd2, by = "Provider") |> left_join(dd3, by = "Provider")|> left_join(dd4, by = "Provider")|> left_join(dd5, by = "Provider")|> left_join(dd6, by = "Provider")|> left_join(dd7, by = "Provider")|> left_join(dd8, by = "Provider")|> left_join(dd9, by = "Provider")
prozd <- prozd |> select(!Provider) #strip provider column away to simplify some model syntaxes
rm(dd1,dd2,dd3,dd4,dd5,dd6,dd7,dd8,dd9, dd10, dd11) #removes the stuff we just merged from working memory

table(complete.cases(prozd), prozd$PotentialFraud) #rows without NA's separated by frauds/nons
compzd <- prozd[complete.cases(prozd) == TRUE,]  #only complete cases



#playing with the data

#train/test split
splitter <- sample(2, nrow(prozd), replace = TRUE, prob = c(0.7, 0.3))
tacotrain <- prozd[splitter == 1,]
tacotest <- prozd[splitter == 2,]

#decisiontree THIS IS GOOD NOW
tree1 <- rpart(PotentialFraud ~., data = prozd) #full data
rpart.plot(tree1)

tree2 <- rpart(PotentialFraud ~., data = compzd) #complete cases only
rpart.plot(tree2)

#decision tree for data training split
tree3 <- rpart(PotentialFraud ~., data = tacotrain) #random sample
rpart.plot(tree3)

guesses <-  predict(tree3, tacotest, type = "class")
table(tacotrain$PotentialFraud) #class imbalance
confusionMatrix(guesses, tacotest$PotentialFraud, positive = "1", mode = "everything") #this model is good for predicting nonfraud (0), but not predicting fraud (1)

#balance the classes by oversampling
over <- ovun.sample(PotentialFraud~., data = tacotrain, N = 1860, method = "over", seed = 24)$data
table(over$PotentialFraud)
tree4 <- rpart(PotentialFraud~., data = over)
rpart.plot(tree4)
guesses2 <- predict(tree4, tacotest, type = "class")
confusionMatrix(guesses2, tacotest$PotentialFraud, positive = "1", mode = "everything")

rfover <- randomForest(PotentialFraud~., data = over)
confusionMatrix(predict(rfover, tacotest), tacotest$PotentialFraud, positive = "1", mode = "everything")

#logistic regression
logz <- glm(PotentialFraud ~ ., family = "binomial", data = prozd)
logz2 <- glm(PotentialFraud ~ ., family = "binomial", data = over)
logistical <- train(PotentialFraud ~ ., data = compzd, family = "binomial", method = "glm")
summary(logistical)

logis2 <- train(PotentialFraud~ outpatients + inpatients + avgReimbursei, data = over, family = "binomial", method = "glm")
summary(logis2) #logis2 and logis3 are the same model

logis3 <- glm(PotentialFraud~ outpatients + inpatients + avgReimbursei, family = binomial(link = "logit"), data = over)
fff <- predict(logis3, tacotrain$PotentialFraud, type = "response")
confusionMatrix(cut(fff, breaks = 2, labels = c("0", "1")), tacotrain$PotentialFraud, positive = "1")
#run option(na.rm = TRUE) if matrix doesn't run correctly

#experimenting with adding predictions to oversampled data and comparing model results
over2 <- over |> add_predictions(rfover, var = "rfPred") |> add_predictions(logis3, var = "lrPred")
over2$lrPred <- cut(over2$lrPred, breaks = 2, labels = c("0", "1")) #convert to factor
over2 <- over2 |> mutate(match = lrPred == rfPred)
table(over2$match, over2$PotentialFraud)

predz <- predict(logz, prozd, type = "response")
predz2 <- predict(logz2, over, type = "response")

cooper <- roc(prozd$PotentialFraud, predz, na.rm = TRUE)
booper <- roc(over$PotentialFraud, as.numeric(rfover$predicted), na.rm = TRUE)
dooper <- roc(over$PotentialFraud, predz2, na.rm = TRUE)
aooper <- roc(tacotrain$PotentialFraud,as.numeric(cut(fff, breaks = 2, labels = c("0", "1"))), na.rm = TRUE)
ggroc(cooper) #make this but with all the different models
ggroc(booper)
ggroc(dooper)
ggroc(list(unbalanced = cooper, forest = dooper, balanced = booper, unbalLogit = aooper)) #boom! all three
