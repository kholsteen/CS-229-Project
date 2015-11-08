# sample_code_library.R
# Function library called by sample_code.R for the Practice Fusion Diabetes Classification Competition.
#
# Requires the provided SQLite database.
# 7-July-2012
# ================================================================================= #

library(RSQLite)
library(plyr)
# ================================================================================= #


# ================================================================================= #
create_flattenedDataset <- function(con, typeString, Ndiagnosis, Nmedication, Nlab, AddTranscript) {
# create_flattenedDataset()
# A given patient will have mulitple diagnoses, medication, labs, prescriptions, etc.
# This function does a simple flattening procedure whereby the top N most 
# prevalent values are converted into binary variables.
# Example: Assume the top 2 most common diagnoses are Hypertension and Thrombocytopenia.
#   Instead of being listed under ICD9 or diagnosis description as 2 possible categorical values, 
#   2 new binary features are created called Hypertension and Thrombocytopenia created to
#   indicate the absence/presence of these diagnoses for each given patient.
#
# Arugments
#       con: SQLite connection
#       typeString: test or training 
#       Ndiagnosis: Number of diagnosis features to include (by ICD9 code)
#       Nmedication: Number of medciation features to include (by medication name)
#       Nlab: Number of lab features to include (by HL7 text)
#
# Returns
#       Data frame with one patientGuid per row. 
#       Columns are [indicator] [Ndiagnosis + Nmedication + Nlab features]
  
  if ( typeString == "test" ) {
    patientTable <- dbGetQuery(con, "SELECT * FROM test_patient")
    patientDemo <-   subset(patientTable, select=c("PatientGuid", "Gender", "YearOfBirth"))
    # Convert gender = "F" or "M" to 0, 1
    patientDemo[patientDemo$Gender == "F",2] <- 0
    patientDemo[patientDemo$Gender == "M",2] <- 1    
    flatDataset <- patientDemo 
  }
  else {
    patientTable <- dbGetQuery(con, "SELECT * FROM training_patient")
    patientDemo <-   subset(patientTable, select=c("PatientGuid", "dmIndicator", "Gender", "YearOfBirth"))
    # Convert gender = "F" or "M" to 0, 1
    patientDemo[patientDemo$Gender == "F",3] <- 0
    patientDemo[patientDemo$Gender == "M",3] <- 1
    
    flatDataset <- patientDemo
  }
  
  if ( typeString == "test" ) {
    if ( Ndiagnosis > 0 ) { flatDataset <- addDiagnosisVariables(con, "test", flatDataset, Ndiagnosis) }
    if ( Nmedication > 0 ) {flatDataset <- addMedicationVariables(con, "test", flatDataset, Nmedication) }
    if ( Nlab > 0 ) { flatDataset <- addLabsVariables(con, "test", flatDataset, Nlab) }
  }
  else {
    if ( Ndiagnosis > 0 ) { flatDataset <- addDiagnosisVariables(con, "training", flatDataset, Ndiagnosis) }
    if ( Nmedication > 0 ) { flatDataset <- addMedicationVariables(con, "training", flatDataset, Nmedication) }
    if ( Nlab >0 ) { flatDataset <- addLabsVariables(con, "training", flatDataset, Nlab) }
    if (AddTranscript > 0) {flatDataset <- addTranscriptVariables(con, "training", flatDataset)}
  }
  
  return(flatDataset) 
}
# ================================================================================= #
addTranscriptVariables <- function(con, typeString, flatDataset) {
  
  transcript <- dbGetQuery(con, "SELECT * FROM training_transcript")
  # get mean, max of numeric variables
  num.vars <- c("BMI", "SystolicBP",  "DiastolicBP")
  for (varname in num.vars) {
    transcript[which(transcript[, varname] %in% c("NULL")), varname] <- NA 
    transcript[, varname] <- as.numeric(transcript[, varname])
  if (varname %in% c("SystolicBP",  "DiastolicBP")) {
      transcript[which(transcript[, varname] < 50), varname] <- NA 
      transcript[which(transcript[, varname] > 250), varname] <- NA 
    } else if (varname %in% "BMI") {
      transcript[which(transcript[, varname] < 17), varname] <- NA
      transcript[which(transcript[, varname] > 50), varname] <- NA     
    }    
  }

  ## loop over numeric variables: 
  maxCols <- lapply(num.vars, function(varname) {
    return(by(transcript[, varname], transcript$PatientGuid, max))
  })
  maxCols <- do.call('cbind', maxCols)
  colnames(maxCols) <- paste0(num.vars,".max")
  meanCols <- lapply(num.vars, function(varname) {
    return(by(transcript[, varname], transcript$PatientGuid, mean, na.rm=TRUE))
  })  
  meanCols <- do.call('cbind', meanCols) 
  meanCols[which(is.nan(meanCols))] <- NA
  colnames(meanCols) <- paste0(num.vars,".mean")
  phys.Endo.Count <- ddply(transcript,.(PatientGuid),
        summarize, count = sum(PhysicianSpecialty %in% c("Endocrinology; Diabetes; & Metabolism")))
  AllCols = as.data.frame(cbind(maxCols, meanCols, phys.Endo.Count = phys.Endo.Count$count))
  AllCols$PatientGuid = unique(transcript$PatientGuid)
  flatDataset <- merge(flatDataset, AllCols, by="PatientGuid", all=TRUE)
 
}


# ================================================================================= #
addDiagnosisVariables <- function(con, typeString, flatDataset, Ndiagnosis) {
# addDiagnosisVariables()
# Adds Ndiagnosis diagnosis features to the input flatDataset.
# Diagnosis features to include are determined by frequency in the training set
# Diagnosis features are identified by ICD9 codes.
#
# Arguments
#      con: SQLite connection
#      typeString: "test" or "training"
#      flatDataset: data frame to which features are added
#      Ndiagnosis: number of diagnosis features to add
#
# Returns
#      flatDataset: input dataset with diagnosis features added
  
  # Create frequency table of ICD9 codes as determined by training set.
  # Train and test sets need to reference the same features.
  train_dxTable <- dbGetQuery(con, "SELECT * FROM training_transcriptdiagnosis td 
                              LEFT JOIN training_diagnosis d 
                              ON d.DiagnosisGuid = td.DiagnosisGuid")
  
  ## create table of most common 5-digit (full) ICD-9 codes
  freqTable5digit <- data.frame(prop.table(table(train_dxTable$ICD9Code)))
  colnames(freqTable5digit) <- c("ICD9", "Freq")
  freqTable5digit$ICD9 <- levels(droplevels(freqTable5digit$ICD9))  # formating step: remove factors to get ICD9 as strings
  freqTable5digit <- freqTable5digit[order(freqTable5digit$Freq, decreasing=TRUE),]
  
  ## create table of most common 1st 3-digits of ICD-9 codes (more general category)
  ## get substrings
  train_dxTable$ICD9_3digit <- substr(train_dxTable$ICD9Code,1,3)
  ## create frequency table
  freqTable3digit <- data.frame(prop.table(table(train_dxTable$ICD9_3digit)))
  colnames(freqTable3digit) <- c("ICD9", "Freq")
  freqTable3digit$ICD9 <- levels(droplevels(freqTable3digit$ICD9)) 
  freqTable3digit <- freqTable3digit[order(freqTable3digit$Freq, decreasing=TRUE),] 
  
  if ( typeString == "test" ) {
    tableToRead <- dbGetQuery(con, 
                              "SELECT * FROM test_transcriptdiagnosis td 
                              LEFT JOIN test_diagnosis d 
                              ON d.DiagnosisGuid = td.DiagnosisGuid")
    ## get substrings for test set
    tableToRead$ICD9_3digit <- substr(tableToRead$ICD9Code,1,3)
  }
  else {
    tableToRead <- train_dxTable
  }

  ## count the number of instances of the given code
  ## considering only the Ndiagnosis most common codes
  dgn5digitList <- lapply(1:Ndiagnosis, function(d) {
    counts <- ddply(tableToRead,c("PatientGuid"),  
                    .fun = function(xx, dgn) {
                      c(count = sum(xx[,"ICD9Code"]==dgn))
                    }, dgn= freqTable5digit[d,1])
    return(counts$count)
  })    
  dgn5digitCols <- do.call('cbind', dgn5digitList)
  colnames(dgn5digitCols) <- paste0("ct.", freqTable5digit$ICD9[1:Ndiagnosis], "_5digit")
      
  dgn3digitList <- lapply(1:Ndiagnosis, function(d) {
    counts <- ddply(tableToRead,c("PatientGuid"),  
                      .fun = function(xx, dgn) {
        c(count = sum(xx[,"ICD9_3digit"]==dgn))
        }, dgn= freqTable3digit[d,1])  
    return(counts$count)
      })  
  dgn3digitCols <- do.call('cbind', dgn3digitList) 
  colnames(dgn3digitCols) <- paste0("ct.", freqTable3digit$ICD9[1:Ndiagnosis], "_3digit") 
    
  dgnAllCols <- as.data.frame(cbind(dgn5digitCols, dgn3digitCols))
  dgnAllCols$PatientGuid <- unique(tableToRead$PatientGuid)
  
  flatDataset <- merge(flatDataset, dgnAllCols, by="PatientGuid", all=TRUE)
  return(flatDataset) 
}

# ================================================================================= #
addMedicationVariables <- function(con, typeString, flatDataset, Nmedication) {
  # addMedicationVariables()
  # Adds specified number of medication features (Nmedication) to the input flatDataset.
  # Medication features are identified by medication name.
  #
  # Arguments
  #      medTable: medication table
  #      flatDataset: data frame to which features are added
  #      Nmedication: number of medication features to add
  #
  # Returns
  #      flatDataset: input dataset with diagnosis features added
  
  # Create frequency table determined by training set.
  # Train and test sets need to reference the same features.
  train_medTable <- dbGetQuery(con, "SELECT * FROM training_medication")
  freqTable <- data.frame(prop.table(table(train_medTable$MedicationName)))  
  colnames(freqTable) <- c("MedName", "Freq")
  freqTable$MedName <- levels(droplevels(freqTable$MedName))  # formating step: remove factors to get MedName as strings
  freqTable <- freqTable[order(freqTable$Freq, decreasing=TRUE),]
  write.csv(freqTable, "freqTable_Meds.csv")
  
  if ( typeString == "test" ) {
    tableToRead <- dbGetQuery(con, "SELECT * FROM test_medication")
  }
  else {
    tableToRead <- train_medTable
  }  
  
  ## Identify hypertension and other classes of medications
  
  for ( i in 1:Nmedication ) {
    hasFeature <- unique(subset(tableToRead, MedicationName==freqTable[i,1])$PatientGuid)
    indCol <- rep(0, length(flatDataset[,1]))
    indCol[flatDataset$PatientGuid %in% hasFeature] <- 1
    flatDataset <- cbind(flatDataset, indCol)
    colnames(flatDataset)[length(flatDataset)] <- freqTable[i,1]
  }
  
  return(flatDataset)
}


# ================================================================================= #
addLabsVariables <- function(con, typeString, flatDataset, Nlab) {
# addLabsVariables()
# Adds specified number of medication features (Nlabs) to the input flatDataset.
# Lab features are identified by HL7 text
#
# Arguments
#      labsTable: joined version of labPanel, labObservation and labResult tables
#      flatDataset: data frame to which features are added
#      Nlab: number of lab features to add
#
# Returns
#      flatDataset: input dataset with diagnosis features added  
  
  # Create frequency table determined by training set.
  # Train and test sets need to reference the same features.
  train_labsTable <- dbGetQuery(con, "SELECT * FROM training_labs")  
  freqTable <- data.frame(prop.table(table(train_labsTable$HL7Text)))
  colnames(freqTable) <- c("HL7text", "Freq")
  freqTable$HL7text <- levels(droplevels(freqTable$HL7text))  # formating step: remove factors to get HL7Text as strings
  freqTable <- freqTable[order(freqTable$Freq, decreasing=TRUE),]
  
  if ( typeString == "test" ) {
    tableToRead <- dbGetQuery(con, "SELECT * FROM test_labs")
  }
  else {
    tableToRead <- train_labsTable
  }  
  
  for ( i in 1:Nlab ) {
    hasFeature <- unique(subset(tableToRead, HL7Text==freqTable[i,1])$PatientGuid)
    indCol <- rep(0, length(flatDataset[,1]))
    indCol[flatDataset$PatientGuid %in% hasFeature] <- 1
    flatDataset <- cbind(flatDataset, indCol)
    colnames(flatDataset)[length(flatDataset)] <- freqTable[i,1]
  }
  
  return(flatDataset) 
}

