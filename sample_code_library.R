# sample_code_library.R
# Function library called by sample_code.R for the Practice Fusion Diabetes Classification Competition.
#
# Requires the provided SQLite database.
# 7-July-2012
# ================================================================================= #



# ================================================================================= #
create_flattenedDataset <- function(con, typeString, Ndiagnosis, AddMedication, Nlab, AddTranscript, nDrTypes) {
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
    flatDataset <- patientDemo 
  }
  else {
    patientTable <- dbGetQuery(con, "SELECT * FROM training_patient")
    patientDemo <-   subset(patientTable, select=c("PatientGuid", "dmIndicator", "Gender", "YearOfBirth", "State"))
    flatDataset <- patientDemo
  }
  if ( typeString == "test" ) {
    if ( Ndiagnosis > 0 ) { flatDataset <- addDiagnosisVariables(con, "test", flatDataset, Ndiagnosis) }
    if ( Nmedication > 0 ) {flatDataset <- addMedicationVariables(con, "test", flatDataset, Nmedication) }
    if ( Nlab > 0 ) { flatDataset <- addLabsVariables(con, "test", flatDataset, Nlab) }
  }
  else {
    if ( Ndiagnosis > 0 ) { flatDataset <- addDiagnosisVariables(con, "training", flatDataset, Ndiagnosis) }
    if ( AddMedication > 0 ) { flatDataset <- addMedicationVariables(con, "training", flatDataset) }
    if ( Nlab >0 ) { flatDataset <- addLabsVariables(con, "training", flatDataset, Nlab) }
    if (AddTranscript > 0) {flatDataset <- addTranscriptVariables(con, "training", flatDataset, nDrTypes)}
  }  
  
  ## eliminate patient with missing diabetes indicator or year of birth
  flatDataset <- flatDataset[!is.na(flatDataset$dmIndicator) & !is.na(flatDataset$YearOfBirth),]    
  flatDataset$Gender <- as.factor(flatDataset$Gender)
  flatDataset$dmIndicator <- as.factor(flatDataset$dmIndicator)
  flatDataset$State <- as.factor(flatDataset$State)  
  flatDataset$PatientGuid <- as.factor(flatDataset$PatientGuid)  
  return(flatDataset) 
}
# ================================================================================= #
addTranscriptVariables <- function(con, typeString, flatDataset, nDrTypes) {
  
  transcript <- dbGetQuery(con, "SELECT * FROM training_transcript")
  # get median of numeric variables
  num.vars <- c("Height", "Weight", "BMI", "SystolicBP",  "DiastolicBP")
  for (varname in num.vars) {
    transcript[which(transcript[, varname] %in% c("NULL")), varname] <- NA     
    transcript[, varname] <- as.numeric(transcript[, varname])
    ## Eliminate "unreasonable" values for numeric vars
    if (varname %in% c("Height")) {
      transcript[which(transcript[, varname] < 36), varname] <- NA 
      transcript[which(transcript[, varname] > 96), varname] <- NA     
    } else if (varname %in% c("Weight")) {
      transcript[which(transcript[, varname] < 50), varname] <- NA 
      transcript[which(transcript[, varname] > 500), varname] <- NA          
    }else if (varname %in% c("SystolicBP",  "DiastolicBP")) {
      transcript[which(transcript[, varname] < 50), varname] <- NA 
      transcript[which(transcript[, varname] > 250), varname] <- NA 
    } else if (varname %in% "BMI") {      
      transcript[which(transcript[, varname] < 14), varname] <- NA
      transcript[which(transcript[, varname] > 60), varname] <- NA     
    } 
  }
  ## loop over numeric variables: 
  medCols <- lapply(num.vars, function(varname) {
    return(by(transcript[, varname], transcript$PatientGuid, median, na.rm=TRUE))
  })
  medCols <- do.call('cbind', medCols)  
  colnames(medCols) <- paste0(num.vars,".med")
  
  ## impute missing BMI
  flg_impute_BMI <- which(is.na(medCols[,"BMI.med"]) & !is.na(medCols[,"Height.med"]) & !is.na(medCols[,"Weight.med"]))
  medCols[flg_impute_BMI, "BMI.med"] <- 
    703*medCols[flg_impute_BMI,"Weight.med"]/(medCols[flg_impute_BMI,"Height.med"]^2) 
  
  ## impute missing BP
  medCols[is.na(medCols[, "SystolicBP.med"] ), "SystolicBP.med"] <-  median(medCols[, "SystolicBP.med"], na.rm=TRUE)
  medCols[is.na(medCols[, "DiastolicBP.med"] ), "DiastolicBP.med"] <-  median(medCols[, "DiastolicBP.med"], na.rm=TRUE)
    
  ## get overall physician specialty frequencies
  freqTabledrType <- data.frame(prop.table(table(transcript$PhysicianSpecialty[!transcript$PhysicianSpecialty %in% c("","x Unknown or N/A")])))
  colnames(freqTabledrType) <- c("drType", "Freq")
  freqTabledrType <- freqTabledrType[order(freqTabledrType$Freq, decreasing=TRUE),]  
  
  ## get patient-level counts of dr visits by physicial specialty
  drTypeList <- lapply(1:nDrTypes, function(d) {
    counts <- ddply(transcript,c("PatientGuid"),  
                    .fun = function(xx, drType) {
                      c(count = sum(xx[,"PhysicianSpecialty"]==drType))
                    }, drType = freqTabledrType$drType[d])  
    return(counts$count)
  })  
  drTypeCols <- do.call('cbind', drTypeList) 
  colnames(drTypeCols) <- paste0("ct.", gsub(" ", "", freqTabledrType$drType[1:nDrTypes]))
    
  AllCols = as.data.frame(cbind(medCols, drTypeCols))
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
  for (colname in colnames(dgnAllCols)) {
    flatDataset[is.na(flatDataset[, colname]), colname] <- 0
  }
  return(flatDataset) 
}

# ================================================================================= #
addMedicationVariables <- function(con, typeString, flatDataset) {

  SyncMedication <- dbGetQuery(con, "SELECT * FROM training_medication")
  count <- runif(length(SyncMedication[,1]), 1.0, 1.0)
  SyncMedication <- cbind(SyncMedication, count, deparse.level = 1)
  rm(count)
  gc()
  
  cntMedication <- dcast(SyncMedication, PatientGuid ~ count, sum, value.var = "count")
  names(cntMedication) <- c("PatientGuid",  "cntMedication")
  flatDataset <- merge(flatDataset, cntMedication, all.x = TRUE, by = c("PatientGuid"))
  flatDataset[is.na(flatDataset[,ncol(flatDataset)]),ncol(flatDataset)]<-0 
  
  SyncMed <- subset(SyncMedication, MedicationName == "Lisinopril oral tablet")
  cntMed <- dcast(SyncMed, PatientGuid ~ count, sum, value.var = "count")
  names(cntMed) <- c("PatientGuid",  "cntMedLis")
  flatDataset <- merge(flatDataset, cntMed, all.x = TRUE, by = c("PatientGuid"))
  rm(SyncMed, cntMed)
  flatDataset[is.na(flatDataset[,ncol(flatDataset)]),ncol(flatDataset)]<-0 
  
  SyncMed <- subset(SyncMedication, MedicationName == "Hydrochlorothiazide oral tablet")
  cntMed <- dcast(SyncMed, PatientGuid ~ count, sum, value.var = "count")
  names(cntMed) <- c("PatientGuid",  "cntMedHyd")
  flatDataset <- merge(flatDataset, cntMed, all.x = TRUE, by = c("PatientGuid"))
  rm(SyncMed, cntMed)
  flatDataset[is.na(flatDataset[,ncol(flatDataset)]),ncol(flatDataset)]<-0 
  
  SyncMed <- subset(SyncMedication, MedicationName == "Norvasc (amLODIPine) oral tablet")
  cntMed <- dcast(SyncMed, PatientGuid ~ count, sum, value.var = "count")
  names(cntMed) <- c("PatientGuid",  "cntMedNor")
  flatDataset <- merge(flatDataset, cntMed, all.x = TRUE, by = c("PatientGuid"))
  rm(SyncMed, cntMed)
  flatDataset[is.na(flatDataset[,ncol(flatDataset)]),ncol(flatDataset)]<-0 
  
  SyncMed <- subset(SyncMedication, MedicationName == "AmLODIPine Besylate (amLODIPine) oral tablet")
  cntMed <- dcast(SyncMed, PatientGuid ~ count, sum, value.var = "count")
  names(cntMed) <- c("PatientGuid",  "cntMedAmL")
  flatDataset <- merge(flatDataset, cntMed, all.x = TRUE, by = c("PatientGuid"))
  rm(SyncMed, cntMed)
  flatDataset[is.na(flatDataset[,ncol(flatDataset)]),ncol(flatDataset)]<-0 
  
  SyncMed <- subset(SyncMedication, MedicationName == "Hydrochlorothiazide-Lisinopril oral tablet")
  cntMed <- dcast(SyncMed, PatientGuid ~ count, sum, value.var = "count")
  names(cntMed) <- c("PatientGuid",  "cntMedHydLis")
  flatDataset <- merge(flatDataset, cntMed, all.x = TRUE, by = c("PatientGuid"))
  rm(SyncMed, cntMed)
  flatDataset[is.na(flatDataset[,ncol(flatDataset)]),ncol(flatDataset)]<-0 
  
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

