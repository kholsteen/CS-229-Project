# sample_code_library.R
# Function library called by sample_code.R for the Practice Fusion Diabetes Classification Competition.
#
# Requires the provided SQLite database.
# 18 Nov 2015:  
#   - Eliminated TypeString
#   - Replaced ICD-9 digits with CCS groupings for diagnosis features
#   - Added ratios of CCS dgn counts to total #transcripts
#   - Added ratios of physician type counts to total #transcripts
# ================================================================================= #

create_flattenedDataset <- function(con,  Ndiagnosis, AddMedication, Nlab, AddTranscript, nDrTypes) {
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
#       Ndiagnosis: Number of diagnosis features to include (by ICD9 code)
#       Nmedication: Number of medciation features to include (by medication name)
#       Nlab: Number of lab features to include (by HL7 text)
#
# Returns
#       Data frame with one patientGuid per row. 
#       Columns are [indicator] [Ndiagnosis + Nmedication + Nlab features]
  
  patientTable <- dbGetQuery(con, "SELECT * FROM training_patient")
  flatDataset <-   subset(patientTable, select=c("PatientGuid", "dmIndicator", "Gender", "YearOfBirth"))
  
  if (AddTranscript > 0) {flatDataset <- addTranscriptVariables(con, flatDataset, nDrTypes)}
  if ( Ndiagnosis > 0 ) { flatDataset <- addDiagnosisVariables(con, flatDataset, Ndiagnosis) }
  if ( AddMedication > 0 ) { flatDataset <- addMedicationVariables(con, flatDataset) }
  if ( Nlab >0 ) { flatDataset <- addLabsVariables(con, flatDataset, Nlab) }
  
  ## eliminate patient with missing diabetes indicator or year of birth
  flatDataset <- flatDataset[!is.na(flatDataset$dmIndicator) & !is.na(flatDataset$YearOfBirth),]    
  flatDataset$Gender <- as.factor(flatDataset$Gender)
  flatDataset$dmIndicator <- as.factor(flatDataset$dmIndicator)
  flatDataset$PatientGuid <- as.factor(flatDataset$PatientGuid) 
  
  return(flatDataset) 
}
# ================================================================================= #
addTranscriptVariables <- function(con,  flatDataset, nDrTypes) {
  
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
  
  ## get flag for high BP
#   flg_bp_high <- cbind(by(transcript[, c("DiastolicBP", "SystolicBP")], transcript$PatientGuid, function(dset) {
#       ct.D80 <- sum(!is.na(dset$DiastolicBP) & dset$DiastolicBP > 80)
#       ct.S130 <- sum(!is.na(dset$SystolicBP) & dset$SystolicBP > 130)
#       return(as.integer(ct.D80 >= 2 || ct.S130 >= 1))
#     }))
  
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
  
  ## count overall number of transcripts (=dr visits)
  ct.Transcripts <- as.vector(by(transcript, transcript$PatientGuid, nrow))
    
  ## calculate ratios between counts of physician visits by type and overall # transcripts
  drType.Transcripts.Ratio <- drTypeCols/ct.Transcripts
  colnames(drType.Transcripts.Ratio) <- paste0("rto.", gsub(" ", "", freqTabledrType$drType[1:nDrTypes]))

  AllCols = as.data.frame(cbind(medCols, drTypeCols, ct.Transcripts=ct.Transcripts, 
                                drType.Transcripts.Ratio))
  guids <- unique(transcript$PatientGuid)
  AllCols$PatientGuid <- guids[order(guids)]
  flatDataset <- merge(flatDataset, AllCols, by="PatientGuid", all.x=TRUE)
  
  ## Replace NA with zeros
  for (colname in c(colnames(drTypeCols), "ct.Transcripts", colnames(drType.Transcripts.Ratio))) {
    flatDataset[is.na(flatDataset[, colname]), colname] <- 0
  }
  return(flatDataset)
}


# ================================================================================= #
addDiagnosisVariables <- function(con,  flatDataset, Ndiagnosis) {
# addDiagnosisVariables()
# Adds Ndiagnosis diagnosis features to the input flatDataset.
# Diagnosis features to include are determined by frequency in the training set
# Diagnosis features are identified by ICD9 codes.
#
# Arguments
#      con: SQLite connection
#      flatDataset: data frame to which features are added
#      Ndiagnosis: number of diagnosis features to add
#
# Returns
#      flatDataset: input dataset with diagnosis features added
  
  # Create frequency table of ICD9 codes as determined by training set.
  # Train and test sets need to reference the same features.
  # why are we joining the transcript diagnosis to the diagnosis?
  train_dxTable <- dbGetQuery(con, "SELECT PatientGuid, 
                                           TranscriptDiagnosisGuid,
                                           TranscriptGuid,
                                           d.DiagnosisGuid,
                                           ICD9Code
                              FROM training_transcriptdiagnosis td 
                              INNER JOIN training_diagnosis d 
                              ON d.DiagnosisGuid = td.DiagnosisGuid")

  ## read in CCS diagnosis groupings csv file
  ccs <- read.csv(paste0(ccs_path, "CCS_Diagnoses.csv"), header=FALSE, stringsAsFactors=FALSE)
  ccs.key <- ccs[!is.na(ccs[, 1]),]
  names(ccs.key) <- c("id", "name")
  ccs.key$startrow = as.numeric(rownames(ccs.key)) + 1
  
  ## iterate over keys to create key-value matches 
  ccs.dgn.list <- lapply(1:nrow(ccs.key), function(i) {
    ccs.value <- c()
    if (i < nrow(ccs.key)) {
      endrow <- ccs.key$startrow[i+1]-2
    } else if (i == nrow(ccs.key)) {
      endrow <- nrow(ccs)
    }    
   # if (ccs.key$startrow[i] > endrow) { print(ccs.key[i, 1:3])}
    for (j in ccs.key$startrow[i]:endrow) {
      ccs.value <- c(ccs.value, unlist(strsplit(as.character(ccs[j,2]), " ")))
    }
    return(ccs.value)
  })  
  names(ccs.dgn.list) <- ccs.key$id
  
  ##modify ICD-9 codes to match CCS code format (add leading & trailing zeros, remove decimal)
  left.of.point <- unlist(strsplit(train_dxTable$ICD9Code, "[.]"))
  train_dxTable$ICD9_CCS <- ""
  train_dxTable$ICD9_CCS[nchar(left.of.point[1])==3] <- gsub("[.]", "", train_dxTable$ICD9Code[nchar(left.of.point[1])==3])
  train_dxTable$ICD9_CCS[nchar(left.of.point[1])==2] <- paste0("0",gsub("[.]", "", train_dxTable$ICD9Code[nchar(left.of.point[1])==2]))
  train_dxTable$ICD9_CCS[nchar(left.of.point[1])==1] <- paste0("00", gsub("[.]", "", train_dxTable$ICD9Code[nchar(left.of.point[1])==1]))
  train_dxTable$ICD9_CCS <- toupper(train_dxTable$ICD9_CCS)

  ccs.present <- sapply(train_dxTable$ICD9_CCS, function(dgn){
    sum(unlist(lapply(ccs.dgn.list, function(dgn.list) {
      return(dgn %in% dgn.list)
      })))})
#  s <- cbind(train_dxTable[which(ccs.present!=1), c("ICD9Code", "ICD9_CCS")], ct = ccs.present[which(ccs.present!=1)])
#    su <- ddply(.data = s, .variables = c("ICD9Code", "ICD9_CCS", "ct"),  .fun = function(xx) {
#      c(count = nrow(xx))
#    })
#    su.sorted <- su[order(su$count, decreasing = TRUE),]
#    write.csv(su.sorted,"codes_to_review.csv")  
  
  ccs.freqList <- unlist(lapply(ccs.dgn.list, function(dgn.list) {
     sum(train_dxTable[,"ICD9_CCS"] %in% dgn.list)
   }))
 
  ccs.freqTable <- data.frame(cbind(id = ccs.key$id, freq = ccs.freqList))
  ccs.freqTable <- ccs.freqTable[order(ccs.freqTable$freq, decreasing=TRUE),]

  ## iterate over keys and count dgns within each key  
  if (!is.numeric(Ndiagnosis)) {Ndiagnosis = nrow(ccs.key)}
    ccs.ct.list <- lapply(ccs.freqTable$id[1:Ndiagnosis], function(id) {  
      counts <- ddply(train_dxTable,c("PatientGuid"),  
                    .fun = function(xx, dgn.list) {
                      c(count = sum(xx[,"ICD9_CCS"] %in% dgn.list))
                    }, dgn.list = ccs.dgn.list[[as.character(id)]])
    return(counts$count)
  })      
  ## combine into data frame that can be merged with flatDataset
  ccs.ct.Cols <- data.frame(do.call(cbind, ccs.ct.list))
  colnames(ccs.ct.Cols) <- paste0("ct.ccs.", ccs.freqTable$id[1:Ndiagnosis])

  guids <- unique(train_dxTable$PatientGuid)
  ccs.ct.Cols$PatientGuid <- guids[order(guids)]
  
  flatDataset <- merge(flatDataset, ccs.ct.Cols, by = c("PatientGuid"), all.x=TRUE)
  for (colname in colnames(ccs.ct.Cols)) {
    flatDataset[is.na(flatDataset[, colname]), colname] <- 0
  }
  
  flatDataset$ct.DGNs <- rowSums(flatDataset[, colnames(ccs.ct.Cols)[-ncol(ccs.ct.Cols)]])
  flatDataset$ct.ccs.Distinct <- rowSums(flatDataset[, colnames(ccs.ct.Cols)[-ncol(ccs.ct.Cols)]]>0)
  
  ## calculate ratio s of ccs counts to total # transcripts (if total # transcripts is present)
  if ("ct.Transcripts" %in% colnames(flatDataset)) {     
    ## initialize
    flatDataset[, paste0("rto.ccs.", ccs.freqTable$id[1:Ndiagnosis])] <- 0
    flatDataset$rto.DGNs <- 0
    ## calculate ratios for subjects with nonzero # transcripts
    flatDataset[flatDataset$ct.Transcripts>0, paste0("rto.ccs.", ccs.freqTable$id[1:Ndiagnosis])] <- 
      flatDataset[flatDataset$ct.Transcripts>0, colnames(ccs.ct.Cols)[-ncol(ccs.ct.Cols)]]/flatDataset$ct.Transcripts[flatDataset$ct.Transcripts>0]
    flatDataset$rto.DGNs[flatDataset$ct.Transcripts>0] <- 
      flatDataset$ct.DGNs[flatDataset$ct.Transcripts>0]/flatDataset$ct.Transcripts[flatDataset$ct.Transcripts>0]
    } 
  ## is it possible to have positive dgn and zero transcripts?
  return(flatDataset) 
}

# ================================================================================= #
addMedicationVariables <- function(con,  flatDataset) {

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
addLabsVariables <- function(con,  flatDataset, Nlab) {
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
  tableToRead <- train_labsTable
    
  
  for ( i in 1:Nlab ) {
    hasFeature <- unique(subset(tableToRead, HL7Text==freqTable[i,1])$PatientGuid)
    indCol <- rep(0, length(flatDataset[,1]))
    indCol[flatDataset$PatientGuid %in% hasFeature] <- 1
    flatDataset <- cbind(flatDataset, indCol)
    colnames(flatDataset)[length(flatDataset)] <- freqTable[i,1]
  }
  
  return(flatDataset) 
}

