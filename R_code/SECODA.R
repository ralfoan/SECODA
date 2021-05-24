# SECODA anomaly detection algorithm
# Ralph Foorthuis
# www.foorthuis.nl
# 2016, 2017, 2018 (20180421)
# GNU Affero General Public License version 3
# For research regarding SECODA see: Foorthuis, R.M. (2017). "SECODA: Segmentation- and Combination-Based Detection of Anomalies." In: Proceedings of the 4th IEEE International Conference on Data Science and Advanced Analytics (DSAA 2017), Tokyo, Japan.
# See this paper for more information about the types of anomalies: Foorthuis, R.M. (2018). "A Typology of Data Anomalies." Accepted for Presentation at IPMU 2018, the 17th International Conference on Information Processing and Management of Uncertainty in Knowledge-Based Systems, Cádiz, Spain.


SECODA <- function(datset, BinningMethod="EW", HighDimMode="NO", MinimumNumberOfIterations=3, MaximumNumberOfIterations=99999, StartHeuristicsAfterIteration=10, FractionOfCasesToRetain=0.2, TestMode=FALSE) {
  
  # Check hard conditions
  if (MinimumNumberOfIterations > MaximumNumberOfIterations) stop("Argument MinimumNumberOfIterations must be smaller than or equal to MaximumNumberOfIterations.")
  if ( class(datset) != "data.frame") stop("Input datset should be a data.frame.")
  if (BinningMethod != "EW" && BinningMethod != "ED") stop("BinningMethod should be 'EW' (equiwidth) or 'ED' (equidepth).")
  if (HighDimMode != "NO" && HighDimMode != "CA" && HighDimMode != "IN") stop("HighDimMode should be 'NO' (normal), 'CA' (first iteration only analyzes categorical attributes) or 'IN' (combine normal and univariate analysis).")
  
  # R settings
  options(digits=22) # After several iterations some anomalies may differ only to a small degree. Make sure that R prints sufficient decimal places if you want to visually compare them.
  require(data.table) 
  
  # Initialize internal parameters
  SDAnoFraction <- 0.003  # This is 0.3%, the percentage of cases beyond 3 times the standard deviation (i.e. (100-99.7)/100=0.003). This is a standard way to define the fraction of cases in a dataset that can be denoted outliers. This is used as a threshold to determine if we can stop the algorithm, which iterates until it converges when the fraction of anomalies as defined by SDAnoFraction is reached. 
  SDAnoFractionInIterationOne <- FALSE  # Whether the SDAnoFraction threshold has already been reached after the first iteration.
  NBreaksCurrent <- 2  # The number of cut() breaks (cut points) to start with for discretization purposes.
  AveAnoScoreStopPoint <- 1 # The threshold against which the fraction of anomalies is being tested. If the fraction of AveAnoScore reaches this threshold, the algorithm converges. We start with 1, which represents that we need to identify 0.3% cases that are totally unique. But this threshold will be increased in later iterations, so as to relax the uniqueness requirement if it is too difficult to find sufficient unique cases. 
  StartIteration <- TRUE  # Whether the next iteration should start.
  UniqueCases <- FALSE  # Whether one or more unique cases have already been encountered.
  CurrentIteration <- 0  # The current iteration number.
  VarsTable <- data.frame(VariableName=character(), DataType=character(), stringsAsFactors=FALSE)  # The set of variables (and their datatypes) that is being analyzed. 
  AnomalyTable <- data.table(Ano_ID=1:nrow(datset), CurrentFrequency=as.integer(NA), AveAnoScore=as.numeric(NA), stringsAsFactors=FALSE)   # The table that will contain anomaly information.
  AnomalyTabTemp <- data.table(Ano_ID=integer(), AveAnoScore=numeric())  # The table to store the scores of the cases that are pruned away from AnomalyTable.
  Constellations <- vector(mode = "character") # The collection of Constellation IDs. 
  AveAnoScore=CurrentFrequencyIN=CurrentFrequency=NULL # Set these variables to NULL, so as to avoid NOTES in the R Build Check.
  if (TestMode==TRUE) { ProgressTable <- data.frame(Iteration=integer(), Breaks=integer(), AveAnoScoreStopPoint=numeric(), FractionCurrentFreq=numeric(), FractionAveAnoScore=numeric(), NoCasesAnalyzed=integer() ) } # Log of iterations and fractions (with represent the progress towards the convergence point).
  
  # Initialize internal function
  PrintResults <- function() {
    # This function prints the results after converging.
    cat(paste0("\nConverged after iteration ", CurrentIteration, "\n"))
    if ( SDAnoFractionInIterationOne == TRUE) cat(paste0("Note: Fraction of ", SDAnoFraction, " anomalies already reached in first iteration. There may be too many variables in the set, too many classes in the categorical variables, or too few rows. It is advised to run SECODA with HighDimMode 'CA' or 'IN'.\n"))
    if (length(VarsTable[VarsTable$DataType=="numerical",1]) > 0) { # If the dataset contains continuous variables.
      cat(paste0("\nCases with an AveAnoScore <= ", AveAnoScoreStopPoint, " may be inspected as potential anomalies\n"))
    } else { # If the dataset contains only categorical variables the AveAnoScoreStopPoint is irrelevant as a threshold.
      cat(paste0("\nCases with low AveAnoScores may be inspected as potential anomalies\n"))
    }
    cat(paste0(" Number of anomalies with score == 1: ", nrow(AnomalyTable[AnomalyTable$AveAnoScore == 1, "Ano_ID", with=FALSE]), "\n"))
    cat(paste0(" Number of anomalies with score <= 2: ", nrow(AnomalyTable[AnomalyTable$AveAnoScore <= 2, "Ano_ID", with=FALSE]), "\n"))
    cat(paste0(" Number of anomalies with score <= 3: ", nrow(AnomalyTable[AnomalyTable$AveAnoScore <= 3, "Ano_ID", with=FALSE]), "\n")) 
    if (length(VarsTable[VarsTable$DataType=="numerical",1]) > 0) cat(paste0(" Number of anomalies with score <= ", AveAnoScoreStopPoint, ": ", nrow(AnomalyTable[AnomalyTable$AveAnoScore <= AveAnoScoreStopPoint, "Ano_ID", with=FALSE]), "\n\n")) 
    # Cases with score <= AveAnoScoreStopPoint can be considered anomalies. The algorithm usually iterates until it converges at a fraction of anomalies of at least the SDAnoFraction. 
  }
  
  # Analyze data types and put in the VarsTable
  for (i in names(datset)) {
    if( sum(is.na(datset[[i]])) != nrow(datset) ) { # Make sure the column is not completely filled with missing values 
      if( class(datset[[i]])=="numeric" || class(datset[[i]])=="integer" ) { VarsTable <- rbind(VarsTable, data.frame(VariableName=i, DataType="numerical")) 
      } else if (class(datset[[i]])=="factor" || class(datset[[i]])=="character" || class(datset[[i]])=="logical") { VarsTable <- rbind(VarsTable, data.frame(VariableName=i, DataType="categorical")) 
      } else { cat(paste0("Warning: Variable '", i, "' of incompatible data type '", class(datset[[i]]), "' is not included in the anomaly detection. The SECODA algorithm may need to be updated.\n")) }
    } else { cat(paste0("Warning: Variable '", i, "' is ignored because it only contains missing values.\n")) }
  }
  VarsTable$VariableName <- as.character(VarsTable$VariableName); VarsTable$DataType <- as.character(VarsTable$DataType)
  if (HighDimMode=="CA" && length(VarsTable[VarsTable$DataType=="categorical",1]) == 0) {
    HighDimMode = "NO"
    cat(paste0("Warning: No categorical attributes found, so switching to HighDimMode 'NO' (normal).\n"))
  }
  if (nrow(VarsTable) > 1) cat(paste0("Starting analysis in mode '", HighDimMode, "' of ", nrow(VarsTable), " variables (", length(VarsTable[VarsTable$DataType=="numerical",1]), " continuous and ", length(VarsTable[VarsTable$DataType=="categorical",1]), " categorical)\n"))
  if (nrow(VarsTable) == 1) cat(paste0("Starting analysis in mode '", HighDimMode, "' of ", nrow(VarsTable), " ", VarsTable$DataType[1], " variable\n"))
  
  # If HighDimMode is "CA", then count frequencies for the categorical-only constellation. This facilitates the analysis of more attributes in later iterations.
  if (HighDimMode == "CA") {
    if (TestMode==TRUE) cat(paste0("Processing categorical variables to create frequency-base\n")) 
    Constellations <- c(Constellations, paste0("i0CA_Constellation"))  # Add new Constellation variable to set.
    AnomalyTable[, (utils::tail(Constellations,n=1)):=(do.call(paste, c(as.data.frame(datset[AnomalyTable$Ano_ID,VarsTable[VarsTable$DataType=="categorical","VariableName"]]), list(sep=".")))),]   # Create frequency-base with only categorical variables. This should be a subset of all variables, so the frequencies should generally be higher. Too many unique cases in the first iteration(s) should therefore be avoided - or at least be less likely. This thus helps in analyzing more variables, although categorical variables get a somewhat higher weight.
    AnomalyTable[, AveAnoScore:=as.numeric(length(get(utils::tail(Constellations,n=1)))), by=list(unique.values=get(utils::tail(Constellations,n=1)))]   # Insert the frequencies of the constellations in each individual row of AveAnoScore. (With data.table we can do this in one statement.) Convert to numeric (double), since AveAnoScore is numeric.
    UniqueCases = TRUE # The UniqueCases parameter helps in "NO" mode to postpone the analysis in order to speed up the process. However, since the HighDimMode is "CA" creates a base filling of frequencies at the very start, postponing is not possible in this mode. For the remainder of the process, we'll therefore pretend we've already seen unique cases, which will guarantee that no future iterations will be ignored.
  }
  
  # Start iterations for numerical variables
  if (TestMode==FALSE) cat("Iteration: ") # Intro label for short progress report.
  while (StartIteration==TRUE) {
    
    CurrentIteration = CurrentIteration + 1  # Set the iteration we're currently in.
    if (TestMode==TRUE) { 
      cat(paste0("Starting iteration ", CurrentIteration, "\n")) 
    } else {
      cat(" ", CurrentIteration) # Print short progress report
    }
    if (CurrentIteration <= StartHeuristicsAfterIteration) {  
      AveAnoScoreStopPoint = AveAnoScoreStopPoint + 0.1  # For e.g. small datasets, start with AveAnoScoreStopPoint 1 and let it increase slowly in order to focus on truly unique and isolated cases (i.e. with a frequency close to 1) in the early iterations, and anomalies with a higher frequency in later iterations (as there are apparently not many unique cases in the set being analyzed). Note also that the 1.1 stop point used in the beginning still only finds truly unique cases in the first iteration (since the first iteration only finds frequencies as real numbers, e.g. 1, 2, 5, 317).
    } else { 
      AveAnoScoreStopPoint = AveAnoScoreStopPoint + 1  # If after 10 iterations (StartHeuristicsAfterIteration) the algorithm has not yet converged, relax the convergence threshold. The higher it gets, the less really unique/isolated cases we need to find (as apparently they do not exist or are hard to find).
    } # The higher the AveAnoScoreStopPoint, the less iterations are needed. This threshold is increased each iteration, as apparently a low number takes too long. (A low AveAnoScoreStopPoint may be needed for small datasets or datasets with many unique constellations. In these situations, AveAnoScoreStopPoint=2 is reached very quickly if addition is started already at the first iteration and a high number therefore is not desirable.) We start with AveAnoScoreStopPoint=2, because the lowest theoretical score is 1 (an anomaly is then a case with an AveAnoScore between 1 and 2). 
    AnomalyTable$CurrentFrequency <- as.integer(NA)  # Reset. [As integer, since the frequency of a given iteration won't have decimals (as opposed to the average).]
    VarNames <- vector(mode = "character")  # Create empty character vector for the new set of discretized variables that need to be taken into account in the current iteration
    
    # Discretize all numeric variables by iterating the variable set
    for (i in 1:nrow(VarsTable)) {   # Iterate through numerical variables (integers and numerics).
      if (VarsTable$DataType[i] == "numerical") {
        VarName=paste("i", CurrentIteration, "Discr_", VarsTable$VariableName[i], sep="")  # Create new variable name for the current numeric variable. The first number is the iteration (prefixed with 'i' because column names cannot start with a number), followed by the original variable name.
        if (BinningMethod == "EW") { # Equiwidth discretization (equal interval)
          if (TestMode==TRUE) { cat(paste(" Starting equiwidth discretization of", VarsTable$DataType[i], "variable", VarsTable$VariableName[i], "into", VarName, "with", NBreaksCurrent, "breaks\n")) }
          AnomalyTable[, (VarName):=as.character(cut(datset[AnomalyTable$Ano_ID,VarsTable$VariableName[i]], breaks = NBreaksCurrent)),] # Create new discretized variable as character. Only for the Ano_ID's in AnomalyTable, which represent (with each iteration's pruning a smaller portion of) the row numbers of the original datset set.
        } else if (BinningMethod == "ED") { # Equidepth discretization (equal frequency)
          if (TestMode==TRUE) { cat(paste(" Starting equidepth discretization of", VarsTable$DataType[i], "variable", VarsTable$VariableName[i], "into", VarName, "with", NBreaksCurrent, "breaks\n")) }
          nrepl <- floor(nrow(AnomalyTable)/NBreaksCurrent) # Calculate the base frequency for a given bin (some decimals will remain). See https://stackoverflow.com/questions/5731116/equal-frequency-discretization-in-r
          if (NBreaksCurrent %% 2 == 0) { # Test odd/even
            nplus <- seq(from=floor(stats::median(1:NBreaksCurrent))-floor((nrow(AnomalyTable) - nrepl*NBreaksCurrent)/2)+1, length=nrow(AnomalyTable) - nrepl*NBreaksCurrent) # Even: Determine the cases around the middle that will have to hold 1 additional case.  The statement "nrow(AnomalyTable) - nrepl*NBreaksCurrent" gives the number of bins that will have to get 1 extra case in order to be able to bin every case of the entire dataset. The entire statement returns the bin IDs that will get one such extra case.
          } else {
            nplus <- seq(from=floor(stats::median(1:NBreaksCurrent))-floor((nrow(AnomalyTable) - nrepl*NBreaksCurrent)/2), length=nrow(AnomalyTable) - nrepl*NBreaksCurrent) # Odd: The same, but do not add 1 to make sure the bins with an additional case are positioned in the middle.
          }
          nrep <- rep(nrepl,NBreaksCurrent) # Create base frequencies for the bins.
          nrep[nplus] <- nrepl+1 # Correct the bin frequencies so that every case in the dataset can be put into one of the bins.
          NAIDs <- which(is.na(datset[AnomalyTable$Ano_ID,VarsTable$VariableName[i]])) # This identifies the NAs in the original vector.
          AnomalyTable[order(datset[AnomalyTable$Ano_ID,VarsTable$VariableName[i]]), (VarName):=as.character( rep(seq.int(NBreaksCurrent),nrep) ),] # This inserts the bin numbers into a new variable in AnomalyTable.
          AnomalyTable[NAIDs, (VarName):= NA, ] # The NAs have been assigned the highest bin number, so if we want to retain the NAs then we should put them back again.
          rm(nrepl, nplus, nrep, NAIDs)
        }
        VarNames <- c(VarNames, VarName) # Remember all discretized variables
      }
    }
    
    # If needed, calculate the univariate anomaly score. I.e. without taking into account the interactions between attributes. This helps to deal with high-dimensionality.
    if (HighDimMode == "IN" && CurrentIteration <= StartHeuristicsAfterIteration) {
      if (TestMode==TRUE) { cat(paste0(" Starting iteration ", CurrentIteration, "'s determination of case-level current univariate frequencies ($CurrentFrequencyIN)\n"  ) ) }
      VarNamesFreq <- vector(mode = "character")  # Create empty character vector for the set of variables containing the frequencies of the variables
      UniqueCases = TRUE # The UniqueCases parameter helps in "NO" mode to postpone the analysis in order to speed up the process. However, since the HighDimMode is "IN" creates base fillings of frequencies in every iteration and we expect high-dimensionality any way, postponing is not desirable in this mode. For the remainder of the process, we'll therefore pretend we've already seen unique cases, which will guarantee that no future iterations will be ignored.
      # Determine frequencies of each discretized numerical attribute:
      for (DiscrVar in VarNames) {
        AnomalyTable[, (paste0(DiscrVar, "Freq")):=length(get(DiscrVar)), by=list(unique.values=get(DiscrVar)) ]  # Put in frequencies for each individual numerical variable.   
        VarNamesFreq <- c(VarNamesFreq, paste0(DiscrVar, "Freq"))
      }
      # Determine frequencies of each categorical attribute:
      for (DiscrVar in VarsTable[VarsTable$DataType=="categorical", "VariableName"]) {
        if (class(datset[,DiscrVar]) == "character" || class(datset[,DiscrVar]) == "logical") datset[,DiscrVar] <- as.factor(datset[,DiscrVar])  # Convert variable to factor, so that summary() can be called below. The original variable is converted, to avoid having to repeat this in each iteration.
        AnomalyTable[, (paste0("i", CurrentIteration, DiscrVar, "Freq")):= as.data.frame(summary(datset[,DiscrVar]))[datset[,DiscrVar],]]  # Insert the class frequencies in each row. This statement is only executed before pruning, so we can use the full datset table (otherwise this statement is needed: datset[AnomalyTable$Ano_ID,DiscrVar] )
        VarNamesFreq <- c(VarNamesFreq, paste0("i", CurrentIteration, DiscrVar, "Freq"))
      }
      # Calculate average frequency of the individually determined categorical and discretized numerical attributes
      AnomalyTable[, CurrentFrequencyIN:=(rowSums(AnomalyTable[,VarNamesFreq, with=FALSE])/length(VarNamesFreq))]
      AnomalyTable <- AnomalyTable[,!VarNamesFreq, with=FALSE]; rm(VarNamesFreq) # Discard the temporary columns containing the individual frequencies (of which the names are stored in VarNamesFreq)
    }
    
    # Determine the constellation for each case
    Constellations <- c(Constellations, paste0("i", CurrentIteration, "Constellation"))  # Add new Constellation variable to set.
    AnomalyTable[, (utils::tail(Constellations,n=1)):=(paste(do.call(paste, c(AnomalyTable[, eval(VarNames), with=FALSE], list(sep="."))), do.call(paste, c(as.data.frame(datset[AnomalyTable$Ano_ID,VarsTable[VarsTable$DataType=="categorical","VariableName"]]), list(sep="."))), sep=".")),] # Insert constellation into new Constellation variable. Use, first, the discretized numerical vars from the AnomalyTable and, second, the original categorical variables from the datset table. For the second (categorical) part the original datset table is used, so we need to give AnomalyTable$Ano_ID as an input to identify the rows because in later iterations the AnomalyTable contains fewer cases. 
    
    # Determine individual anomaly score by counting the total number of cases present in the individual case's constellation.
    if (TestMode==TRUE) { cat(paste0(" Starting iteration ", CurrentIteration, "'s determination of case-level current combination-based frequencies ($CurrentFrequency)\n"  ) ) }
    AnomalyTable[, CurrentFrequency:=length(get(utils::tail(Constellations,n=1))), by=list(unique.values=get(utils::tail(Constellations,n=1)))]  # Put in the frequencies of the constellations in each individual row. (With data.table we can do this in one statement, instead of creating a separate frequencies table and subsequently doing an inner join on the constellation.)
    if ( UniqueCases == FALSE && min(AnomalyTable$CurrentFrequency)<=1 ) { UniqueCases = TRUE }
    
    # Calculate the average anomaly score, based on the average and the current frequency
    if (TestMode==TRUE) { cat(paste0(" Starting iteration ", CurrentIteration, "'s calculation of case-level average anomaly scores ($AveAnoScore)\n"  ) ) }
    if (HighDimMode != "IN" || CurrentIteration > StartHeuristicsAfterIteration) {
      if ((CurrentIteration == 1 && HighDimMode != "CA") || UniqueCases == FALSE ) {
        AnomalyTable$AveAnoScore <- AnomalyTable$CurrentFrequency  # The first iteration does not have an existing average yet (unless HighDimMode is "CA"), so the frequency just gets inserted. Do the same if no unique combinations have yet been found. This latter tactic ensures that iterations in which no unique cases exist can be ignored so as to speed up the process (their score in AnomalyTable$AveAnoScore may be overwritten, although the last iteration without unique combinations will be used in the calculation of the average AveAnoScore). Iterations with non-unique cases can be ignored since they don't contain information about anomalies yet, but can be expected to result in high average scores that need to be brought down (which will require extra iterations).  Once unique combinations are encountered, no iterations will be ignored anymore.
        if (CurrentIteration>1 && TestMode==TRUE) { cat(paste0(" No unique cases found yet; ignoring (previous) CurrentFrequency\n")) }  # Apparently we have not yet encountered unique cases after iteration 1. 
      } else {
        AnomalyTable$AveAnoScore <- rowMeans(cbind(AnomalyTable$AveAnoScore, AnomalyTable$CurrentFrequency), na.rm = TRUE)  # Calculate mean of two vectors, the existing average and the newly calculated frequency of the current iteration. Note that the current frequency weighs as much as (the average of) all other iterations combined.   http://stackoverflow.com/questions/3497464/element-wise-mean-in-r
      }
    } else { # If HighDimMode == "IN" and the heuristics are not being applied yet
      AnomalyTable$AveFreqIN <- rowMeans(cbind(AnomalyTable$CurrentFrequency, AnomalyTable$CurrentFrequencyIN))  # Calculate mean of multivariate (combination-based) and univariate frequency
      WeightIN <- ((StartHeuristicsAfterIteration-CurrentIteration+1)/StartHeuristicsAfterIteration)/2  # Determine the weight of the univariate AveFreqIN. We want to gradually decrease this weight up until we start the heuristics in the iteration set in StartHeuristicsAfterIteration. A constant weight would be 50%, therefore we divide by 2 in the end.
      AnomalyTable$AveAnoScore <- rowMeans(cbind((AnomalyTable$AveAnoScore*(1-WeightIN)), (AnomalyTable$AveFreqIN*WeightIN)), na.rm=TRUE)  # Calculate mean of previous and current iteration
    }
    
    # Iteration management: Reset variables
    if (TestMode==FALSE) { AnomalyTable <- AnomalyTable[ , !(names(AnomalyTable) %in% c(VarNames, Constellations)), with=FALSE] }  # Drop the individual discretized variables and constellation description if you're not interested in them.  
    if ( CurrentIteration == 1 && ((nrow(AnomalyTable[AnomalyTable$AveAnoScore <= AveAnoScoreStopPoint, "Ano_ID", with=FALSE]) / nrow(datset)) > SDAnoFraction ) )  SDAnoFractionInIterationOne <- TRUE
    
    # Logging
    if (TestMode==TRUE) { 
      cat(paste0(" Fraction of anomalies in $AveAnoScore ", (nrow(AnomalyTable[AnomalyTable$AveAnoScore <= AveAnoScoreStopPoint, "Ano_ID", with=FALSE]) / nrow(datset)), "\n")); cat(paste0(" Fraction of anomalies in $CurrentFreq ", (nrow(AnomalyTable[AnomalyTable$CurrentFrequency  <= AveAnoScoreStopPoint, "Ano_ID", with=FALSE]) / nrow(datset)), "\n"))  
    }
    
    # Verify if the algorithm has converged. This is the case if there are no continuous variables, if the maximum number of iterations is reached, or if a sufficient fraction of anomalies has been detected. 
    if ( length(VarsTable[VarsTable$DataType=="numerical",1]) == 0 || CurrentIteration >= MaximumNumberOfIterations || (CurrentIteration >= MinimumNumberOfIterations && ((nrow(AnomalyTable[AnomalyTable$AveAnoScore <= AveAnoScoreStopPoint, "Ano_ID", with=FALSE]) / nrow(datset)) > SDAnoFraction )) ) {  # Verify if the fraction of anomalies (which have a score <= the stop point) is larger than the set fraction (0.003) for the original dataset. If so, we have a sufficiently detailed average anomaly score.  
      # ! OOK PROBEREN: plus/minus 2.5 times the standard deviation, zou iets moeten zijn van: (100-98.8)/100=0.012  ]
      StartIteration = FALSE  # The algorithm has converged, so a new iteration is not needed.
      PrintResults() # Print final results
    } 
    
    # Log:
    if (TestMode==TRUE) { ProgressTable <- rbind(ProgressTable, data.frame(Iteration=CurrentIteration, Breaks=NBreaksCurrent, AveAnoScoreStopPoint=AveAnoScoreStopPoint, FractionCurrentFreq=(nrow(AnomalyTable[AnomalyTable$CurrentFrequency <= AveAnoScoreStopPoint, "Ano_ID", with=FALSE]) / nrow(datset)), FractionAveAnoScore=(nrow(AnomalyTable[AnomalyTable$AveAnoScore <= AveAnoScoreStopPoint, "Ano_ID", with=FALSE]) / nrow(datset)), NoCasesAnalyzed=nrow(AnomalyTable) )) }
    
    # Update the discretization parameters for the next iteration.
    if (CurrentIteration <= StartHeuristicsAfterIteration) { 
      NBreaksCurrent = NBreaksCurrent + 1  # In the first 10 iterations (StartHeuristicsAfterIteration) simply add 1 cut point. 
    } else {
      NBreaksCurrent = NBreaksCurrent + (floor(AveAnoScoreStopPoint) - 2) # After 10 iterations the algorithm apparently has not converged yet, so let the number of cut points increase with increasingly larger steps. We will accept a slight increase in arbitrariness of the ranking as a result of bigger leaps and thus some loss of information (the negative effects can be expected to be small, as the most important information has already been gathered in previous iterations). We simply re-use AveAnoScoreStopPoint here (or at least the floor() to ensure it's an integer and not a decimal) to have the NBreaksCurrent increase faster (but detract 2 to start at 0, since AveAnoScoreStopPoint has already increased to 2 the first time we want to take larger steps). 
    }
    
    # Run the pruning heuristic after 10 iterations (StartHeuristicsAfterIteration) to speed up the next iteration: discard the cases that are very unlikely to be later identified as anomalies. Do not prune away more than set in FractionOfCasesToRetain (standard not more than 80%).
    if (CurrentIteration > StartHeuristicsAfterIteration && StartIteration==TRUE && nrow(AnomalyTable)/nrow(datset) >= FractionOfCasesToRetain) {
      ToRetain <- as.integer(AnomalyTable[AnomalyTable$AveAnoScore < stats::quantile(AnomalyTable$AveAnoScore, prob=0.95, names=FALSE), "Ano_ID", with=FALSE]$Ano_ID) # Store the IDs of cases to retain.  
      AnomalyTabTemp <- rbind(AnomalyTabTemp, AnomalyTable[!(AnomalyTable$Ano_ID %in% ToRetain), c("Ano_ID", "AveAnoScore"), with=FALSE])  # Add to AnomalyTabTemp the scores of the cases that will be discarded from AnomalyTable (and will thus not be analyzed anymore in future iterations). 
      AnomalyTable <- AnomalyTable[AnomalyTable$Ano_ID %in% ToRetain, ]   # Delete the 5% rows with the hitherto highest average anomaly score, i.e. discard the cases that at this time seem to be the most normal (most frequent). Note, however, that often more than this percentage will actually be discarded. The reason for this is the fact that normal cases are abundant, and thus many records around the specified quantile have the same AveAnoScore.
      if (TestMode==TRUE) { cat(paste0(" Discarded ", nrow(datset)-nrow(AnomalyTable), " cases (", round(100-nrow(AnomalyTable)/nrow(datset)*100, digits = 2), "%)", " to speed up the next iteration.\n")) }
    } else if (CurrentIteration > StartHeuristicsAfterIteration && StartIteration==TRUE && nrow(AnomalyTable)/nrow(datset) < FractionOfCasesToRetain ) {
      if (nrow(AnomalyTable)/nrow(datset) < SDAnoFraction) { # In this extreme situation the algorithm needs to stop to prevent an endless loop. Almost all cases are discarded (even more than the theoretically expected amount of 'normal' cases), probably because of a highly skewed dataset. After pruning so much, the algorithm may not converge anymore, so we stop it.
        StartIteration = FALSE  
        if (TestMode==TRUE) cat(paste0("\nDiscarded most of the cases in previous pruning action(s) (", round(100-nrow(AnomalyTable)/nrow(datset)*100, digits = 2), "%), so stopping analysis process."))
        PrintResults() # Print final results        
      } else {
        if (TestMode==TRUE) cat(paste0(" Not discarded any cases, as over 80% has already been pruned away.\n")) # Continue analysis without pruning.
      }
    } 
    
  } # End iterations for numerical variables
  
  # Combine the scores of the cases left in AnomalyTable and the discarded cases in AnomalyTabTemp. Note that the scores of the discarded cases are higher than they would have been without pruning, as later iterations could not bring down these scores.
  AnomalyTabTemp <- rbind(AnomalyTabTemp, AnomalyTable[, c("Ano_ID", "AveAnoScore"), with=FALSE])
  AnomalyTabTemp <- AnomalyTabTemp[with(AnomalyTabTemp, order(Ano_ID)), ]  # Sort on Ano_ID
  return(as.data.frame(AnomalyTabTemp))
}

