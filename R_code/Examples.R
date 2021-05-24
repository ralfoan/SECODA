# Ralph Foorthuis - 2017-2021
# This file shows various R examples of running the SECODA algorithm for detecting anomalies in sets with numerical and/or categorical attributes. The examples are used in the publications referenced below.

# The examples illustrate the six types of anomalies (Foorthuis, 2018): 
# I   - Extreme value anomaly 
# II  - Rare class anomaly
# III - Simple mixed data anomaly 
# IV  - Multidimensional numerical anomaly 
# V   - Multidimensional rare class anomaly 
# VI  - Multidimensional mixed data anomaly

# See these papers for more information about the types of anomalies: 1) Foorthuis, R.M. (2021). "On the Nature and Types of Anomalies: A Review of Deviations in Data." arXiv. 2) Foorthuis, R.M. (2018). "A Typology of Data Anomalies." IPMU 2018, the 17th International Conference on Information Processing and Management of Uncertainty in Knowledge-Based Systems, Cádiz, Spain. 
# See www.foorthuis.nl and the following paper for detailed information about the SECODA algorithm: Foorthuis, R.M. (2017). "SECODA: Segmentation- and Combination-Based Detection of Anomalies." In: Proceedings of the 4th IEEE International Conference on Data Science and Advanced Analytics (DSAA 2017), Tokyo, Japan. 
# See this paper for the analysis results of discretization in the context of anomaly detection: Foorthuis, R.M. (2018). "The Impact of Discretization Method on the Detection of Six Types of Anomalies in Datasets." In: Proceedings of the 30th Benelux Conference on Artificial Intelligence (BNAIC 2018), November 8-9 2018, Den Bosch, the Netherlands. 



##### Initialization

# Set folder for data files and R code
DatFolder <- "D:/R/"  # Set your own folder path here. Don't forget to end with a slash character.

# Load libraries
library(foreign) # For loading datasets
library(rgl) # If you have rgl installed, then you can inspect a 3D plot 

# If you have installed the package SECODA you can load it
library(SECODA)

# Alternatively, you can load the SECODA functionality from file
# source(paste0(DatFolder, "SECODA.R")) # This is the regular version, which uses the data.table package for good time performance
# source(paste0(DatFolder, "SECODABaseR.R")) # This is the base R version, which doesn't require data.table (but performs more slowly and offers less options)

# SECODA treats numeric and categorical data differently. Before running SECODA() make sure that the data types are declared correctly. Numeric data should be 'integer' or 'numeric', whereas categorical data should be 'factor', 'logical' or 'character'. 


##### A. Simple univariate set

# Generate data
set.seed(49)
x1 <- rnorm(2360, mean=4, sd=8.0)
hist(x1)  # Show histogram of the distribution. This does not show the anomalies.
hist(x1, breaks=30) # And with more bins.
stripchart(x1) # You can see the anomalies better by a using a stripchart. You can see the distribution somewhat better by using random jitter:  stripchart(x1, method="jitter")
# The two cases on the left and the one on the right are clearly anomalies and should be detected first. Then, as a matter of degree, the more isolated cases are more and more positioned inwards and thus should be detected consequently.

# Run algorithm
AnomalyTable <- SECODA(as.data.frame(x1))  # SECODA expects a data.frame. 
Ano_ID_anoms <- AnomalyTable[AnomalyTable$AveAnoScore <= head(tail(sort(AnomalyTable$AveAnoScore, decreasing=TRUE),n=3), n=1), "Ano_ID"]  # Show 3 most extreme anomalies
stripchart(x1[Ano_ID_anoms], col="red", add=TRUE)
# One can see several Extreme Value Anomalies (Type I), although it should be said that they are not very isolated.

# Create a mixture of two distributions and a very rare value in the middle of the range. 
x2 <- rnorm(8000, mean=120, sd=4)
x2 <- as.data.frame(c(x1, 70, x2)); colnames(x2)[1] <- "ValueA"
hist(x2$ValueA)
hist(x2$ValueA, breaks=30)
hist(x2$ValueA, breaks=60)
stripchart(x2$ValueA)
# The value 70 is also a Type I anomaly, because that type includes extremely rare (also intermediate) univariate values. There are two distributions with significant different means, and the anomaly in the middle apparently is an extreme case of one of those distributions. It should certainly be detected.

# Run algorithm
AnomalyTable <- SECODA(as.data.frame(x2$ValueA))  # SECODA expects a data.frame. Note that, even if datset is a data.frame, you still need the as.data.frame() function if you select only 1 column. This is because R treats a single column of a data.frame as a vector. Run this statement: class(x2); class(x2$ValueA)
# Plot most extreme anomaly
Ano_ID_anoms <- AnomalyTable[AnomalyTable$AveAnoScore <= head(tail(sort(AnomalyTable$AveAnoScore, decreasing=TRUE),n=1), n=1), "Ano_ID"]
stripchart(x2[Ano_ID_anoms, "ValueA"], col="red", cex=2, add=TRUE)
# Plot 2 most extreme anomalies
Ano_ID_anoms <- AnomalyTable[AnomalyTable$AveAnoScore <= head(tail(sort(AnomalyTable$AveAnoScore, decreasing=TRUE),n=2), n=1), "Ano_ID"]
stripchart(x2[Ano_ID_anoms, "ValueA"], col="red", cex=2, add=TRUE)

rm(x1, x2, AnomalyTable, Ano_ID_anoms)



##### B. Mountain

# Load data
Mountain <- read.arff(paste0(DatFolder, "Mountain.arff"))
str(Mountain) # Check dataset structure

# If you have rgl installed, then you can inspect a 3D plot
plot3d(Mountain, col="red", size=4)
# There are 3 isolated anomalies.

# Run algorithm with standard settings
AnomalyTable <- SECODA(Mountain)

# Highlight first anomaly (or more, if the 1st anomaly score has multiple occurrences)
Ano_ID_anoms <- AnomalyTable[AnomalyTable$AveAnoScore <= head(tail(sort(AnomalyTable$AveAnoScore, decreasing=TRUE),n=1), n=1), "Ano_ID"]
points3d(Mountain[Ano_ID_anoms, 1:3], color="black", size=9)
# This highlights the Multidimensional Numerical Anomaly (Type IV), which requires multiple numerical values to be analyzed jointly to be detected.

# Highlight first 4 anomalies
Ano_ID_anoms <- AnomalyTable[AnomalyTable$AveAnoScore <= head(tail(sort(AnomalyTable$AveAnoScore, decreasing=TRUE),n=4), n=1), "Ano_ID"]
points3d(Mountain[Ano_ID_anoms, 1:3], color="black", size=9)
# The two anomalies at the lower and upper scale of X3 are both Extreme Value Anomalies (Type I).

rm(Mountain, AnomalyTable, Ano_ID_anoms)



##### C. Helix

# Load Monochrome Helix
Helix <- read.arff(paste0(DatFolder, "Helix.arff"))[,1:3]
str(Helix) # Check dataset structure

# If you have rgl installed, then you can inspect a 3D plot
plot3d(Helix, col="red", size=4)

# Run algorithm with standard settings
AnomalyTable <- SECODA(Helix)

# plot first 1 anomaly (or more, if the 1st anomaly score has multiple occurrences)
Ano_ID_anoms <- AnomalyTable[AnomalyTable$AveAnoScore <= head(tail(sort(AnomalyTable$AveAnoScore, decreasing=TRUE),n=1), n=1), "Ano_ID"]
points3d(Helix[Ano_ID_anoms, 1:3], color="red", size=10, alpha=0.6)
# This is an Extreme Value Anomaly, since it can be detected by only looking at the extreme values for X1.

# plot first 2 anomalies
Ano_ID_anoms <- AnomalyTable[AnomalyTable$AveAnoScore <= head(tail(sort(AnomalyTable$AveAnoScore, decreasing=TRUE),n=2), n=1), "Ano_ID"]
points3d(Helix[Ano_ID_anoms, 1:3], color="red", size=10, alpha=0.6)
# This highlights a Multidimensional Numerical Anomaly (Type IV), which requires multiple numerical values to be analyzed jointly to be detected.

# plot the other 7 anomalies
Ano_ID_anoms <- AnomalyTable[AnomalyTable$AveAnoScore <= head(tail(sort(AnomalyTable$AveAnoScore, decreasing=TRUE),n=7), n=1), "Ano_ID"]
points3d(Helix[Ano_ID_anoms, 1:3], color="red", size=10, alpha=0.6)
# We see that 'outliers' 3 to 7 are less extreme outlying, perhaps not even anomalies one is interested in. However, visually it is clear why they have a low anomaly score, as they are indeed all outlying and located in less densely populated areas.

rm(Helix, AnomalyTable, Ano_ID_anoms)


# Load color Helix
Helix <- read.arff(paste0(DatFolder, "Helix.arff"))
str(Helix) # Check dataset structure. There is a color attribute now.

# If you have rgl installed, then you can inspect a 3D plot
plot3d(Helix[,1:3], col=Helix$color, size=4)

# Run algorithm with standard settings
AnomalyTable <- SECODA(Helix)

# plot first 1 anomaly (or more, if the 1st anomaly score has multiple occurrences)
Ano_ID_anoms <- AnomalyTable[AnomalyTable$AveAnoScore <= head(tail(sort(AnomalyTable$AveAnoScore, decreasing=TRUE),n=1), n=1), "Ano_ID"]
points3d(Helix[Ano_ID_anoms, 1:3], col=Helix[Ano_ID_anoms,"color"], size=11, alpha=0.5)
# This is a Multidimensional Mixed Data Anomaly (Type VI), since the blue case seems to be misplaced between the red cases.

# plot first 2 anomalies
Ano_ID_anoms <- AnomalyTable[AnomalyTable$AveAnoScore <= head(tail(sort(AnomalyTable$AveAnoScore, decreasing=TRUE),n=2), n=1), "Ano_ID"]
points3d(Helix[Ano_ID_anoms, 1:3], col=Helix[Ano_ID_anoms,"color"], size=11, alpha=0.5)
# This is a Multidimensional Mixed Data Anomaly (Type VI), since the red case seems to be misplaced between the blue cases.

# plot first 3 anomalies
Ano_ID_anoms <- AnomalyTable[AnomalyTable$AveAnoScore <= head(tail(sort(AnomalyTable$AveAnoScore, decreasing=TRUE),n=3), n=1), "Ano_ID"]
points3d(Helix[Ano_ID_anoms, 1:3], col=Helix[Ano_ID_anoms,"color"], size=11, alpha=0.5)
# This is the Multidimensional Numerical Anomaly that was also identified in the previous run.

# plot first 4 anomalies
Ano_ID_anoms <- AnomalyTable[AnomalyTable$AveAnoScore <= head(tail(sort(AnomalyTable$AveAnoScore, decreasing=TRUE),n=4), n=1), "Ano_ID"]
points3d(Helix[Ano_ID_anoms, 1:3], col=Helix[Ano_ID_anoms,"color"], size=11, alpha=0.5)
# This is the Extreme Value Anomaly we have seen before.

# plot 6 anomalies
Ano_ID_anoms <- AnomalyTable[AnomalyTable$AveAnoScore <= head(tail(sort(AnomalyTable$AveAnoScore, decreasing=TRUE),n=6), n=1), "Ano_ID"]
points3d(Helix[Ano_ID_anoms, 1:3], col=Helix[Ano_ID_anoms,"color"], size=11, alpha=0.5)
# These latter cases are less anomalous, but indeed somewhat isolated. 

rm(Helix, AnomalyTable, Ano_ID_anoms)



##### D. NoisyMix

# Load data
NoisyMix <- read.arff(paste0(DatFolder, "NoisyMix.arff"))
str(NoisyMix) # Check dataset structure

# If you have rgl installed, then you can inspect a 3D plot, showing all 5 attributes
plot3d(NoisyMix[NoisyMix$CodeType=="p_3",1:3], col=NoisyMix[NoisyMix$CodeType=="p_3",4], size=3, type="p"); plot3d(NoisyMix[NoisyMix$CodeType=="s_0.3",1:3], col=NoisyMix[NoisyMix$CodeType=="s_0.3",4], size=0.5, type="s", add=TRUE)

# Run algorithm with standard settings
AnomalyTable <- SECODA(NoisyMix)

# plot first anomaly (or more, if the 1st anomaly score has multiple occurrences)
Ano_ID_anoms <- AnomalyTable[AnomalyTable$AveAnoScore <= head(tail(sort(AnomalyTable$AveAnoScore, decreasing=TRUE),n=1), n=1), "Ano_ID"]
points3d(NoisyMix[Ano_ID_anoms, 1:3], col=NoisyMix[Ano_ID_anoms,"CodeColor"], size=20, alpha=0.5)
# This highlights several anomalies that have obtained the lowest score. The green case is a Rare Class Anomaly (Type II), because it's the only green case. The two blue anomalies are Multidimensional Mixed Data Anomalies (Type VI), because - although blue is very normal in the dataset - blue is rarely seen in that area.

# plot first 4 anomalies
Ano_ID_anoms <- AnomalyTable[AnomalyTable$AveAnoScore <= head(tail(sort(AnomalyTable$AveAnoScore, decreasing=TRUE),n=4), n=1), "Ano_ID"]
points3d(NoisyMix[Ano_ID_anoms, 1:3], col=NoisyMix[Ano_ID_anoms,"CodeColor"], size=20, alpha=0.5)
# The two red anomalies are Multidimensional Mixed Data Anomalies (Type VI). The big red one in the middle is a second-order anomaly: a red color is not rare in that numerical area, neither is a big size. And big and red cases are not rare either. However, the combination of red AND big is rare in that specific numerical area.

# Plot only numerical variables
plot3d(NoisyMix[,1:3], col="seagreen", size=3)

# Run algorithm with standard settings only on numerical variables
AnomalyTable <- SECODA(NoisyMix[,1:3])

# Highlight the first 4 anomalies
Ano_ID_anoms <- AnomalyTable[AnomalyTable$AveAnoScore <= head(tail(sort(AnomalyTable$AveAnoScore, decreasing=TRUE),n=4), n=1), "Ano_ID"]
points3d(NoisyMix[Ano_ID_anoms, 1:3], color="black", size=8)
# These are all numerically isolated cases, Type I and Type IV anomalies.

rm(NoisyMix, AnomalyTable, Ano_ID_anoms)



##### E. Categorical anomalies

# Generate data
set.seed(25)
Freq1 <- 420
x1 <- rnorm(Freq1,mean=0, sd=0.7)
x2 <- rnorm(Freq1,mean=0, sd=0.7)
Shape <- sample(1:2, Freq1, replace=T) # Create code value
dat1 <- data.frame(x1, x2, Shape)
dat1$Color <- "red"; dat1[dat1$Shape==1, "Color"] <- "blue"  # Set colors based on Shape
dat1$Shape[303] <- 1 # Turn case 303 into an anomaly by giving it a deviant combination of Shape (1 or a circle) and Color (red): a Multidimensional Rare Class Anomaly (Type V).
dat1 <- rbind(dat1, data.frame(x1=-0.42, x2=0.9, Color="blue", Shape=0)) # Add a new case with a unique shape (0 or a square): a Rare Class Anomaly (Type II).
dat1 <- rbind(dat1, data.frame(x1=-1.9, x2=-0.5, Color="blue", Shape=5)) # And a new case with a unique shape and extreme location (5 or a tilted square in the extreme left): Simple Mixed Data Anomaly (Type III).

# Plot
plot(dat1[,1:2], col=dat1$Color, pch=dat1$Shape)

# Make the Shape attribute categorical, otherwise it will be treated as a continuous variable during the analysis
dat1$Shape <- as.factor(dat1$Shape) # You can optionally omit this step to check what the difference in result for this last case is
str(dat1) # Verify data types. The dataset consists of 2 continuous attributes and 2 categorical attributes
plot(dat1[,1:2], col=dat1$Color, pch=as.numeric(levels(dat1$Shape))[dat1$Shape]) # Note that if you want to plot it, you will have to convert the categories/factors to numbers in the correct way.

# Run SECODA
AnomalyTable <- SECODA(dat1)

# Mark most extreme anomalies
Ano_ID_anoms <- AnomalyTable[AnomalyTable$AveAnoScore <= head(tail(sort(AnomalyTable$AveAnoScore, decreasing=TRUE),n=3), n=1), "Ano_ID"]
points(dat1[Ano_ID_anoms, 1:2], col="black", cex=2.5, lwd=2)
# The Type II, III and V anomalies are indeed detected.
# Case 303 is truly unique as a result of its unique combination of Shape and Color (Type V anomaly). It's a red circle, whereas 
# all other cases are either red triangles or blue circles. Case 421 has a unique shape: a 0 (shown as a square), which makes this a Type II anomaly.
# Case 422 also has a unique shape: a 5 (shown as a tilted square). Because it's also located at an extreme location, it is a Type III anomaly.

# Mark 7 most extreme anomalies
AnomalyTable <- SECODA(dat1, MinimumNumberOfIterations = 9)  # Due to the small dataset SECODA only needed a couple of iterations to detect the true anomalies in the previous round. If you're interested in a good estimation of weak anomalies (statistical noise cases), it's best to force SECODA to run more iterations for improved precision. This gives a better impression of cases that are (somewhat) anomalous due to them being scattered and having a relatively rare class value or rare class value combination.
Ano_ID_anoms <- AnomalyTable[AnomalyTable$AveAnoScore <= head(tail(sort(AnomalyTable$AveAnoScore, decreasing=TRUE),n=7), n=1), "Ano_ID"]
plot(dat1[,1:2], col=dat1$Color, pch=as.numeric(levels(dat1$Shape))[dat1$Shape]); points(dat1[Ano_ID_anoms, 1:2], col="black", cex=2.5, lwd=2)
# Now the cases that are more outlying (isolated) are also shown.

rm(dat1, AnomalyTable, Ano_ID_anoms, Freq1, Shape, x1, x2)



##### F. TimeSeries

# Load data
TimeSeries <- read.arff(paste0(DatFolder, "TimeSeries.arff"))
str(TimeSeries) # Check dataset structure

# Plot the timeseries
plot(1, type="n", xlab="Month", ylab="Average Wage", xlim=range(TimeSeries$Time), ylim=range(TimeSeries$AverageWage)) # Create empty plot
lines(TimeSeries$Time,TimeSeries$AverageWage,lwd=1,col="green4") # Draw a smooth line 
points(TimeSeries$Time,TimeSeries$AverageWage, pch=20, cex=0.5, lwd=1,col="green4")  # Also show the actual datapoints in the plot

# Run algorithm with standard settings
AnomalyTable <- SECODA(TimeSeries)
# Plot anomaly
Ano_ID_anoms <- AnomalyTable[AnomalyTable$AveAnoScore <= head(tail(sort(AnomalyTable$AveAnoScore, decreasing=TRUE),n=1), n=1), "Ano_ID"]
points(TimeSeries[Ano_ID_anoms, ], col="Red", cex=1.7, lwd=2)
# The anomaly is detected. It's a Multidimensional Numerical Anomaly (Type IV), only deviant in its neighborhood (i.e. in its own time period or season).

# Note that this detection is based on the anomalous case(s) being isolated. There is also a simple way to use SECODA by explicitly taking into 
# account the sequence of the data points. See the section below about the proper way to conduct a (simple) change point detection for level shifts.


# Extend data
TimeSeries <- TimeSeries[3:398,]; TimeSeries$AverageWage[183] <- mean(TimeSeries$AverageWage[182], TimeSeries$AverageWage[184]) # Correct anomaly by imputation
year.grid = TimeSeries$Time + 32.3; TimeSeries <- rbind(TimeSeries, data.frame("Time"=year.grid, "AverageWage"=TimeSeries$AverageWage)) # Extend existing time series
plot(TimeSeries, lwd=2, type="l", col="green4") # Plot total series as green line.
TimeSeriesLS <- TimeSeries # Store for later use.

# Add Extreme Value Anomaly (in time series analysis this also known as an additive outlier, which represents an isolated spike)
TimeSeries$AverageWage[183] = 4500; TimeSeries$AverageWage[182] = 3900; TimeSeries$AverageWage[184] = 3900
plot(TimeSeries, lwd=2, type="l", col="green4", xlab="Time") # Plot total series as green line

# Run algorithm
AnomalyTable <- SECODA(TimeSeries)
Ano_ID_anoms <- AnomalyTable[AnomalyTable$AveAnoScore <= head(tail(sort(AnomalyTable$AveAnoScore, decreasing=TRUE),n=1), n=1), "Ano_ID"]
points(TimeSeries[Ano_ID_anoms, ], col="Red", cex=1.6, lwd=2)
# The isolated spike is correctly identified as an anomaly

# Add new Extreme Value Anomaly
TimeSeries$AverageWage[605] = 1750; TimeSeries$AverageWage[604] = 1900; TimeSeries$AverageWage[606] = 1900  # This is another extreme value anomaly (additive outlier)
plot(TimeSeries, lwd=2, type="l", col="green4") # Plot total series as green line
# And optionally turn this one into what in time series analysis is called an transitory change outlier, of which the impact takes relatively long to disappear:
IOchange = seq(from=800, to=0, length.out=length(TimeSeries$AverageWage[606:678])) # Create diminishing effect that will be added to the existing timeline
IOchange[2:20] = IOchange[2:20] + seq(from=500, to=0, length.out=length(2:20)); TimeSeries$AverageWage[606:678] = TimeSeries$AverageWage[606:678] - IOchange; TimeSeries$AverageWage[606] = 1370  # Some improvements
plot(TimeSeries, lwd=2, type="l", col="green4", xlab="Time") # Plot total series as green line

# Run anomaly detection
AnomalyTable <- SECODA(TimeSeries)
Ano_ID_anoms <- AnomalyTable[AnomalyTable$AveAnoScore <= head(tail(sort(AnomalyTable$AveAnoScore, decreasing=TRUE),n=6), n=1), "Ano_ID"]
points(TimeSeries[Ano_ID_anoms, ], col="Red", cex=1.6, lwd=2)
# The most extreme anomalies are indeed the additive and innovational outliers. These are both Extreme Value Anomalies, and easy to spot.
# Extreme Value Anomalies are in essence univariate. So these can also be detected by only analyzing the AverageWage attribute:
plot(TimeSeries, lwd=2, type="l", col="green4", xlab="Time") # Replot
AnomalyTable <- SECODA(as.data.frame(TimeSeries$AverageWage))
Ano_ID_anoms <- AnomalyTable[AnomalyTable$AveAnoScore <= head(tail(sort(AnomalyTable$AveAnoScore, decreasing=TRUE),n=6), n=1), "Ano_ID"]
points(TimeSeries[Ano_ID_anoms, ], col="Red", cex=1.6, lwd=2)


# Create sudden but structural shift in level
year.grid = TimeSeriesLS$Time + 64.4; TimeSeriesLS <- rbind(TimeSeriesLS, data.frame("Time"=year.grid, "AverageWage"=TimeSeriesLS$AverageWage)) # First create longer series
TimeSeriesDC <- TimeSeriesLS # Store for later
TimeSeriesLS$AverageWage[length(year.grid):length(TimeSeriesLS$AverageWage)] = TimeSeriesLS$AverageWage[length(year.grid):length(TimeSeriesLS$AverageWage)] + 8000  # Create level shift
plot(TimeSeriesLS, lwd=2, type="l", col="green4", xlab="Time") # Plot total series as green line

# Run algorithm
AnomalyTable <- SECODA(TimeSeriesLS)
Ano_ID_anoms <- AnomalyTable[AnomalyTable$AveAnoScore <= head(tail(sort(AnomalyTable$AveAnoScore, decreasing=TRUE),n=2), n=1), "Ano_ID"]
points(TimeSeriesLS[Ano_ID_anoms, ], col="Red", cex=1.6, lwd=2)
# No real awkward cases yet
Ano_ID_anoms <- AnomalyTable[AnomalyTable$AveAnoScore <= head(tail(sort(AnomalyTable$AveAnoScore, decreasing=TRUE),n=4), n=1), "Ano_ID"]
points(TimeSeriesLS[Ano_ID_anoms, ], col="Red", cex=1.6, lwd=2)
# Here the change point is detected. But: general-purpose density-based algorithms like SECODA, which do not explicitly acknowledge the time 
# (or sequence) nature of the data, cannot be expected to conduct proper time series analysis and thus not to 'magically' detect such change 
# point anomalies. However, they can apparently still perform reasonably well, considering the obvious anomaly (the level shift moment) is 
# singled out as one of the most extreme outliers. What you also see here is that you can expected more false positives, though.

# Note, however, that a more proper change point analysis can easily be conducted! Simply calculate the difference between two time points.
TimeSeriesLS$Difference <- c(NA, TimeSeriesLS[2:nrow(TimeSeriesLS), 2] - TimeSeriesLS[1:nrow(TimeSeriesLS)-1, 2])  # Vector subtraction to determine the difference. See below for more on this.
plot(TimeSeriesLS[c(1,3)]) # There clearly is one outlier in terms of changes between two time points. This is the level shift moment.

# The new Difference variable can now be analyzed on extreme values
AnomalyTable <- SECODA(as.data.frame(TimeSeriesLS$Difference))
Ano_ID_anoms <- AnomalyTable[AnomalyTable$AveAnoScore <= head(tail(sort(AnomalyTable$AveAnoScore, decreasing=TRUE),n=2), n=1), "Ano_ID"]
points(TimeSeriesLS[Ano_ID_anoms, c(1,3)], col="Red", cex=1.8, lwd=2) # There are 2 anomalies. The obvious extreme outlier and one empty case (NA). This latter concerns the first row, and is the result of the fact that the difference cannot be calculated for the first case. Since it's an NA it will not be shown in the plot.
# Plot in the original set
Ano_ID_anoms <- AnomalyTable[(AnomalyTable$AveAnoScore <= head(tail(sort(AnomalyTable$AveAnoScore, decreasing=TRUE),n=2), n=1)) & (AnomalyTable$Ano_ID > 1), "Ano_ID"] # Make new selection, ignoring the first row with the NA.
plot(TimeSeriesLS[,1:2], lwd=2, type="l", col="green4", xlab="Time") # Plot total series as green line
points(TimeSeriesLS[Ano_ID_anoms, 1:2], col="Red", cex=1.8, lwd=2) 
# This is a Multidimensional Numerical Anomaly (Type IV), and can only be detected by taking into account both numerical attributes However, after the
# data transformation (calculating the difference), the detection task is simplified to finding an Extreme Value Anomaly on the new difference variable.

# We can also use this solution for the other analyses we conducted. Let's revisit the first series.
TimeSeriesRevisited <- read.arff(paste0(DatFolder, "TimeSeries.arff"))  # Reload the original series
plot(1, type="n", xlab="Month", ylab="Average Wage", xlim=range(TimeSeriesRevisited$Time), ylim=range(TimeSeriesRevisited$AverageWage)); lines(TimeSeriesRevisited$Time,TimeSeriesRevisited$AverageWage,lwd=1,col="green4") # Draw a smooth line 
TimeSeriesRevisited$Difference <- c(NA, TimeSeriesRevisited[2:nrow(TimeSeriesRevisited), 2] - TimeSeriesRevisited[1:nrow(TimeSeriesRevisited)-1, 2]) # Use vector subtraction to determine the difference between two sequential points in time.
AnomalyTable <- SECODA(as.data.frame(TimeSeriesRevisited$Difference))
Ano_ID_anoms <- AnomalyTable[(AnomalyTable$AveAnoScore <= head(tail(sort(AnomalyTable$AveAnoScore, decreasing=TRUE),n=2), n=1)) & (AnomalyTable$Ano_ID > 1), "Ano_ID"] # Make selection, ignoring the first row with the NA.
points(TimeSeriesRevisited[Ano_ID_anoms, 1:2], col="Red", cex=1.8, lwd=2) 
# This detects the largest change in two sequential points in time. You could also take more points into account and, e.g., calculate the average change.


# Create Deviant Cycle Anomaly
# Here the cyclical (e.g. seasonal) pattern is temporarily distorted. This is a more complex problem, since the anomaly is at the level of a cycle, not at the level of an individual point. It's a set of individual points that is anomalous as a group.
year.grid = TimeSeriesDC$Time + 128.8
TimeSeriesDC <- rbind(TimeSeriesDC, data.frame("Time"=year.grid, "AverageWage"=TimeSeriesDC$AverageWage))
plot(TimeSeriesDC, lwd=2, type="l", col="green4", xlab="Time", ylim=c(1700,4000)) # Plot total series as green line
# Create anomalous cycle:
TimeSeriesDCano <- TimeSeriesDC; TimeSeriesDCano$AverageWage[1647:2000] = TimeSeriesDCano$AverageWage[2000] + (TimeSeriesDCano$AverageWage[1647:2000]/50 - TimeSeriesDCano$AverageWage[2000]/50)
plot(TimeSeriesDCano, lwd=2, type="l", col="green4", xlab="Time", ylim=c(1700,4000))

# Regular SECODA or other density-based algorithms will not work here. If you have Twitter's Anomaly Detection installed, then you can try using that:
# AnomalyDetectionTs(TimeSeriesDCano[,c(3,2)], direction = "both", plot = TRUE)$plot
# However, that does not seem to work either, as only extreme values are detected.

# We will therefore construct our own statistical model. In the process it's shown that these complex anomalies can still be detected by relatively simple models.
# The logic behind this is that we train a model that learns the normal pattern. Deviations of new data from this normal pattern are then denoted as anomalies.
# First train the model on the normal pattern:
TimeSeriesDC$Seq <- 1:nrow(TimeSeriesDC)
# TimeSeriesDC$AverageWage <- TimeSeriesDC$AverageWage + TimeSeriesDC$Seq/10  # To make it more interesting you can add a long-term trend (but you can also skip this step)
TimeSeriesDC$AverageWage_1Ago <- c(NA, TimeSeriesDC$AverageWage[1:(nrow(TimeSeriesDC)-1)]) # Insert new variable with value from previous state. Although we can avoid the NA's we'll accept a few of them because we have sufficient data
TimeSeriesDC$AverageWage_3Ago <- c(NA, NA, NA, TimeSeriesDC$AverageWage[1:(nrow(TimeSeriesDC)-3)])
TimeSeriesDC$AverageWage_10Ago <- c(rep(NA,10), TimeSeriesDC$AverageWage[1:(nrow(TimeSeriesDC)-10)])
TimeSeriesDC$AverageWage_40Ago <- c(rep(NA,40), TimeSeriesDC$AverageWage[1:(nrow(TimeSeriesDC)-40)])
TimeSeriesDC$AverageWage_396Ago <- TimeSeriesDC$AverageWage  # The cycle is 396 rows long, so this is the same as the existing Value (assuming the trend was not added). No need to introduce NA's here.
TimeSeriesDC$AverageWage_396Ago <- rnorm(nrow(TimeSeriesDC), mean=TimeSeriesDC$AverageWage_396Ago, sd=0.1)  # Since we created the cycles by re-using old ones, we add some random variation for realism. Also, this results in some difference with AverageWage_792Ago, which otherwise would be an identical predictor and result in multicollinearity (and ignoring AverageWage_792Ago during model building). 
TimeSeriesDC$AverageWage_792Ago <- TimeSeriesDC$AverageWage  
TimeSeriesDC$AverageWage_792Ago <- rnorm(nrow(TimeSeriesDC), mean=TimeSeriesDC$AverageWage_792Ago, sd=0.2)  # We add some random variation for realism and to prevent ignoring AverageWage_792Ago during fitting. Because we add more variation than for AverageWage_396Ago, we also make sure AverageWage_792Ago (i.e. the data from two cycles ago) is less important than AverageWage_396Ago (i.e. the most recent cycle), and thus gets a lower weight in the model.
plot(TimeSeriesDC[,1:2], lwd=2, type="l", col="green4", xlab="Time", ylim=c(1700,4000))  # We will train on this normal pattern

# Fit linear model (regular linear regression)
glm.fit <- glm(AverageWage ~ AverageWage_1Ago + AverageWage_3Ago + AverageWage_10Ago + AverageWage_40Ago + AverageWage_396Ago + AverageWage_792Ago, family=gaussian, data = TimeSeriesDC)
summary(glm.fit)
# We see that the AverageWage_396Ago variable has the largest weight, which means that the respective data point of the previous cycle has the most predictive power.

# Now prepare the dataset for actual anomaly detection
TimeSeriesDCano$Seq <- 1:nrow(TimeSeriesDCano)
# TimeSeriesDCano$AverageWage <- TimeSeriesDCano$AverageWage + TimeSeriesDCano$Seq/10  # To make it more interesting you can add a long-term trend
TimeSeriesDCano$AverageWage_1Ago <- c(NA, TimeSeriesDCano$AverageWage[1:(nrow(TimeSeriesDCano)-1)])
TimeSeriesDCano$AverageWage_3Ago <- c(NA, NA, NA, TimeSeriesDCano$AverageWage[1:(nrow(TimeSeriesDCano)-3)])
TimeSeriesDCano$AverageWage_10Ago <- c(rep(NA,10), TimeSeriesDCano$AverageWage[1:(nrow(TimeSeriesDCano)-10)])
TimeSeriesDCano$AverageWage_40Ago <- c(rep(NA,40), TimeSeriesDCano$AverageWage[1:(nrow(TimeSeriesDCano)-40)])
TimeSeriesDCano$AverageWage_396Ago <- TimeSeriesDCano$AverageWage  # The cycle is 396 long, so this is the same (assuming the trend was not added). No need to introduce NA's
TimeSeriesDCano$AverageWage_396Ago <- rnorm(nrow(TimeSeriesDC), mean=TimeSeriesDCano$AverageWage_396Ago, sd=0.1)  # Add random variation
TimeSeriesDCano$AverageWage_792Ago <- TimeSeriesDC$AverageWage
TimeSeriesDCano$AverageWage_792Ago <- rnorm(nrow(TimeSeriesDC), mean=TimeSeriesDCano$AverageWage_792Ago, sd=0.2)  # Add random variation
plot(TimeSeriesDCano[,1:2], lwd=2, type="l", col="green4", xlab="Time", ylim=c(1700,4000))  
# This is the sequence we want to analyze.

# Now conduct anomaly detection
TimeSeriesDCano$Prediction <- predict(glm.fit, newdata = TimeSeriesDCano[, 4:9]) # Predict the value
TimeSeriesDCano$Difference <- abs(TimeSeriesDCano$AverageWage - TimeSeriesDCano$Prediction) # Calculate difference between prediction and observed real value
# Show 200 most extreme anomalies:
Ano_ID_anoms <- na.omit(TimeSeriesDCano[TimeSeriesDCano$Difference >= head(tail(sort(TimeSeriesDCano$Difference, decreasing=FALSE),n=200), n=1), "Seq"])
plot(TimeSeriesDCano[,1:2], lwd=2, type="l", col="green4", xlab="Time", ylim=c(1700,4000))
points(TimeSeriesDCano[Ano_ID_anoms, 1:2], col="Red", cex=1.7, lwd=2) 
# These are indeed all in the anomalous range. You can also set a threshold, e.g. 40:
Ano_ID_anoms <- na.omit(TimeSeriesDCano[TimeSeriesDCano$Difference >= 40, "Seq"])
plot(TimeSeriesDCano[,1:2], lwd=2, type="l", col="green4", xlab="Time", ylim=c(1700,4000))
points(TimeSeriesDCano[Ano_ID_anoms, 1:2], col="Red", cex=1.7, lwd=2) 
# This points to the anomalous range as well.
# You do have to know the cycles beforehand, so that the model is able to learn that the new value can be predicted by the value of the previous 
# cycle, and perhaps some other (more or less recent) values.


rm(TimeSeries, TimeSeriesDC, TimeSeriesDCano, TimeSeriesLS, TimeSeriesRevisited, glm.fit, AnomalyTable, Ano_ID_anoms, IOchange, year.grid)

