# SECODA
Anomaly detection with SECODA for the R environment. SECODA is a general-purpose unsupervised non-parametric anomaly detection algorithm for datasets containing numerical and/or categorical attributes.

## A. Version
SECODA version 0.5.4 - 20200228

## B. FILES
This project includes several types of files:
* SECODA implementations for R (see below)
* Example datasets and code (run the code in "Examples.R")
* Publications, Help/Description files, version information and links to other resources

## C. SECODA SOFTWARE
This project contains several implementations of SECODA:
1. The regular SECODA as a code file that can be executed by R.
1. The regular SECODA as an R package that can be installed in the R environment. 
1. The SECODA base R version as a code file, which runs more slowly than the regular implementation (but does not need the data.table package). The results are identical to the regular version. Note, however, that this base R version does not offer all options.

This software should be regarded as a beta version, and as such will undoubtedly still contain bugs. 

Help improve this algorithm and software by sending comments to Ralph Foorthuis (ralphfoorthuis@gmail.com). 
If you report a bug in the code, please also provide a small dataset and the statements to reproduce
the error. 

I would also appreciate it if you send me comments about how SECODA did (or did not) help you in your analysis.

The software in this container is licensed under a GNU AFFERO GENERAL PUBLIC LICENSE VERSION 3.

See the Help file, documents or "Examples.R" for information on using the SECODA() algorithm in R.

See the following references for background information on SECODA and the typology of anomalies:
* Foorthuis, R.M. (2017). [SECODA: Segmentation- and Combination-Based Detection of Anomalies](https://arxiv.org/abs/2008.06869). In: Proceedings of the 4th IEEE International Conference on Data Science and Advanced Analytics (DSAA 2017), Tokyo, Japan.
* Foorthuis R.M. (2019). [All or In-cloud: How the Identification of Six Types of Anomalies is Affected by the Discretization Method](https://tunguska.home.xs4all.nl/Publications/Docs/All%20or%20In-cloud.%20How%20the%20Identification%20of%20Six%20Types%20of%20Anomalies%20is%20Affected%20by%20the%20Discretization%20Method%20-%20Foorthuis.pdf). In: Atzmueller M., Duivesteijn W. (eds). Artificial Intelligence. BNAIC 2018. Springer, Communications in Computer and Information Science, Vol. 1021, pp 25-42. DOI: 10.1007/978-3-030-31978-6_3
* Foorthuis R.M. (2021). [On the Nature and Types of Anomalies: A Review of Deviations in Data](https://link.springer.com/content/pdf/10.1007/s41060-021-00265-1.pdf). International Journal of Data Science and Analytics, DOI: 10.1007/s41060-021-00265-1
* Foorthuis, R.M. (2018). [A Typology of Data Anomalies](https://tunguska.home.xs4all.nl/Publications/Docs/A%20Typology%20of%20Data%20Anomalies%20-%20Foorthuis%20-%20IPMU%202018.pdf). In: Proceedings of IPMU 2018, the 17th International Conference on Information Processing and Management of Uncertainty in Knowledge-Based Systems, Cádiz, Spain. 
* Foorthuis, R.M. (2017). [Anomaly Detection with SECODA](https://tunguska.home.xs4all.nl/Publications/Docs/Anomaly%20Detection%20with%20SECODA.pdf). Poster Presentation at the 4th IEEE International Conference on Data Science and Advanced Analytics (DSAA 2017), Tokyo, Japan.
* Foorthuis, R.M. (2017). [The SECODA Algorithm for the Detection of Anomalies in Sets with Mixed Data](https://tunguska.home.xs4all.nl/Publications/Docs/The%20SECODA%20Algorithm%20for%20the%20Detection%20of%20Anomalies%20in%20Sets%20with%20Mixed%20Data.pdf). Presentation on www.foorthuis.nl

