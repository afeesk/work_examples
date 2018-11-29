This sample uses the caret package in R to perform classification of the bank note dataset
avaiable on http://archive.ics.uci.edu/ml/. The dataset is read directly from the website into
R. 

The algorithms investigated here are naive Bayes, k nearest neighbour, Random Forest and 
Decion Tree (C 4.5 leveraging J48 algorithm).

The core is to perform different cross validation methods on the dataset and then compare their
performances leveraging receiver operating characteristic curve (ROC) as the performance metric.

Detailed process and interpretations were provided in the pdf document.
