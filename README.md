Clustering Homework

This is a BIOS 611 homework assignment which utilizes K-means clustering and spectral clustering to answer questions 
and perform tasks on a generated dataset. We will evaluate how these two techniques perform with structure and property changes in 
the data.

There are two tasks in the R script:
Task 1 generates data with specified clusters and creates datasets that move the clusters progressively closer. 
K-means clustering is then performed on the datasets.

Task 2 generates a dataset of concentric shells and performs spectral clustering on the dataset.


Task 1 Interpretations:
The simulation results show that the higher dimensions (true number of clusters) have a higher estimated number of clusters. It also seems that the gap statistic method is able to estimate the correct number of clusters for each dimension but then starts to estimate far lower clusters for all dimensions at side length 3.

It seems the gap statistic method consistently begins to reduce its estimate of the number of clusters at side length 3 for dimensions 3, 4, and 6. The gap statistic method consistently begins to reduce its estimate of the number of clusters at side length 2 for dimensions 2 and 5.


Task 2 Interpretations: