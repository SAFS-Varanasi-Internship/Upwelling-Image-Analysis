# Upwelling-Image-Analysis
Analysis of SST data for upwelling patterns

## First

* code process CSV which...
* Read in data which is currently all in csvs.
* Have to deal with rgb values, so reshape to have those so each image is one line as all the images are in the same csv, will have to do a loop or something I think to process it

## Second

* code the find closest centriod so each image finds its closest centroid
* code the compute centroid code to compute new centroids of those groupings
* code the random initialize centroid to set centroids as random k images

## Thirdly

* code multtiple random initializations code, which will run the algorithm with multiple random initializations...
* return the random initiazation with the lowest cost

## Fourthly

* ode find K which will loop through a range of Ks (2-12?)
* plot the cost...
* see if the elbow method works well.
* chose the K that looks the best

Fifthly
* IF I NEED TO GET RID OF NANs
* ...im(isnan(im)=0
