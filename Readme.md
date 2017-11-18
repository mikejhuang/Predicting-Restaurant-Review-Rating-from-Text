# Yelp Data Challenge: Predict Restaurant Rating from Review Text
_________________________________________________________________________
## About this project
This is part of the Yelp Data Challenge that you can find here: https://www.yelp.com/dataset/challenge
My goal is to determine the features that is most predictive of a rating. This would allow restaurants to gain meaningful insight on the kinds of customer experiences that corresponds to ratings. What is the more important, service or food?
This project introduces a method for finding the most informative features for scale based classification (rating of 1-5). 
Classification is then performed with SVM. 

## Description of files
* `FilterRestaurants.R`Subset only restaurant reviews from the dataset
* `generatewordclouds.R` Exploratory analysis with word clouds for each rating
* `correlationwordlist.R` For a list of features in term document matrix form, assign a Coincidence Strength Factor score 
to quantify how informative a feature is (see yelpdatachallengecoincidence.pdf for an explanation)
`naivebayesevensample2.R` Use naive bayes classifier to predict ratings
`svmmodel2.R` Use SVM classifier to predict ratings

## Author

Mike Huang, contact: astroguy@gmail.com