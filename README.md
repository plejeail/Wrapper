## wrap: An implementation of wrappers (features selection) in a R package
The wrapper is an algorithm which goal is to fined the best subset of features to modelize the target. 
The core of the wrapper approach is to train and test differents models (changing features) and then select the best one.
We implemented several wrapper algorithm in a function, named wrapper. 
### The wrapper search algorithms supported :  
- Ascending 
- Descending
- Genetic (from package genalg)

planned on update: 
- Hill-Climbing 

### Supported statistical methods
Those methods are included in the wrapper function and can be used very easily. 
If your data mining algorithm is not here, you can use it by creating your own function of evaluation. 
For more informations, look at the documentation.
- Discriminant Analysis         (lda/qda {MASS})
- Linear regression             (lm {stats}) 
- Logistic regression           (glm {stats}, family = "binomial") 
- SVM                           (svm {e1071})
- Naive bayes                   (naiveBayes {e1071})
- Random forest                 (randomForest {randomForest})

no update is planned for now as most of the used methods are represented here.
### Validation method  
- Currently the only method integrated is the basic training/set

You can use another method with a special evaluation function, but be aware that the wrappers are very heavy, so you may not want to abuse of cross validation and bootstrap, espacially if you have a lot of features.<br />
Planed:
- Integration of Cross Validation
- Integration of Bootstrap

### Data
We provide several free dataset (that we didn't create) in order to test the package and play with it.
- boston
- diabete
- income
- ozone

### Authors
- Myself
- Maïté Garcia (https://linkedin.com/in/garciamaite)
- Jean-Luc Yongwe (https://linkedin.com/in/yongwe-jean-luc-a63b82a5, https://github.com/yikkin)
