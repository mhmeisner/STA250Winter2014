myRandomForest = function(form,d,ntrees=500,split_n=3){
	# form: formula object, standard notation
	# d: data frame, predictor variables in form must have column with same name in d
	# ntrees = number of trees in the forest
	# split_n = number of predictor variables to consider splitting on at each node
	
}


getOOBError = function()

fitTree = function(d,response,predictors){
	
}


getBestSplit = function(d,response,predictors){
	# assume predictor is categorical and response is continuous 
	n = nrow(d)
	sapply(predictors,function(p){
		
	})
	
}

# cat response, cat predictor (need to consider all possible subsets of categorical predictors for split? )
# cat response, cont predictor
# cont response, cat predictor
# cont respones, cont predictor 


# simulate CV stuff to show my reasoning is valid!  

a = rnorm(5)
b = rnorm(5)
lm('a~b')
lm(a~b)

f = formula('a~b')
class(f)
