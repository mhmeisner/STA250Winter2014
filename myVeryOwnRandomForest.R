myRandomForest = function(form,d,ntrees=500,split_n=2,min_bin_size){
	# form: formula object, standard notation
	# d: data frame, predictor variables in form must have column with same name in d
	# ntrees = number of trees in the forest
	# split_n = number of predictor variables to consider splitting on at each node
	# min_bin_size = number of training examples in each bin at which we stop splitting that bin (not implemented in this version )
	response = as.character(form)[2]
	
	predictors = strsplit(as.character(form)[[3]],' \\+ ')[[1]]
	
	trees_and_errors = sapply(1:ntrees,function(i){
		# sample rows of d with replacement to get dataset to use for the ith tree 
		w = sample(1:nrow(d),replace=T)
		oob_indices = (1:nrow(d))[!(1:nrow(d) %in% w)]
		oob_d = d[oob_indices,]
		dd = d[w,] 
		tree = growTree(dd,response,predictors,split_n,min_bin_size)
		oob_error = getOOBError(tree,oob_d,response)
		list(tree,oob_error)
	})
	
	# get estimate of oob error
	oob_error = mean(sapply(1:ntrees,function(i){
		trees_and_errors[2,i][[1]]
	}))	
	list_of_trees = trees_and_errors[1,]
	
	# return error and trees so we can predict 
	list(list_of_trees,oob_error)
}


getOOBError = function(t,d,response){
	# given a tree (a list with 1st element the variable, 2nd element the cutoff, 3rd element the prediction if variable is less than the cutoff, 4th the prediction if greater than cutoff), and a oob data frame d, calculates MSE of the model predicting response for d
	var = d[,t[[1]]]
	pred = ifelse(var<as.numeric(t[[2]]),as.numeric(t[[3]]),as.numeric(t[[4]]))
	actual = d[,response]
	mse = mean((actual-pred)**2)
	mse
	
}

growTree = function(d,response,predictors,split_n,min_bin_size){
	#current_bins = list(1:nrow(d))
	#current_bin_sizes = nrow(d)
	m = makeNode(d,response,predictors, split_n)
	predictionIfLessThanCutoff = mean(d[,response][d[,m[[1]]]<m[[2]]])
	predictionIfGreaterThanCutoff = mean(d[,response][d[,m[[1]]]>m[[2]]])
	as.list(c(unlist(m),predictionIfLessThanCutoff,predictionIfGreaterThanCutoff))
}


growTree(d,'y',c('x1','x2','x3'),2,3)

makeNode = function(d,response,predictors,split_n){
	some_preds = sample(predictors,split_n) # stochastically choose subset of variables to consider 
	g = getBestSplit(d,response,some_preds)
	g
}





getBestSplit = function(d,response,predictors){
	# assume predictor is continuous and response is continuous 
	# searches through all predictors. For each, finds the best split. Returns a list with [[1]] the name of the best variable to split on (as a character of the variable name), and [[2]] the decision value for that variable 
	n = nrow(d)
	actual = d[,response]
	mse_pred_splitvalue = sapply(predictors,function(p){
		# tries every predictor, finds the best split for that predictor.  Returns list with the mse (we'll later select the smallest), the name of the predictor, and the value for the decision rule (predictor<splitvalue)
		pred = d[,p]
		
		# need to get candidate values to split on: 
		candidate_split_points = getCandidateSplitPoints(pred)
		
		# find mse when using each candidate split: 
		errs = sapply(candidate_split_points,function(i){
			grp1 = actual[pred<i]
			grp2 = actual[pred>=i]
			mse = sum((grp1-mean(grp1))**2,(grp2-mean(grp2))**2)/length(actual)
			mse
		})
		w = which.min(errs)
		predictor_splitvalue = candidate_split_points[w]
		list(min(errs),p,predictor_splitvalue)
		
	})
	min = which.min(mse_pred_splitvalue[1,]) # find idx for which predictor had best split 
	mse_pred_splitvalue[,min][2:3]
}

g = getBestSplit(d,'y',c('x1','x2','x3'))

getCandidateSplitPoints = function(x){
	# given values of a continuous predictors, finds the values y_1,...,y_k s.t. all candidate splits to try should be of the form x<y_i. Uses midpoints between unique values. 
	u = sort(unique(x))
	sapply(1:(length(u)-1),function(i){mean(c(u[i],u[i+1]))})
}

getCandidateSplitPoints(x)


# make function for prediction:
predictRandomForest = function(trees,d){
	predictions = sapply(trees,function(tree){
		sapply(1:nrow(d),function(i){
			var = d[,tree[[1]]][i]
			pred = ifelse(var<as.numeric(tree[[2]]),as.numeric(tree[[3]]),as.numeric(tree[[4]]))
			pred
		})
	})
	rowMeans(predictions)
}

# testing code 
d = data.frame(y=rnorm(100),x1=rnorm(100,y,1),x2=rnorm(100,y,2),x3=rnorm(100))
f = myRandomForest(y~x1+x2+x3,d,100,2,3)
length(f)
f[[1]]
f[[2]] # oob error 

# simulate new data under in same way, measure predictive performance: 
d = data.frame(y=rnorm(100),x1=rnorm(100,y,1),x2=rnorm(100,y,2),x3=rnorm(100))
p = predictRandomForest(f[[1]],d)
plot(d$y,p,xlab='Actual',ylab='Predicted',main='Predicted vs. Actual: Simulated Data with myRandomForest')

# explore how to extract vars from formula object: 
f = formula('b~a+tt+dfadf')
class(f)
strsplit(as.character(f)[[3]],' \\+ ')[[1]]

# environment issue exploration: 
a = function(x,y,z){
	print(x)
	#x = 500
	b(y,z)
}


b = function(y,z){
	print(x)
}
x = 4
a(56,3,4)

lala = 10
a = function(x){
	lala = 2
	sapply(1:5,function(j){
		print(lala)
	})
}
a(4)