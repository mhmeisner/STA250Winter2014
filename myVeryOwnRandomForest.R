myRandomForest = function(form,d,ntrees=500,split_n=2,min_bin_size){
	# form: formula object, standard notation
	# d: data frame, predictor variables in form must have column with same name in d
	# ntrees = number of trees in the forest
	# split_n = number of predictor variables to consider splitting on at each node
	# min_bin_size = number of training examples in each bin at which we stop splitting that bin 
	response = as.character(form)[2]
	predictors = strsplit(as.character(form)[[3]],' \\+ ')[[1]]
	
	trees = sapply(1:ntrees,function(i){
		# sample rows of d with replacement to get dataset to use for the ith tree 
		w = sample(1:nrow(d),replace=T)
		dd = d[w,] 
		growTree(dd,response,predictors,split_n,min_bin_size)
	})
	
}


getOOBError = function()

growTree = function(d,response,predictors,split_n,min_bin_size){
	current_bins = list(1:nrow(d))
	current_bin_sizes = nrow(d)
	w = current_bin_sizes>min_bin_size
	while(sum(w)>0){
		for()
	}
	
}

makeNode = function(d,response,predictors,split_n){
	some_preds = sample(predictors,split_n) # stochastically choose subset of variables to consider 
	g = getBestSplit(d,response,some_preds)
	
}



d = data.frame(y=c(5,6,7,7,7),x1=c(2,3,3,3,3),x2=c(6,6,1,33,1),x3=c(1,1,2,2,2))

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

# cat response, cat predictor (need to consider all possible subsets of categorical predictors for split? )
# cat response, cont predictor
# cont response, cat predictor
# cont respones, cont predictor 


# simulate CV stuff to show my reasoning is valid!  

a = rnorm(5)
b = rnorm(5)
lm('a~b')
lm(a~b)

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