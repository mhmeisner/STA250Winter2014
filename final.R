library(parallel)
library(e1071)
library(randomForest)
load('~/d_cot20.rda')



forwardSVMBlockCVParallel = function(d,predictor_vars,response_var,cv_var,cluster,n_var){
	# d: data frame containing data
	# predictor_vars: character vector of column names of features to be considered in feature selection process
	# response_var: what we want to predict (character of column name)
	# cv_var: every unique value of this column will be used to determine a test set for k-fold CV (and instances with that unique value will be used as the test set during the corresponding fold)
	# n_var: num features in largest model we will consider 
	unique_cv_vars = unique(d[,cv_var])
	cat(unique_cv_vars)
	cat('Cluster Applying! Go relax.')
	cv_results = clusterApply(cluster,unique_cv_vars,withinBlockFeatureSelect,d=d,vars=predictor_vars,n_var=n_var,response_var=response_var,cv_var=cv_var)
	cv_results

}

# fxn to run within each CV loop 
withinBlockFeatureSelect = function(cv_var_unique,d,vars, n_var, response_var,cv_var){
	# n_var is the number of features in the largest model we will build
	# this function takes a training set, breaks it into blocks based on year, and uses these yearly blocks to do CV in order to select features
	test_indices = d[,cv_var]==cv_var_unique
	dd_test = d[test_indices,] 
	dd = d[!test_indices,] # this is the training set for the kth fold of the outermost loop of k-fold CV.  We will now perform many rounds of k-1 fold CV within this set to determine features we add to the model: 
	
	# candidate variables that could be added to the model (starts with all):
	var_vec = vars

	# varaiables already in the mode (starts with none):
	included_vars = character(0)

	# list to store the formula object corresponding to the best model with a given number of features:
	best_forms = list()

	# and a list of store the mse of the best model of each size:
	lowest_errors = list()

	# since the subsetted test/training sets will be the same when doing k-1 fold CV to add each feature, pre-compute them (saves lots of subsetting comp. time)	
	unique_cv = unique(dd[,cv_var])
	cv_dfs = lapply(unique_cv,function(u){
		test_indices1 = dd[,cv_var]==u
		test = dd[test_indices1,]
		train = dd[!test_indices1,]
		list(train,test)
	})
	
	nfolds = length(unique_cv)
	#cl = makeCluster(11,'FORK') # need to make after all functions defined 
	# for 
	for(n in 1:n_var){
	 	errors = numeric(length(var_vec))
	 	forms = list()
	 	for(v_num in 1:length(var_vec)){ # could also parallelize this! 
	 		v = var_vec[v_num]
	 		test_vars = c(included_vars, v)
	 		form = varCharToForm(response_var, test_vars)
			
	 		# could also parallellize this...i.e launch another cluster (within each other cluster) for this internal CV loop  (didn't work)
	 		mses = sapply(1:nfolds, function(i){ 
	 			
	 			train = cv_dfs[[i]][[1]]
	 			test = cv_dfs[[i]][[2]]
	 			s = svm(form, data = train)
	 			test = test[,formulaToVarList(form)]  #needed for svm.predict...gets messsed up if missing values in other columns (gets response and predcitors )
				# note: sum(complete.cases(test)) == length(preds) is true
	 			preds = predict(s,test)
	 			actual = test[,response_var][complete.cases(test)]
	 			mse = mean((preds-actual)**2)
	 			nfields = length(preds) # need this so that we can properly weight CV errors from each block...this is the actual number of fields for which we have both predictions and yield measured 
	 			c(mse,nfields)
	 		})
	 		weighted_err = sum(mses[1,]*mses[2,],na.rm=T)/sum(mses[2,])
	 		errors[v_num] = weighted_err
	 		forms[[v_num]] = form
	 	}
	 	best_var = var_vec[which.min(errors)]
	 	included_vars = c(included_vars, best_var)
	 	var_vec = var_vec[-which.min(errors)]
	 	f = forms[[which.min(errors)]]
	 	best_forms = c(best_forms, f)
	 	lowest_errors[n] = min(errors)
	}
	
	best_form = best_forms[[which.min(lowest_errors)]]
	smallest_error = lowest_errors[[which.min(lowest_errors)]]
	# havent' computed errors on the test set!!! should be able to add this; just need to add test set above
	## -- then we can also see how errors compare and break it up by fields to see if they need to be removed from CV procedure 
	s = svm(best_form, data = dd)
	dd_test = dd_test[,formulaToVarList(best_form)]  #needed for svm.predict...gets messsed up if missing values in other columns (gets response and predcitors )
	preds = predict(s,dd_test)
	actual = dd_test[,response_var][complete.cases(dd_test)]
	mse = mean((preds-actual)**2)
	return(list(best_form,smallest_error,mse))
}

## some boring fxsn needed for above:
# turns a character string of varialbe names into fomula object 
varCharToForm = function(response, predictors){
	preds = paste(predictors, collapse = '+')
	form_as_char=paste(response, preds, sep = '~')
	formula(form_as_char)
}
# turns formula into character vector of all vars in the formula: 
formulaToVarList = function(form){
	predictors = as.character(form)[1-2]
	predictors = unlist(strsplit(predictors, ' \\+ '))
	predictors = gsub('[\n ]','', predictors)
	predictors
}


# features to consider
good_weather_vars_june = c("jan_mean_temp","feb_mean_temp","mar_mean_temp","apr_mean_temp","may_mean_temp","jun_mean_temp","oct_mean_temp","nov_mean_temp","dec_mean_temp","oct_rain","nov_rain","dec_rain","jan_rain","feb_rain","mar_rain","apr_rain","may_rain","jun_rain")
june_spray_vars = c("may_16_31_lygus_sprays","may_16_31_aphid_sprays","may_16_31_thrip_sprays","may_16_31_mite_sprays","may_16_31_weed_sprays","june_1_15_lygus_sprays","june_1_15_aphid_sprays","june_1_15_thrip_sprays","june_1_15_mite_sprays","june_1_15_weed_sprays","june_16_30_lygus_sprays","june_16_30_aphid_sprays","june_16_30_thrip_sprays","june_16_30_mite_sprays","june_16_30_weed_sprays")
through_june_vars = c('may_29_june_4_total_insects','june_5_11_total_insects','june_12_18_total_insects','june_19_25_total_insects','june_26_july_2_total_insects',june_spray_vars,good_weather_vars_june,"other_june_1_15_retention_tp5","other_june_16_30_retention_tp5", 'june_1_15_retention_tp5','june_16_30_retention_tp5', 'plant_after', 'temik_planting_spray', 'prior_crop_year1','prior_crop_year2', 'cotton_type', 'yield_acreage','prior_p_fertilization_year1')


cl = makeCluster(12,'FORK') # need to make after all functions defined 
f = forwardSVMBlockCVParallel(d,through_june_vars,'actual_yield','crop_year',cl,30)
f
mean(sapply(f,function(i){i[[2]]}))
mean(sapply(f,function(i){i[[3]]}),na.rm=T)

############ RF

RFParallel = function(d,response_var,form,cv_var,cluster){
	# d: data frame containing data
	# predictor_vars: character vector of column names of features to be considered in feature selection process
	# response_var: what we want to predict (character of column name)
	# cv_var: every unique value of this column will be used to determine a test set for k-fold CV (and instances with that unique value will be used as the test set during the corresponding fold)
	unique_cv_vars = unique(d[,cv_var])
	cat(unique_cv_vars)
	cat('Cluster Applying! Go relax.')
	cv_results = clusterApply(cluster,unique_cv_vars,fitRFandGetError,d = d,cv_var=cv_var,response_var=response_var,form=form)
	cv_results

}

fitRFandGetError = function(cv_var_unique,d,cv_var,response_var,form){
	test_indices = d[,cv_var]==cv_var_unique
	test = d[test_indices,] 
	train = d[!test_indices,]
	r = randomForest(form,train,na.action = na.omit)
	preds = predict(r,test)
	actual = d[,response_var][as.numeric(names(preds))]
	list(preds,r$mse[500])
}



cl = makeCluster(12,'FORK') # need to make after all functions defined 
cat(cl)
form=varCharToForm('actual_yield',through_june_vars)
f = RFParallel(d,'actual_yield',form'crop_year',cl)
preds = unlist(r[1,])
actual = d$actual_yield[as.numeric(names(preds))]
# compare mse from OOB in rf to performance on new year test data:
w = sapply(1:ncol(r),function(i){length(r[1,i][[1]])})
sum(w*unlist(r[2,]))/sum(w) # .12
mean((preds-actual)**2,na.rm=T) # .41 
mean(abs(preds-actual)/actual,na.rm=T) # 83% accurate
plot(preds,actual,xlab='Predicted',ylab='Actual',main='RF Predicted vs. Actual')
