#1. Write R code implementing the forward variable selection
# procedure in that paper, Section 5.1 "Forward Variable Selection"
# page 132.
#Test your code and explain why you think your code is correct.

### TO DO, FORWARD
# -diff measures besides RSS
# -standard error measurement
# -column index instead of string iv
# -cv / test set
# -full verbosity
# -global variable for name of performance measurement (eg rss)
# -allow user to pass two data frames within single call (train+test)

# Global variable declaration a la https://stackoverflow.com/questions/10904124/global-and-local-variables-in-r
# User defines proportion of data to be allocated to validation set
# Eg, default .4 makes the validation set smaller than the training set
foo <- function() {
  test.proportion <<- .4
}
foo()

foo5 <- function() {
  
}
foo5()

# Although you can change this to something besides RSS, if you change it to something like
# R^2, you will need to make changes elsewhere (ie (if < ...) ---> (if > ...) )
lm.performance<-function(mod){
  return(sum(resid(mod)^2))
}

# transforms variables into lm string, ie "w~x+y+..."
my.paste<-function(iv,already.included,new.can){
  str.one<-paste0(iv,"~",new.can)
  if (!is.null(already.included)) {
    for (i in 1:length(already.included)){
      str.one<-paste0(str.one,"+",already.included[i])
    }
  }
  return(str.one)
}

test.paste<-function(iv,vars){
  s<-paste(iv,"~",sep="")
  num.vars<-length(vars)
  for (i in 1:num.vars){
    if (vars[i] != iv){
      if (i != num.vars){
        s<-paste(s,vars[i],"+",sep="")
      }else{
        s<-paste(s,vars[i],sep="")
      }
    }
  }
  return(s)
}

# recreates x~y var list to re-call model on new data
my.paste.two<-function(mod,iv){
  vars<-setdiff(names(mod$model),iv)
  #print(vars)
  #print(length(vars))
  str.one<-paste0(iv,"~",vars[1])
  #print(str.one)
  if (length(vars)>1){
    for (i in 2:length(vars)){
      str.one<-paste0(str.one,"+",vars[i])
    }
  }
  return(str.one)
}

# Workhorse- adds the next best variable
get.best<-function(data,candidates,iv,already.included){
  init.mod<-do.call("lm",list(my.paste(iv,already.included,candidates[1]),data))
  min.rss<-lm.performance(init.mod)
  best.mod<-init.mod
  addl.var<-candidates[1]
  for (i in 2:length(candidates)){
    lin.mod<-do.call("lm",list(my.paste(iv,already.included,candidates[i]),data))
    resids<-lm.performance(lin.mod)
    if (resids<min.rss) {
      min.rss<-resids
      best.mod<-lin.mod
      addl.var<-candidates[i]
    }
  }
  return(best.mod)
}

print_output_old<-function(performance,vars,std.err){
  #print(errors)
  print(performance)
  print(vars)
  print(std.err)
}

print_output<-function(performance,vars,std.err,min,minse,train){
  if (length(performance)!=length(vars) || 
      length(vars)!=length(std.err)){
    print("OH FU-")
    # raise error here
  }
  for (i in 1:length(performance)){
    if (i==1){
      print("1 VAR")
    }else{
      print(paste(i,"-var model",sep=""))
    }
    print(performance[i])
    print(vars[[i]])
    if (performance[i]==min){
      if (missing(train)){
        print("This model has the lowest RSS")
      } else {
        if (train) {
          print("This model has the lowest training RSS")
        } else {
          print("This model has the lowest test RSS")
        }
      }
    }
    if (performance[i]==minse){
      if (missing(train)){
        print("This model is the simplest within 1 SE of the minimum")
      } else {
        if (train){
          print("This model is the simplest within 1 SE of the minimum training RSS")
        }else{
          print("This model is the simplest within 1 SE of the minimum test RSS")
        }
      }
    }
    if (i != length(performance)){
      print("----------")
    }
  }
}

# estimates standard error of RSS
boot.est<-function(data,lin.mod,iv){
  b<-500
  error.vec<-rep(NA,b)
  for (i in 1:b){
    boot.inds<-sample(1:nrow(data),nrow(data),replace=TRUE)
    boot.real<-data[boot.inds,]
    new.mod<-do.call("lm",list(my.paste.two(lin.mod,iv),boot.real))
    error.vec[i]<-lm.performance(new.mod)
  }
  return(sd(error.vec))
}

find.minse<-function(rss,lb,ub,min){
  min.ind<-get.index(rss,min)
  for (i in 1:length(rss)){
    if (rss[i] < ub[min.ind] && 
        rss[i] > lb[min.ind]){
      return(rss[i])
    }
  }
  warning('find.minse broke')
}

get.index<-function(arr,val){
  for (i in 1:length(arr)){
    if (arr[i]==val){
      return(i)
    }
  }
  # raise error here
  warning('get.index failed')
  return(NA)
}


# greedy search through possible linear models
# does not consider variable transformations or interactions
# user passes a string denoting which variable is the one to be predicted
# a more robust implementation would allow a column index as well
run.forward<-function(data,iv,plotting,verbose){
  init.poss<-setdiff(names(data),iv)
  rss<-rep(NA,length(init.poss))
  se<-rep(NA,length(init.poss))
  list.vars<-list()
  lm.best<-get.best(data,init.poss,iv,NULL)
  rss[1]<-lm.performance(lm.best)
  se[1]<-boot.est(data,lm.best,iv)
  best.measure<-rss[1]
  used<-setdiff(names(lm.best$model),iv)
  list1<-list(used)
  list.vars[1]<-list1
  poss<-setdiff(init.poss,used)
  for (i in 2:length(poss)){
    lm.next<-get.best(data,poss,iv,used)
    rss[i]<-lm.performance(lm.next)
    se[i]<-boot.est(data,lm.next,iv)
    if (rss[i]<best.measure){
      best.measure<-rss[i]
      lm.best<-lm.next
    }
    used<-setdiff(names(lm.next$model),iv)
    list1<-list(used)
    list.vars[i]<-list1
    poss<-setdiff(init.poss,used)
  }
  # get.best can't get the final ~. model, so I have to do it outside the loop
  full.mod<-do.call("lm",list(my.paste(iv,init.poss[-1],init.poss[1]),data))
  rss[length(init.poss)]<-lm.performance(full.mod)
  se[length(init.poss)]<-boot.est(data,full.mod,iv)
  list1<-list(setdiff(names(full.mod$model),iv))
  list.vars[length(init.poss)]<-list1
  if (rss[length(init.poss)]<best.measure){
    best.measure<-rss[length(init.poss)]
    lm.best<-full.mod
  }
  lb<-rep(NA,length(se))
  ub<-rep(NA,length(se))
  for (i in 1:length(lb)){
    lb[i]<-rss[i]-se[i]
    ub[i]<-rss[i]+se[i]
  }
  minimum<-min(rss)
  minse<-find.minse(rss,lb,ub,minimum)
  #print(names(fin.mod$model))
  if (verbose){
    print(minimum)
    #print(get.index(rss,minimum))
    print(paste(paste("Min RSS=",round(minimum,4),sep=""),paste("by ",get.index(rss,minimum),"-var model",sep="")))
    print(paste(paste("Simplest 1-SE RSS=",round(minse,4),sep=""),paste("by ",get.index(rss,minse),"-var model",sep="")))
    print("----------")
    print_output(rss,list.vars,se,minimum,minse)
  }
  if (plotting){
    require(ggplot2)
    num.vars<-seq(1,length(names(data))-1)
    x3<-as.data.frame(cbind(num.vars,rss))
    g<-ggplot(x3,aes(num.vars,rss))+
      geom_point()+ggtitle("RSS vs Predictors in Forward Stepwise Regression")+
      geom_errorbar(aes(ymin=rss-se, ymax=rss+se), width=.1)
    print(g)
  }
  return(lm.best)
}

run.forward.test<-function(data.orig,iv,plotting,verbose){
  tset.inds<-sample(seq(1,nrow(data.orig)),nrow(data.orig)*test.proportion,replace=FALSE)
  data=data.orig[tset.inds,]
  testset=data.orig[-tset.inds,]
  init.poss<-setdiff(names(data),iv)
  rss<-rep(NA,length(init.poss))
  se<-rep(NA,length(init.poss))
  rss.test<-rep(NA,length(init.poss))
  se.test<-rep(NA,length(init.poss))
  list.vars<-list()
  lm.best<-get.best(data,init.poss,iv,NULL)
  best.test<-do.call("lm",list(test.paste(iv,names(lm.best$model)),testset))
  rss.test[1]<-lm.performance(best.test)
  se.test[1]<-boot.est(testset,best.test,iv)
  rss[1]<-lm.performance(lm.best)
  se[1]<-boot.est(data,lm.best,iv)
  best.measure<-rss[1]
  best.test.measure<-rss.test[1]
  used<-setdiff(names(lm.best$model),iv)
  list1<-list(used)
  list.vars[1]<-list1
  poss<-setdiff(init.poss,used)
  for (i in 2:length(poss)){
    lm.next<-get.best(data,poss,iv,used)
    rss[i]<-lm.performance(lm.next)
    se[i]<-boot.est(data,lm.next,iv)
    if (!missing(testset)){
      next.test<-do.call("lm",list(test.paste(iv,names(lm.next$model)),testset))
      rss.test[i]<-lm.performance(next.test)
      se.test[i]<-boot.est(testset,next.test,iv)
      if (rss.test[i]<best.test.measure){
        best.test.measure<-rss.test[i]
        lm.best.test<-next.test
      }
    }
    if (rss[i]<best.measure){
      best.measure<-rss[i]
      lm.best<-lm.next
    }
    
    used<-setdiff(names(lm.next$model),iv)
    list1<-list(used)
    list.vars[i]<-list1
    poss<-setdiff(init.poss,used)
  }
  # get.best can't get the final ~. model, so I have to do it outside the loop
  full.mod<-do.call("lm",list(my.paste(iv,init.poss[-1],init.poss[1]),data))
  rss[length(init.poss)]<-lm.performance(full.mod)
  se[length(init.poss)]<-boot.est(data,full.mod,iv)
  list1<-list(setdiff(names(full.mod$model),iv))
  list.vars[length(init.poss)]<-list1
  if (rss[length(init.poss)]<best.measure){
    best.measure<-rss[length(init.poss)]
    lm.best<-full.mod
  }
  full.mod.test<-do.call("lm",list(test.paste(iv,names(full.mod$model)),testset))
  rss.test[length(init.poss)]<-lm.performance(full.mod.test)
  se.test[length(init.poss)]<-boot.est(testset,full.mod.test,iv)
  if (rss.test[length(init.poss)]<best.test.measure){
    best.test.measure<-rss.test[length(init.poss)]
    lm.best.test<-full.mod.test
  }
  lb<-rep(NA,length(se))
  ub<-rep(NA,length(se))
  lb.test<-rep(NA,length(se))
  ub.test<-rep(NA,length(se))
  for (i in 1:length(lb)){
    lb[i]<-rss[i]-se[i]
    ub[i]<-rss[i]+se[i]
  }
  for (i in 1:length(lb)){
    lb.test[i]<-rss.test[i]-se.test[i]
    ub.test[i]<-rss.test[i]+se.test[i]
  } 
  
  minimum<-min(rss)
  minse<-find.minse(rss,lb,ub,minimum)
  #print(rss)
  print('---')
  print(se)
  print('---')
  #print(ub)
  #print(paste('minimum=',minimum))
  #print(paste('minse=',minse))
  minimum.test<-min(rss.test)
  minse.test<-find.minse(rss.test,lb.test,ub.test,minimum.test)
  #print(names(fin.mod$model))
  
  if (verbose){
    print(minimum)
    #print(get.index(rss,minimum))
    print(paste(paste("Min Train RSS=",round(minimum,4),sep=""),paste("by ",get.index(rss,minimum),"-var model",sep="")))
    print(paste(paste("Simplest 1-SE Train RSS=",round(minse,4),sep=""),paste("by ",get.index(rss,minse),"-var model",sep="")))
    print("----------")
    print_output(rss,list.vars,se,minimum,minse)
    
    print(minimum.test)
    #print(get.index(rss,minimum))
    print(paste(paste("Min Test RSS=",round(minimum.test,4),sep=""),paste("by ",get.index(rss.test,minimum.test),"-var model",sep="")))
    print(paste(paste("Simplest 1-SE Test RSS=",round(minse.test,4),sep=""),paste("by ",get.index(rss.test,minse.test),"-var model",sep="")))
    print("----------")
    print_output(rss.test,list.vars,se.test,minimum.test,minse.test)
    
  }
  if (plotting){
    require(ggplot2)
    
    num.vars<-seq(1,length(names(data))-1)
    x3<-as.data.frame(cbind(num.vars,rss))
    g<-ggplot(x3,aes(num.vars,rss))+
      geom_point()+ggtitle("Train RSS vs Predictors in Forward Stepwise Regression")+
      geom_errorbar(aes(ymin=rss-se, ymax=rss+se), width=.1)
    print(g)
    
    num.vars<-seq(1,length(names(data))-1)
    x4<-as.data.frame(cbind(num.vars,rss.test))
    g2<-ggplot(x4,aes(num.vars,rss.test))+
      geom_point()+ggtitle("Test RSS vs Predictors in Forward Stepwise Regression")+
      geom_errorbar(aes(ymin=rss.test-se.test, ymax=rss.test+se.test), width=.1)
    print(g2)
  }
  return(lm.best)
}


run.forward.cv<-function(data,iv, plotting,verbose){
  print('in cv')
  print(plotting)
  print(verbose)
}

# wrapper function to catch errors and set default arguments
lm.forward<-function(data,iv,test,...){
  test.default<-'none'
  verbose.default<-TRUE
  plotting.default<-TRUE
  valid.tests <<- c('test','cv','none')
  
  if (missing(iv)) stop("Please use at least two arguments lm.forward(data.frame,'column.name')")
  if (!(iv %in% colnames(data))) stop(paste(iv,' is not a valid column'))
  if (!(is.data.frame(data))) stop("First argument to lm.forward must be a data frame")
  if (missing(test)){
    test<-test.default
  }
  else if ((!missing(test))&&(!(test %in% valid.tests))){
    warning('you provided invalid 3rd arg to lm.forward. use one of \'test\',\'cv\',or \'none\'')
    test<-test.default
  }
  optional.args<-list(...)
  num.opt<-length(optional.args)
  if (num.opt==0) {
    plotting<-plotting.default
    verbose<-verbose.default
  }
  else if (num.opt==1){
    if (optional.args[1]!=TRUE && optional.args[1]!=FALSE){
      warning('4th arg to lm.forward must be TRUE or FALSE. resorting to default')
      plotting<-plotting.default
    } else {
      plotting<-optional.args[[1]]
    }
    verbose<-verbose.default
  } else if (num.opt>1){
    if (num.opt>2){
      warning(paste('ignoring extraneous arguments starting with',optional.args[[3]]))
    }
    if (optional.args[[1]]!=TRUE && optional.args[[1]]!=FALSE){
      warning('4th arg to lm.forward must be TRUE or FALSE. resorting to default')
      plotting<-plotting.default
    } else {
      plotting<-optional.args[[1]]
    }
    if (optional.args[[2]]!=TRUE && optional.args[[2]]!=FALSE){
      warning('5th arg to lm.forward must be TRUE or FALSE. resorting to default')
      verbose<-verbose.default
    } else {
      verbose<-optional.args[[2]]
    }
    
  }
  if (test=='test'){
    return(run.forward.test(data,iv,plotting,verbose))
  } else if (test=='cv'){
    return(run.forward.cv(data,iv,plotting,verbose))
  } else if (test=='none'){
    return(run.forward(data,iv,plotting,verbose))
  }
}

#x<-lm.forward(mtcars,"mpg")
#x<-lm.forward(mtcars,"mpg",'test')
#x<-lm(mpg~., data=mtcars)
#summary(x)
#summary(x)$coefficients[, 2]
