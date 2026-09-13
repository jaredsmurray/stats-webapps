library(shiny)
for(file in c('apps/sap_sampling/app.R','apps/sap_sampling/app_shinylive.R','apps/thaler/app.R')) {
 e <- new.env();source(file,local=e)
 thaler <- grepl('thaler',file)
 testServer(e$server, {
  if(thaler) session$setInputs(n_proj=23,k=50,show_normal=TRUE,show_prop=FALSE,show_loss=TRUE,as_percent=FALSE)
  else session$setInputs(n=81,n_typed=81,k=50,bw_means=.5,show_normal=TRUE,show_ci=TRUE)
  count <- function() if(thaler) length(values$history) else length(values$mean_history)
  trigger <- 0
  batch <- function(k) {
   trigger <<- trigger+1
   session$setInputs(k=k)
   if(thaler) session$setInputs(sim_many=trigger) else session$setInputs(take_many=trigger)
  }
  for(k in list(NA_real_,NULL,NaN,Inf,-Inf)) {
   before <- count();batch(k);stopifnot(count()==before)
  }
  batch(2.4);stopifnot(count()==2)
  batch(0);stopifnot(count()==3)
  batch(10001);stopifnot(count()==10003)
  if(thaler) session$setInputs(sim_one=1) else session$setInputs(take_one=1)
  stopifnot(count()==10004)
  if(thaler) session$setInputs(n_proj=3) else session$setInputs(n=100)
  stopifnot(count()==0)
  batch(1);stopifnot(count()==1)
  session$setInputs(clear=1);stopifnot(count()==0)
 })
 cat(file,': invalid input, rounding, bounds, recovery, size change, clear PASS\n')
}
