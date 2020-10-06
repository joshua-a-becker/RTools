## A FUNCTION FOR UNNESTING DATAFRAME INCEPTIONS

df_unnest = function(d) {
  if(nrow(d)==0){return(NULL)}
  to_unnest = which(sapply(colnames(d), FUN=function(z){class(d[,z])})=="data.frame" &
                      sapply(colnames(d), FUN=function(cname){length(d[,cname])>0} ))
  
  if(length(to_unnest)==0) {return(d)}
  
  return_d = d[,-to_unnest]
  for(cname in names(to_unnest)) {
    new_chunk = d[,cname]
    colnames(new_chunk)=paste0(cname, ".",colnames(new_chunk))
    return_d = cbind(return_d, new_chunk)
  }
  
  to_unnest = which(sapply(colnames(return_d), FUN=function(z){class(return_d[,z])})=="data.frame")
  
  
  
  if(length(to_unnest)>0) {return(df_unnest(return_d))}
  else{return(return_d)}
}