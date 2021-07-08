



















1 1
2 2
3 3
4

start = 1
end = 2
start1 = 1
end1 = 3
res = matrix(0,1,2)
i = 1
intervals_creator <- function(start,end,start1,end1, res){
  if(start1 <= end1){
    do.call('rbind', lapply(start:end, function(i){
      #-- dodac przesuwajacy sie start - dol przedzialu
      #res <- list(res,cbind(rep(start1,length(start:i)), start:i))
      res <- list(res,cbind(rep(start1,length(i)), i))
      
      intervals_creator(i, end, start1+1, end1, do.call('rbind',res))
    }))
  }else{
    res
  }
}
  

intervals_creator(1,2,1,3, matrix(0,1,2))

