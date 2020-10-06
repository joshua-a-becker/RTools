#rm(list=ls());gc()
require(tidyverse)
require(R.utils)

#~# ./ THE AGREEMENT GAME \.
###
### - You are a UN rep trying to design a multilateral agreement 
###       that must satisfy N people
###       on K different issues
###       (to save the world from certain doom!)
### - An issue can take an integer value between 0 and (limit)
###      so when limit=1, it is a binary choice 
### - A "solution" S is a (row) vector, where each entry indicates a value for an issue
### - A country's payoff function is a (column) vector the length of a solution
###      such that their payoff = S x P
### - A country will only sign if their net payoff is positive.
### - Your goal is to maximize the number of signatories, thus increasing 
###      the probability that the agreement will succeed
###      (and that the world will be saved!)

### ./ THIS SCRIPT \.
###
### This script is a proof of concept-showing that the setup 
### above can produce a 'complex' fitness landscape.  Here I 
### operationalize 'complex' as creating the possibility of 
### getting stuck on a local maximum in myopic search, due to
### interdependence between issues.

### Basically this shows that the model above creates a kind of NK space

### There's a lot left undiscussed here about how this model actually
### maps onto a negotiation situation.  in this scenario, we have a god-like
### agent trying to design a solution, but in the negotiation simulation 
### that i experienced first-hand, we were all each trying to satisfy
### our own conflicting payoff functions.



### READ THE GAME FUNCTION TO SEE HOW IT WORKS
### OR SKIP DOWN BELOW TO USE IT
go = function(n=20, k=5
              , max.payoff=1, min.payoff=-5
              , BATNA=0
              , t=1000, reps=10, P=NULL
              ) {
  
  ### SOME UTILITIES
  create.payoff = function(k){replicate(k, runif(1,min.payoff,max.payoff))}
  #count.payoff  = function(s, p) { sum( (s%*%p)>BATNA ) }
  threshold.payoff  = function(s, p) { sum( (s%*%p)>BATNA ) }
  welfare.payoff  = function(s, p) { sum( (s%*%p) ) }
  results = data.frame(solution=character() ,payoff=numeric())
  
  P### set payoff matrix if not defined
  if(is.null(P)) P=replicate(n, create.payoff(k))
  
  ### for convenience
  pb <- txtProgressBar(min = 0, max = t*reps, style = 3);cnt=0
  
  ### each "rep" is an independent run of the model
  for(i in 1:reps) {
    
    ### s defines the starting point, then search t times
    s=round(runif(k,0,limit))
    for(j in 1:t){
      setTxtProgressBar(pb, cnt);cnt=cnt+1
      
      ### payoff of the current solution
      current.count.payoff = threshold.payoff(s,P)
      
      ### so how do we explore?  when i was participating in this 
      ### negotiation (simulated) we started with an initial proposal,
      ### then would "tweak" one issue at a time and see if an increase/decrease
      ### on that issue could help bring another person onboard.
      
      ### pick a random issue to 'tweak' 
      adjust = sample(1:k, 1)
      
      ### copy the solution, and randomly increase or decrease target issue by 1
      s.new = s; s.new[adjust]=s.new[adjust]+sample(c(1,-1),1)
      
      ### ensure it's within limits
      if(s.new[adjust]>limit){s.new[adjust]=limit}
      if(s.new[adjust]<0){s.new[adjust]=0}
      
      ### if the new solution is better, keep it and update the score
      if(current.count.payoff<=threshold.payoff(s.new,P)) {
        s = s.new
        current.count.payoff = threshold.payoff(s,P)
        current.welfare.payoff = welfare.payoff(s, P)
      }
    }
    
    ### for convenience
    best_solution = paste0(s, collapse=",") 
    
    ### if this is a new 'best' solution, save it
    if(sum(grepl(best_solution,results$solution))==0){
      results = rbind(results, data.frame(
        solution=best_solution
        , count.payoff=current.count.payoff
        , welfare.payoff=current.welfare.payoff
        , stringsAsFactors=F
      ))
    }
  }
  list(results=results, P=P,payfn=threshold.payoff)
}
  


### TO EXPLORE THE SPACE:
###   1. set the n/k/max/min you want to try
###   2. check out the results.
###   3. if you see something promising,
###          set t/reps higher to map it more precisely


k=6
n=100
max.payoff=5
min.payoff=-10
limit=1  
BATNA=0
### QUICK RUN JUST TO CHECK IT OUT
results=go(n=n, k=k, max.payoff=max.payoff, min.payoff=min.payoff
           , t=1000
           , reps=20
           , BATNA=BATNA
           )
results$results%>%
  arrange(count.payoff)

### IF YOU FIND SOMETHING YOU LIKE,
### GET A MORE PRECISE MAP
results=go(n=n, k=k, max.payoff=max.payoff, min.payoff=min.payoff
   , t=5000, reps=50
   , P=results$P
   , BATNA=BATNA
   )
results$results%>%
  arrange(count.payoff)

