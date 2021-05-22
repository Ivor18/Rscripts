estimatePi <- function(n) {
  
  #generate random values between -1 and 1
  random_values_x <- runif(n, min = -1, max =1)
  random_values_y <- runif(n, min = -1, max =1)
  
  #create pair (x,y) of points
  random_pairs <- cbind(random_values_x, random_values_y)
  
  #initialize counters
  circle_count <- 0
  square_count <- n
  
  #count no. points that are within the circle
  for(i in 1 : n) {
    if((random_pairs[i,1]^2 + random_pairs[i,2]^2) <= 1)
      circle_count <- circle_count + 1
  }
  
  #return estimate
  return(4*circle_count/square_count)
}

print(estimatePi(10000))