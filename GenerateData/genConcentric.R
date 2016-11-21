genConcentric<- function(center = c(0,0), r1=1, r2=2, r3=3, size1=150, size2 = 150, size3= 150, outnb = 5)
{
  size = size1 - outnb
  set1 <- as.data.frame(matrix(0, size1, 2))
  step = 2*r1/size
  for(i in 1:size)
  {
    x=center[1]-r1 + i*step
    ymin = -sqrt(r1^2 - (x-center[1])^2)
    ymax = -ymin
    y = runif(1, ymin, ymax) + center[2]
    set1[i, ] = c(x, y)
  }
  
  for (i in (size+1):size)
  {
    x = runif(1, r1, 1.5*r1)
    y = runif(1, r1, 1.5*r1)
    sign <- sample(c(-1, 1, -1, 1, -1, 1, -1, 1, -1, 1), 2)
    x = center[1] + x * sign[1]
    y = center[2] + y * sign[2]
    set1[i,] = c(x,y)
  }
  
  size = size2-outnb
  count = 1
  set2 <- as.data.frame(matrix(0, size2, 2))
  step = 2*r2/size
  s <- as.integer(size/2)
  for(i in 1:s)
  {
    x=center[1]-r2 + count*step # steps 1, 3, 5, ...
    count = count + 2
    if(abs(x-center[1])<r1)
    {
      ymin = -sqrt(r2^2 - (x-center[1])^2)
      ymax = -sqrt(r1^2 - (x-center[1])^2)
    }
    else
    {
        ymin = -1*sqrt(r2*r2 - (x-center[1])^2)
        ymax =  -ymin
    }
    
    y = runif(1, ymin, ymax) + center[2]
    set2[i, ] = c(x, y)
  }
  
  count = 0
  for(i in (s+1):size)
  {
    x=center[1]-r2 + count*step # steps 2, 4, 6, ...
    count = count + 2
    if(abs(x-center[1])<r1)
    {
      ymax = sqrt(r2^2 - (x-center[1])^2)
      ymin = sqrt(r1^2 - (x-center[1])^2)
    }
    else
    {
      ymin = -1*sqrt(r2*r2 - (x-center[1])^2)
      ymax =  -ymin
    } 
    y = runif(1, ymin, ymax) + center[2]
    set2[i, ] = c(x, y)
  }

  for (i in (size+1):size2)
  {
    x = runif(1, r2, 1.5*r2)
    y = runif(1, r2, 1.5*r2)
    sign <- sample(c(-1, 1, -1, 1, -1, 1, -1, 1, -1, 1), 2)
    x = center[1] + x * sign[1]
    y = center[2] + y * sign[2]
    set2[i,] = c(x,y)
  }
  
  size = size3 - outnb
  count = 0
  set3 <- as.data.frame(matrix(0, size3, 2))
  step = 2*r3/size
  s = as.integer(size/2)
  for(i in 1:s)
  {
    x=center[1]-r3 + count*step # steps 2, 4, 6, ...
    count = count + 2
    if(abs(x-center[1])<r2)
    {
      ymax = sqrt(r3^2 - (x-center[1])^2)
      ymin = sqrt(r2^2 - (x-center[1])^2)
    }
    else
    {
      ymin = -sqrt(r3^2 - (x-center[1])^2)
      ymax = sqrt(r3^2 - (x-center[1])^2)
    }
    
    y = runif(1, ymin, ymax) + center[2]
    set3[i, ] = c(x, y)
  }
  
  count = 1
  for(i in (s+1):size)
  {
    x=center[1]-r3 + count*step # steps 2, 4, 6, ...
    count = count + 2
    if(abs(x-center[1])<r2)
    {
      ymin = -sqrt(r3^2 - (x-center[1])^2)
      ymax = -sqrt(r2^2 - (x-center[1])^2)
    }
    else
    {
      ymin = -sqrt(r3^2 - (x-center[1])^2)
      ymax = sqrt(r3^2 - (x-center[1])^2)
    }
    
    y = runif(1, ymin, ymax) + center[2]
    set3[i, ] = c(x, y)
  }
  
  for (i in (size+1):size3)
  {
    x = runif(1, r3, 1.5*r3)
    y = runif(1, r3, 1.5*r3)
    sign <- sample(c(-1, 1, -1, 1, -1, 1, -1, 1, -1, 1), 2)
    x = center[1] + x * sign[1]
    y = center[2] + y * sign[2]
    set3[i,] = c(x,y)
  }
  
  concentrics = as.data.frame(rbind(set1, set2, set3))
  return(concentrics)
}