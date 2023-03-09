# mid square method

midsq = function(n){
  c = c()
  s = 7182
  for(i in 1:n){
    k = s^2
    s = ((k%/%100)%%10000)
    c[i] = s / 10000
  }
  return(c)
}

#generating 10 values
x = midsq(10)
x
#plot 
hist(x, nclass=10)


#z method
zmethod = function(z, a, c, m, n){
  result = c()
  if(m>0 & m>a & m>c & z<m){
    for(i in 1:n){
      z = (a*z + c)%%m
      result[i] = z
    }
    return(result)
  }else{
    print("Value Error: z, a, c, and 0 are not less than m")
  }
}

#generating 100 values
z = zmethod(z=27, a=17, c=43, m=100, n=100)
z
#plot
hist(z, nclass = 10)


