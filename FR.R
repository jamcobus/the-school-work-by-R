'''使用共轭梯度法求无约束问题f(x)=2*(x[1]^2)-2*x[1]*x[2]+x[2]^2+2*x[1]-2*x[2]
和f’(x)，置信度0.0001，迭代限制10'''
f<-function(x){
  return(2*(x[1]^2)-2*x[1]*x[2]+x[2]^2+2*x[1]-2*x[2])
}
grad_f<- function(x){
  return(c(4*x[1]-2*x[2]+2,2*x[2]-2*x[1]-2))
}

conjugate_gradient<- function(f,grad_f,x0,max_iter=10,tol=0.0001){
  x<-x0
  d<- -grad_f(x)
  g_norm<-sqrt(sum(grad_f(x)^2))
  f_values<-c(f(x))
  g_norms<-c(g_norm)
  
  for(i in 1:max_iter){
    a<-optimize(function(a){f(x+a*d)},interval=c(-100,100))$minimum
    x<-x+a*d
    g<-grad_f(x)
    g_norm<-sqrt(sum(g^2))
    print(x)
    print(a)
    print(d)
    if(g_norm<tol){
      break
    }
    beta<-sum(g^2)/sum(grad_f(x-a*d)^2)
    d<- -g+beta*d
    
    f_values<-c(f_values,f(x))
    g_norms<-c(g_norms,g_norm)
  }
  return(list(x=x,f_values=f_values,g_norms=g_norms))
  
}
result<-conjugate_gradient(f,grad_f,c(0,0))
cat("x:",result$x,"\n")
#cat("f_values:",result$f_values,"\n")
#cat("g_norms:",result$g_norms,"\n")
#print(result)
