#Função que plota o gráfico em 2 dimensões
f2.teste <- function(){
    x<-seq(-2.0 , 2.0,length=200)
    f<-function(x) {
        ( (x + 0.5) ^2 )}

    y<-f(x)
    plot(x, y, type = "l", xlab="x", ylab = "f(x)", panel.first = grid())
}

f2.teste()


#Função que plota o gráfico em 3 dimensões
f2.teste2 <- function(){
    x1<-seq(-2.0 , 2.0,length=50)
    x2<-x1
    f<-function(x1,x2) {
        ( (x1+0.5)^2 + (x2+0.5)^2 )}

    z<-outer(x1,x2, f)

    persp(x1,x2,z,theta=30,phi=30, ltheta=90,shade=0.50,ticktype="detailed",d=5, zlab = "f{x1;x2}")
    #persp(x1,x2,z,theta=30,phi=30, ltheta=90,shade=0.50,ticktype="detailed",d=5, zlab = "", xlab = "", ylab = "")
}

f2.teste2()

