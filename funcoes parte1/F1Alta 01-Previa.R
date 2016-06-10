#Função que plota o gráfico em 2 dimensões
f1.teste <- function(){
    x<-seq(-100.0 , 100.0,length=200)
    f<-function(x) {
        (x^2 )}

    y<-f(x)
    plot(x, y, type = "l", xlab="x", ylab = "f(x)", panel.first = grid())
}

f1.teste()

#Função que plota o gráfico em 3 dimensões
f1.teste2 <- function(){
    x1<-seq(-100.0 , 100.0,length=50)
    x2<-x1
    f<-function(x1,x2) {
        (x1^2 + x2^2 )}

    z<-outer(x1,x2, f)

    persp(x1,x2,z,theta=30,phi=30, ltheta=90,shade=0.50,ticktype="detailed",d=5, zlab = "f{x1;x2}")
    #persp(x1,x2,z,theta=30,phi=30, ltheta=90,shade=0.50,ticktype="detailed",d=5, zlab = "", xlab = "", ylab = "")
}

f1.teste2()

