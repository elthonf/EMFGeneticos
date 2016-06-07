#Função que plota o gráfico em 2 dimensões
f3.teste <- function(){
    x<-seq(-500.0 , 500.0,length=1000)
    f<-function(x) {
        ( -x * sin(sqrt(abs(x))) )}

    y<-f(x)
    plot(x, y, type = "l", ylab = "f(x)", panel.first = grid())
}

f3.teste()


#Função que plota o gráfico em 3 dimensões
f3.teste2 <- function(){
    x1<-seq(-500.0 , 500.0,length=200)
    x2<-x1
    f<-function(x1,x2) {
        ( ( -x1 * sin(sqrt(abs(x1))) ) + ( -x2 * sin(sqrt(abs(x2))) ) )}

    z<-outer(x1,x2, f)

    persp(x1,x2,z,theta=30,phi=30, ltheta=90,shade=0.50,ticktype="detailed",d=5, zlab = "f{x1,x2}")
}

f3.teste2()


#Função que plota o gráfico em 2 dimensões
f3.teste3 <- function(){
    x<-seq(415.0 , 425.0,length=1000)
    f<-function(x) {
        ( -x * sin(sqrt(abs(x))) )}

    y<-f(x)
    plot(x, y, type = "l", ylab = "f(x)", panel.first = grid())
}

f3.teste3()



#Função que identifica o mínimo global
f3.teste4 <- function(){
    x<-seq(420.9685 , 420.9690,length=100000)
    f<-function(x) {
        ( -x * sin(sqrt(abs(x))) )}

    y<-f(x)
    mY = min(y);
    index = which(y == mY);
    mX = x[index];

    ylab = paste("f(x) - min =", mY);
    xlab = paste("x - min =", mX);
    plot(x, y, type = "l", ylab = ylab, xlab = xlab, panel.first = grid());
    return (min(y));
}

a = f3.teste4()

print(a*30, digits = 20)
#print(f3.exec09$best[1000], digits = 20) #Para comparar







