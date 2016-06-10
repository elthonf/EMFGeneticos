
#Função que plota o gráfico nos moldes pedidos pela professora, ou seja, maximizando em 0.
rastrigin.teste <- function(){
    x<-seq(-5.0,5.0,length=150)
    y<-x
    f<-function(x,y) {
        ((x^2 -10*cos(2*pi*x) +10) +
             (y^2 -10*cos(2*pi*y) +10) ) *-1}

    z<-outer(x,y, f)

    persp(x,y,z,theta=30,phi=30, ltheta=90,shade=0.50,ticktype="detailed",d=5)
}

#Função que plota o gráfico trabalhado, inverso do pedido pela professora, ou seja, minimizando em 0.
rastrigin.teste2 <- function(){
    x<-seq(-5.12,5.12,length=100)
    y<-x
    f<-function(x,y) { 20+(x^2-10*cos(2*3.14*x))+
            (y^2-10*cos(2*3.14*y)) }
    z<-outer(x,y,f)
    z[is.na(z)]<-1
    persp(x,y,z,theta=30,phi=30,expand=0.5,col="red",
          ltheta=90,shade=0.50,ticktype="detailed",d=5,r=1)
}

#Função extra, para testar multi-cores no gráfico. Não utilizada.
rastrigin.teste3 <- function(){
    par(bg = "white")
    x <- seq(-1.95, 1.95, length = 30)
    y <- seq(-1.95, 1.95, length = 35)
    z <- outer(x, y, function(a, b) a*b^2)
    nrz <- nrow(z)
    ncz <- ncol(z)
    # Create a function interpolating colors in the range of specified colors
    jet.colors <- colorRampPalette( c("blue", "green") )
    # Generate the desired number of colors from this palette
    nbcol <- 100
    color <- jet.colors(nbcol)
    # Compute the z-value at the facet centres
    zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
    # Recode facet z-values into color indices
    facetcol <- cut(zfacet, nbcol)
    persp(x, y, z, col = color[facetcol], phi = 30, theta = -30)


}

#Execução das funções
rastrigin.teste()
rastrigin.teste2()
#rastrigin.teste3()

