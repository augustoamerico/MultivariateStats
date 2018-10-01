#https://plot.ly/r/trisurf/
#https://plot.ly/r/reference/#mesh3d


### Animations: https://plot.ly/r/animations/
### Camera Settings: https://plot.ly/r/reference/#layout-scene-camera

### Understanding Matrixes's Spans and determinants

library(plotly)

drawMatrixWithDet <- function(iMatrix, size){
  if(size>3){
    #raise error
  } 
  if(size==2){
    matrixRep <- iMatrix
    a<-iMatrix[1,]
    b<-iMatrix[2,]
    aPb<-a+b
    zero<-c(0,0)
    
    surf<-as.data.frame(
      matrix(c(
        zero,b,aPb,a
      ),4,byrow=T))
    names(surf)<-c("x","y")
    
    newRows <- c()
    for(i in 1:dim(matrixRep)[2]){
      newRows<- c(newRows,0,0,matrixRep[i,])
    }
    
    newRows <- c(newRows,0,0)
    dfPlot <- as.data.frame(matrix(newRows,dim(matrixRep)[2]*2+1,byrow = T))
    colors <- c("red","red","green","green","blue")
    dfPlot$colors <- colors
    names(dfPlot) <- c("x","y","colors")
    
    plotly::plot_ly(dfPlot, x = ~x, y = ~y, type = 'scatter', mode = 'lines',
                    opacity = 1, line = list(width = 3, color = ~colors,reverscale = FALSE)) %>%
      plotly::add_polygons(
        x = surf$x,
        y = surf$y,
        opacity=0.4
        #facecolor = rep(toRGB(viridisLite::inferno(6)), each = 2)
        #facecolor = rep("orange",12)
      )
    
    
  }else{
    matrixRep <- iMatrix
    a<-iMatrix[1,]
    b<-iMatrix[2,]
    c<-iMatrix[3,]
    aPb<-a+b
    aPc<-a+c
    bPc<-b+c
    aPbPc<-a+b+c
    zero<-c(0,0,0)
    surf<-as.data.frame(
      matrix(c(
        zero,b,aPb,a,c,bPc,aPbPc,aPc
      ),8,byrow=T))
    
    names(surf)<-c("x","y","z")
    newRows <- c()
    for(i in 1:dim(matrixRep)[2]){
      newRows<- c(newRows,0,0,0,matrixRep[i,])
    }
    newRows <- c(newRows,0,0,0)
    dfPlot <- as.data.frame(matrix(newRows,dim(matrixRep)[2]*2+1,byrow = T))
    colors <- c("red","red","red","green","green","blue","blue")
    dfPlot$colors <- colors
    names(dfPlot) <- c("x","y","z","colors")
    
    #print(dfPlot)
    #print(surf)
    
    plotly::plot_ly(dfPlot, x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'lines',
                    opacity = 1, line = list(width = 6, color = ~colors,  opacity=c(0.3,0.3,0.3,0.3,0.3,0.3),reverscale = FALSE)) %>%
      plotly::add_mesh(
        x = surf$x,
        y = surf$y,
        z = surf$z,
        i = c(0, 0, 0, 0, 2, 2, 1, 1, 1, 1, 5, 5),
        j = c(1, 3, 4, 3, 6, 3, 2, 5, 5, 0, 6, 4),
        k = c(2, 2, 7, 7, 7, 7, 6, 6, 4, 4, 7, 7),
        opacity=0.4,
        #facecolor = rep(toRGB(viridisLite::inferno(6)), each = 2)
        facecolor = rep("orange",12)
      )
  }
}


matrixA2 <- matrix(c(1,0,0,1),2) 
matrixA2
drawMatrixWithDet(matrixA2,dim(matrixA2)[1])

matrixB2 <- matrix(c(1,3,0,1),2) 
matrixB2
drawMatrixWithDet(matrixB2,dim(matrixB2)[1])

matrixA <- matrix(c(1,0,0,0,1,0,0,0,1),3) 
matrixA
drawMatrixWithDet(matrixA,dim(matrixA)[1])

matrixB <- matrix(c(1,2,3,2,2,1,3,2,4),3)
matrixB
drawMatrixWithDet(matrixB,dim(matrixB)[1])

# Vector Norm

# Vector Span (a.k.a determinant)

drawMatrixWithDet(matrix(c(1,2,3,2,2,1,3,2,4),3),3)


#####

df <- data.frame(
  x = c(1,2,1), 
  y = c(1,5,2), 
  z = c(1,2,3),
  f = c(1,2,3)
)

p <- df %>%
  plot_ly(
    x = ~x,
    y = ~y,
    z = ~z,
    frame = ~f,
    type = 'scatter3d',
    mode = 'markers',
    showlegend = F
  )

p

gapminder::gapminder %>%
  plot_ly(
    x = ~gdpPercap, 
    y = ~lifeExp, 
    size = ~pop, 
    color = ~continent, 
    frame = ~year, 
    text = ~country, 
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )
  )