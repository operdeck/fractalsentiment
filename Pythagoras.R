library(ggplot2)
library(data.table)

xstart <- 0
ystart <- 0
angle <- 0

segments <- list()

move <- function(length, pendown=TRUE, id=0)
{
  yend <<- ystart + sin(angle)*length
  xend <<- xstart + cos(angle)*length
  if (pendown) segments[[1+length(segments)]] <<- c(xstart, ystart, xend, yend, length, id)
  xstart <<- xend
  ystart <<- yend
}

turn <- function(a)
{
  angle <<- angle+a
}

id <- 0

draw <- function(depth, length)
{
  if (depth == 0) return()
  id <<- id+1
  myid <- id
  
  turn(pi/2)
  move(length, id=myid)
  turn(-pi/4)
  
  draw(depth-1, length/sqrt(2))
  move(length/sqrt(2), F)
  turn(-pi/2)
  draw(depth-1, length/sqrt(2))
  move(length/sqrt(2), F)
  
  turn(-3*pi/4)
  move(length, F)
  turn(pi)
  move(length, id=myid)
  turn(-pi/2)
  move(length, id=myid)
  turn(-pi/2)
  move(length, id=myid)
  turn(pi)
}

 
size <- 10
depth <- 13
draw(depth, size)

plotdata <- as.data.table(t(as.data.table(segments)))
setnames(plotdata, c("x0","y0","x1","y1","depth","id"))
plotdata[, i:=seq(.N)]
plt <- ggplot(plotdata, aes(x=x0,y=y0,xend=x1,yend=y1)) +
  #geom_segment(aes(color=log(depth)))+
  geom_polygon(aes(x0,y0,group=id,fill=depth),alpha=0.8)+
  scale_fill_gradient2(mid="orange", low="purple", guide=F)+
  scale_color_gradient2(mid="orange", low="purple", guide=F)+
  #coord_fixed(ratio=1)+
  #geom_point()+
  #geom_text(aes(x=(x0+x1)/2,y=(y0+y1)/2,label=i))+
  # geom_label(data = data.table(x0=mean(plotdata$x0),y0=mean(plotdata$y0),x1=0,y1=0), 
  #            mapping=aes(label=paste("Depth",depth)))+
  theme_void()
print(plt)

