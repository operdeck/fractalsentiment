library(ggplot2)
library(data.table)

xstart <- 0
ystart <- 0
angle <- 0

segments <- list()

move <- function(length, pendown=TRUE)
{
  yend <<- ystart + sin(angle)*length
  xend <<- xstart + cos(angle)*length
  if (pendown) segments[[1+length(segments)]] <<- c(xstart, ystart, xend, yend, length)
  xstart <<- xend
  ystart <<- yend
}

turn <- function(a)
{
  angle <<- angle+a
}

draw <- function(depth, length)
{
  if (depth == 0) {
    move(length)
  } else {
    w <- 2.5
    l1 <- length/w
    l2 <- length-2*l1
    draw(depth-1, l1)
    turn(pi/2)
    draw(depth-1, l2)
    turn(-pi/2)
    draw(depth-1, l2)
    turn(-pi/2)
    draw(depth-1, l2)
    turn(pi/2)
    draw(depth-1, l1)
  }
}

size <- 10
depth <- 5
draw(depth, size)
turn(pi/2)
draw(depth, size)
turn(pi/2)
draw(depth, size)
turn(pi/2)
draw(depth, size)
turn(pi/2)

plotdata <- as.data.table(t(as.data.table(segments)))
setnames(plotdata, c("x0","y0","x1","y1","depth"))
plotdata[, i:=seq(.N)]
plt <- ggplot(plotdata, aes(x=x0,y=y0,xend=x1,yend=y1)) +
  geom_segment(aes(color=log(depth)))+
  scale_color_gradient(low="purple", high="yellow", guide=F)+
  coord_fixed(ratio=1)+
  #geom_point()+
  #geom_text(aes(label=i))+
  geom_label(data = data.table(x0=mean(plotdata$x0),y0=mean(plotdata$y0),x1=0,y1=0), 
             mapping=aes(label=paste("Depth",depth)))+
  theme_void()
print(plt)

