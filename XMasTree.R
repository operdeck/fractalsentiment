library(ggplot2)
library(data.table)
library(colorspace)

xstart <- 0
ystart <- 0
angle <- pi/2

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

draw <- function(depth, length, id=0)
{
  move(length, id=depth)
  if (depth > 0) {
    lbranch <- 0.5*length
    ltrunc <- 0.7*length
    turnangle <- 2*pi/3
    
    turn(turnangle)
    draw(depth-1, lbranch)
    move(-lbranch, F)
    
    turn(-turnangle)
    draw(depth-1, ltrunc)
    move(-ltrunc, F)
    
    turn(-turnangle)
    draw(depth-1, lbranch)
    move(-lbranch, F)
    
    turn(turnangle)
    
  }
}

size <- 10
depth <- 9
draw(depth, size)

plotdata <- as.data.table(t(as.data.table(segments)))
setnames(plotdata, c("x0","y0","x1","y1","depth","id"))
plotdata[, i:=seq(.N)]
plt <- ggplot(plotdata, aes(x=x0,y=y0,xend=x1,yend=y1)) +
  geom_segment(aes(color=depth,size=I(depth/4)))+
  scale_color_gradient(low="lightgreen",high=scales::muted("green"),guide=F)+
  coord_fixed(ratio=1)+
  geom_point(data = plotdata[id<=1], color="red",size=0.2)+
  geom_point(data = data.frame(x0=0,y0=max(plotdata$y0),x1=0,y1=0),color="yellow",size=3)+
  #geom_text(aes(x=(x0+x1)/2,y=(y0+y1)/2,label=i))+
  #geom_label(data = data.table(x0=mean(plotdata$x0),y0=mean(plotdata$y0),x1=0,y1=0), 
  #           mapping=aes(label=paste("Depth",depth)))+
  theme_void()+
  theme(panel.background = element_rect(fill = 'black'))
print(plt)
