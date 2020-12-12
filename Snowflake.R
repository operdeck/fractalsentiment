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

draw <- function(depth, length, id=0)
{
  if (depth == 0) {
    move(length, id=id)
  } else {
    draw(depth-1, length/3, id=id)
    turn(pi/3)
    draw(depth-1, length/3, id=id)
    turn(-2*pi/3)
    draw(depth-1, length/3, id=id)
    turn(pi/3)
    draw(depth-1, length/3, id=id)
  }
}

nflakes <- 100
size <- 10
depth <- 4
for (i in seq(nflakes)) {
  flakesize <- runif(1, min=0, max=size/5)
  xstart <- runif(1, min=-size, max=size)
  ystart <- runif(1, min=-size, max=size)
  
  angle <- 0
  
  draw(depth, flakesize, id=i)
  turn(-2*pi/3)
  draw(depth, flakesize, id=i)
  turn(-2*pi/3)
  draw(depth, flakesize, id=i)
}

plotdata <- as.data.table(t(as.data.table(segments)))
setnames(plotdata, c("x0","y0","x1","y1","depth","id"))
plotdata[, i:=seq(.N)]
plt <- ggplot(plotdata, aes(x=x0,y=y0,xend=x1,yend=y1)) +
  #geom_segment(aes(color=log(depth)))+
  geom_polygon(aes(group=id,fill=id),alpha=0.6)+
  scale_fill_gradient(low="yellow", high="orange", guide=F)+
  coord_fixed(ratio=1)+
  #geom_point()+
  #geom_text(aes(label=i))+
  #geom_label(data = data.table(x0=mean(plotdata$x0),y0=mean(plotdata$y0),x1=0,y1=0), 
  #           mapping=aes(label=paste("Depth",depth)))+
  theme_void()+
  theme(panel.background = element_rect(fill = 'lightblue'))
print(plt)

# library(gganimate)
# p1 <- copy(plotdata)[, seq:=1]
# p2 <- copy(plotdata)[, seq:=2]
# p2[, x0:=x0-4]
# p2[, x1:=x1-4]
# p<-ggplot( rbind(p1, p2), aes(x=x0,y=y0,xend=x1,yend=y1)) +
#   geom_polygon(aes(group=id,fill=id),alpha=0.6)+
#   transition_time(seq)
# p
# 
# 
