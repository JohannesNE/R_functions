readkeygraph <- function(prompt)
{
	getGraphicsEvent(prompt = prompt, 
			 onMouseDown = NULL, onMouseMove = NULL,
			 onMouseUp = NULL, onKeybd = onKeybd,
			 consolePrompt = "[click on graph then follow top prompt to continue]")
	Sys.sleep(0.01)
	return(keyPressed)
}

onKeybd <- function(key) 
{
	keyPressed <<- key
}

xaxis=c(1:10) # Set up the x-axis.
yaxis=runif(10,min=0,max=1) # Set up the y-axis.
plot(xaxis,yaxis)

for (i in xaxis)
{
	# On each keypress, color the points on the graph in red, one by one.
	points(i,yaxis[i],col="red", pch=19)
	keyPressed = readkeygraph("[press any key to continue]")
	print(keyPressed)
}