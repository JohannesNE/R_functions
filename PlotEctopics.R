library(dplyr)
library(ggplot2)

source("HRV_matlab.R")

#Choose file to analyse
path_ecg_mat <- "C:/Users/jene/Documents/Research/Ekstrasystoler/24H rettet/"
ecg_files <- grep(".mat", list.files(path_ecg_mat), value = TRUE)

ectopic_times <- c(18*60+19, 29*60-0.5, 17*60+53, 60*60+36*60+33.5) #eksample
ectopics2 <- c(1,2,3,4)

freq <- 125

output_folder <- "C:/Users/jene/Documents/Research/Ekstrasystoler/24H types/"

# === VISUAL SETTINGS ======
plot_padding <- 3 #seconds on both sides of ectopic


#Load ecg as time series
if(FALSE) { 
	raw_data <- getData(paste(path_ecg_mat, ecg_files[2], sep = ""))
	raw_ekg <- ts(raw_data$CNT$EKG, frequency = 125, start = 0)
	raw_R <- raw_data$HRV$Data$T.RR # R detections
	rm(raw_data)
}

### Functions ------------------

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
	
#Test plot
plot(window(raw_ekg, start=0, end=5))

#faster subset, as window is too slow
subset_index <- function(time_s) {
	(time_s*freq-plot_padding*freq):(time_s*freq+plot_padding*freq)
}

check_ectopic <- function(t_ectopic, main_title = "none") {
	#Plots ecg as vector and calculates fitting time axis, as window() is too slow
	plot(y = raw_ekg[subset_index(t_ectopic)], x = seq(t_ectopic - plot_padding, t_ectopic + plot_padding, by =1/freq),
	     main = main_title, type = "l")
	abline(v=raw_R[raw_R > t_ectopic - plot_padding & raw_R < t_ectopic + plot_padding]+1/freq, #+1/freq to make marks fit
	      col = "red") #Mark R detections
	
	readkeygraph("[press key to classify beat]")
}
#Shows legend on plot
show_message <- function(msg) {
	legend("center", legend = msg)
}

sort_ectopics <- function(vec_ectopics){
	#Check if variable is denovo or old analysis (ie vector or data.frame)
	if (class(vec_ectopics) == "numeric") {
		df_ectopics <- data.frame(time = vec_ectopics, class = NA)
	}
	if (class(vec_ectopics) == "data.frame") {
		df_ectopics <- vec_ectopics
	}
	
	i <- ifelse(any(is.na(df_ectopics$class)) ,min(which(is.na(df_ectopics$class))), 1) #set index to first NA in data.frame
	
	n_ectopics <- nrow(df_ectopics)
	
	windows() #getGraphicsEvent only works in win.plot
	while (TRUE) {
		ectopic_class <- check_ectopic(df_ectopics$time[i], 
					       main_title = sprintf("Class = %s  (%i / %i)", 
					       		     df_ectopics$class[i],
					       		     i,
					       		     n_ectopics)) #Show beat and get key press
		#Check key press
		if (ectopic_class == "v") {
			df_ectopics$class[i] <- "V"
			show_message("V")
		}
		else if (ectopic_class == "s") {
			df_ectopics$class[i] <- "SV"
			show_message("SV")
		}
		else if (ectopic_class == "u") {
			df_ectopics$class[i] <- "unknown"
			show_message("unknown")
		}
		else if (ectopic_class == "n") {
			df_ectopics$class[i] <- "normal"
			show_message("normal")
		}
		else if (ectopic_class == "Left") {
			if (i > 1) i <- i - 1
			next
		}
		else if (ectopic_class == "Right") {
			if (i < n_ectopics) i <-  i + 1
			next
		}
		else if (ectopic_class == "ctrl-Q") {
			break
		}
		else next
		
		if (i < n_ectopics) i <- i+1
	}
	dev.off()
	df_ectopics
	
}
