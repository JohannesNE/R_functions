library(dplyr)
library(xlsx)

source("HRV_matlab.R")

#Folder Paths
path_ecg_mat <- "C:/Users/jene/Documents/Research/Ekstrasystoler/24H rettet/"
path_ectopics <- "C:/Users/jene/Documents/Research/Ekstrasystoler/DetectedEctopics/"

#find files
ecg_files <- grep(".mat", list.files(path_ecg_mat), value = TRUE)
ectopics_files <- list.files(path_ectopics)

ectopic_times_ex <- c(18*60+19, 29*60-0.5, 17*60+53, 60*60+36*60+33.5) #eksample for ecg_files[2]

freq <- 125

output_folder <- "C:/Users/jene/Documents/Research/Ekstrasystoler/24H types/"

# === VISUAL SETTINGS ======
plot_padding <- 5 #seconds on both sides of ectopic

### Keyboard Shortcuts ------------------
shortcuts <- c("u" = "Unknown", 
	       "s" = "SV",
	       "v" = "V",
	       "n" = "Normal",
	       "a" = "Arythmia",
	       "m" = "EMG")

### Functions ------------------

#Load ecg as time series
get_analysis_data <- function(index){
	#Check that file has match in ectopics folder (tests that file name exists in ectopic folder)
	if(sum(grepl(strsplit(ecg_files[index], "\\.")[[1]][1], ectopics_files)) == 1) {
		raw_data <- getData(paste(path_ecg_mat, ecg_files[index], sep = "")) #loads kubios .mat file
		raw_ecg <- ts(raw_data$CNT$EKG, frequency = 125, start = 0)
		raw_R <- raw_data$HRV$Data$T.RR # R detections
		rm(raw_data)
		
		ectopic_file <- grep(strsplit(ecg_files[index], "\\.")[[1]][1], ectopics_files,
				     value = TRUE)
		
		message(sprintf("ECG: %s \nEctopic: %s", ecg_files[index], ectopic_file))
		
		#read xlsx and convert to vector
		ectopic_times <- read.xlsx(paste(path_ectopics, ectopic_file, sep = ""), 
						 1, header = FALSE)[,1]
		
		#Convert ms to s
		ectopic_times <- ectopic_times / 1000
		
		#Return list of raw data incl file name
		list(ECG = raw_ecg, R = raw_R, ectopics = ectopic_times, 
		     file = strsplit(ecg_files[index], "\\.")[[1]][1], 
		     file_short = substr(ectopic_file, 11, 16))	
	}
	else if(sum(grepl(strsplit(ecg_files[index], "\\.")[[1]][1], ectopics_files)) == 0) {
		return(message("No match in ectopic folder"))
	}
	#Excel generates a copy of any open file. This creates an error
	else if(sum(grepl(strsplit(ecg_files[index], "\\.")[[1]][1], ectopics_files)) > 1) {
		return(message("More than 1 match... probably due to file being open in excel"))
	}
	

}

readkeygraph <- function(prompt, console = "ss")
{
	getGraphicsEvent(prompt = prompt, 
		onMouseDown = NULL, onMouseMove = NULL,
		onMouseUp = NULL, onKeybd = onKeybd,
		consolePrompt = console)
	Sys.sleep(0.01)
	return(keyPressed)
}

onKeybd <- function(key)
{
	keyPressed <<- key
}
	
#Test plot
#plot(window(aa$ECG, start=0, end=5))

#faster subset, as window is too slow
subset_index <- function(time_s) {
	(time_s*freq-plot_padding*freq):(time_s*freq+plot_padding*freq)
}

#Called by classify_ectopics
check_ectopic <- function(t_ectopic, main_title = "none", R_mark = "point", raw_ecg, raw_R, 
			  console) {
	#Plots ecg as vector and calculates fitting time axis, as window() is too slow
	plot(y = raw_ecg[subset_index(t_ectopic)], 
	     x = seq(t_ectopic - plot_padding, t_ectopic + plot_padding, by =1/freq),
	     main = main_title, 
	     xlab = "Time [s]", ylab = "",
	     type = "l")
	
	points(x = t_ectopic, y = par("usr")[3], col = "blue", pch = 24,
	       cex = 2, bg = "blue")
	
	#Marks R with vertical lines
	if (R_mark == "line") {
		abline(v = raw_R[raw_R > t_ectopic - plot_padding &
				 	raw_R < t_ectopic + plot_padding] + 1 / freq, #+1/freq to make marks fit
				 	col = "red") #Mark R detections
	}
	#Mark R with crosses
	if (R_mark == "point") {
		points(x = raw_R[raw_R > t_ectopic - plot_padding &
				 	raw_R < t_ectopic + plot_padding] + 1 / freq,
				 	y = raw_ecg[raw_R[raw_R > t_ectopic - plot_padding &
				 			  	raw_R < t_ectopic + plot_padding] * freq + 1], # +1 to make marks fit
				 	col = "red",
				 	pch = 4, #Type X (3 = cross)
				 	cex = 1.5) # size
	}
	
	readkeygraph("[press key to classify beat]", console)
}
#Shows legend on plot
show_message <- function(msg) {
	legend("center", legend = msg)
}

classify_ectopics <- function(analysis, typed = NA){
	#Check if variable is denovo or (partly) analysed (typed contains analysed dataframe)
	if (class(analysis) == "list") {
		if (all(is.na(typed))) {
			df_ectopics <- data.frame(time = analysis$ectopics, type = NA)
		}
		else if (names(typed)[2] == "df") {
			if (typed$file != analysis$file){ #Checks that analysis and type are from same file
				message("OBS: Analysis and typed files does not match!")
			}
			df_ectopics <- typed$df
		}
		else stop("Wrong typed format")
	}
	else stop("Wrong analysis format")
	
	i <- ifelse(any(is.na(df_ectopics$type)) ,min(which(is.na(df_ectopics$type))), 1) #set index to first NA in data.frame
	
	n_ectopics <- nrow(df_ectopics)
	
	windows(1600, 600) #getGraphicsEvent only works in win.plot
	
	ectopic_type <- NA #to avoid error in ifelse function used to write last input to console
	while (TRUE) {
		#Show beat and get key press
		ectopic_type <- check_ectopic(df_ectopics$time[i], 
					       main_title = sprintf("%s - Type = %s  (%i / %i)",
					       		     analysis$file_short,
					       		     df_ectopics$type[i],
					       		     i,
					       		     n_ectopics),
					       R_mark = "point", #or lines
					      raw_ecg = analysis$ECG,
					      raw_R = analysis$R,
					      #Show last classification in console
					      console = ifelse(ectopic_type %in% names(shortcuts),
					      		 shortcuts[ectopic_type], "--")) 
		#Check key press
		if (ectopic_type %in% names(shortcuts)) {
			df_ectopics$type[i] <- shortcuts[ectopic_type]
		}
		else if (ectopic_type == "Left") {
			if (i > 1) i <- i - 1
			next
		}
		else if (ectopic_type == "Right") {
			if (i < n_ectopics) i <-  i + 1
			next
		}
		else if (ectopic_type == "ctrl-Q") {
			break
		}
		else next
		
		if (i < n_ectopics) i <- i+1
	}
	dev.off()
	list(file = analysis$file, df = df_ectopics)
	
}

save_analysis <- function(typed_ectopics, path = output_folder) {
	file_path <- paste0(path, typed_ectopics$file, ".csv")
	if (file.exists(file_path)){
		YN <- readline("File already exists. Overwrite? [Y/N]")
		if (toupper(YN) != "Y") stop("File not saved")
	}
	write.csv(typed_ectopics$df, file_path, row.names = FALSE) #add file
	message(sprintf("Saved: %s", file_path))
}
