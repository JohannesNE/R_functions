#Instruction =======================
# Indlæs data med funtionen:
# data <- get_analysis_data(1)
# hvor 1 er index nr for den fil der skal indlæses
# listen af filer er i variablen ecg_files
#
# Classificer med functionen:
# results <- classify_ectopics(data)
#
# Gem med save_analysis(results)
# Load med results <- load_analysis()

#CODE =========================
library(dplyr)
library(xlsx)

# Set working directory to folder containing HRV_matlab.R

source("HRV_matlab.R")

#Folder Paths
path_ecg_mat <- "C:/Users/jene/Documents/Research/Ekstrasystoler/24H rettet/"
path_ectopics <- "C:/Users/jene/Documents/Research/Ekstrasystoler/DetectedEctopics/"
output_folder <- "C:/Users/jene/Documents/Research/Ekstrasystoler/24H types/"

#find files
ecg_files <- grep(".mat", list.files(path_ecg_mat), value = TRUE)
ectopics_files <- list.files(path_ectopics)

# === SETTINGS ======
freq <- 125
plot_padding <- 5 #seconds on both sides of ectopic

### Keyboard Shortcuts ------------------
shortcuts <- c("u" = "Unknown", 
	       "s" = "SV",
	       "v" = "V",
	       "n" = "Normal",
	       "a" = "Arythmia",
	       "m" = "EMG")

### Functions ------------------

#Load ecg as time series Chose file by index
load_analysis_data <- function(index){
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
	
#Test plot
#plot(window(aa$ECG, start=0, end=5))

classify_ectopics <- function(analysis, typed = NA){
	
	#Closure functions -----------------
	check_ectopic <- function(t_ectopic, main_title = "none", R_mark = "point", 
				  console) {
		#faster subset of raw ECG, as window is too slow
		subset_index <- function(time_s) {
			(time_s*freq-plot_padding*freq):(time_s*freq+plot_padding*freq)
		}
		readkeygraph <- function(prompt)
		{
			onKeybd <- function(key)
			{
				keyPressed <<- key
			}
			getGraphicsEvent(prompt = prompt, 
					 onMouseDown = NULL, onMouseMove = NULL,
					 onMouseUp = NULL, onKeybd = onKeybd,
					 consolePrompt = console)
			Sys.sleep(0.01)
			return(keyPressed)
		}
		#Plots ecg as vector and calculates fitting time axis, as window() is too slow
		plot(y = analysis$ECG[subset_index(t_ectopic)], 
		     x = seq(t_ectopic - plot_padding, t_ectopic + plot_padding, by =1/freq),
		     main = main_title, 
		     xlab = "Time [s]", ylab = "",
		     type = "l")
		
		points(x = t_ectopic, y = par("usr")[3], col = "blue", pch = 24,
		       cex = 2, bg = "blue")
		
		mtext("Press [ctrl + Q] to save and exit", side = 1, adj = 0, line = 4)
		
		#Marks R with vertical lines
		if (R_mark == "line") {
			abline(v = analysis$R[analysis$R > t_ectopic - plot_padding &
					 	analysis$R < t_ectopic + plot_padding] + 1 / freq, #+1/freq to make marks fit
					 	col = "red") #Mark R detections
		}
		#Mark R with crosses
		if (R_mark == "point") {
			points(x = analysis$R[analysis$R > t_ectopic - plot_padding &
					 	analysis$R < t_ectopic + plot_padding] + 1 / freq,
					 	y = analysis$ECG[analysis$R[analysis$R > t_ectopic - plot_padding &
					 			  	analysis$R < t_ectopic + plot_padding] * freq + 1], # +1 to make marks fit
					 	col = "red",
					 	pch = 4, #Type X (3 = cross)
					 	cex = 1.5) # size
		}
		
		readkeygraph("[press key to classify beat]")
	}
	#-------------------------------------------
	
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
		backup_res <<- list(file = analysis$file, df = df_ectopics)
		#Show beat and get key press
		ectopic_type <- check_ectopic(df_ectopics$time[i], 
					       main_title = sprintf("%s - Type = %s  (%i / %i)",
					       		     analysis$file_short,
					       		     df_ectopics$type[i],
					       		     i,
					       		     n_ectopics),
					       R_mark = "point", #or lines
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

review_specific <- function(analysis, typed_ectopics, target_type = "Unknown") {
	type_na <- is.na(typed_ectopics$df$type)
	new_subset <- list(file = typed_ectopics$file,
			   df = typed_ectopics$df[!type_na & typed_ectopics$df$type == target_type,])
	new_result <- classify_ectopics(analysis, new_subset)
	typed_ectopics$df$type[!type_na & typed_ectopics$df$type == target_type] <- new_result$df$type
	list(file = typed_ectopics$file,
	     df = typed_ectopics$df)
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

#Creates object exual to the saved one.
load_analysis <- function(file_path = file.choose()) {
	df <- read.csv(file_path, stringsAsFactors = FALSE)
	file_name <- strsplit(basename(file_path), "\\.")[[1]][1]
	list(file = file_name, df = df)
}
