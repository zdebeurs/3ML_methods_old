# Zoe de Beurs (working with Saku Vrtilek, Nazma Islam, and Giri Gopalan). Some of this code is adapted from https://github.com/ggopalan/XRay-Binary-Classification.

# Script aims to take a sample such that each source has an average of ~200 pts per sample

# Be sure to install "ggplot2" if not already installed with "install.packages("ggplot2")"
library(ggplot2)

#Read in the data (Adjust this path based on where you download your files )
setwd("~/Documents/Summer19/SAO_Research/3ML_methods_for_XRB_classification/")

# Name of folder which contains XRB sources 
file_loc = "./Training_and_Testing_33"

# Set an output folder for the sampled (20%) observations
output_folder = "Sampled_20percent_Training_and_Testing/"
dir.create(paste("Sampled_20percent_Training_and_Testing/"), showWarnings = FALSE)

# List the files in this folder
file_list <- list.files(file_loc)
test_systems = strsplit(file_list, ".asc")
test_systems = unlist(test_systems)

# Print the XRB systems
test_systems

# Create an empty list to store the data
fulldata <- {}
# Store the pre-sampled length of each XRB system
orig_system_length <- {}

# Loop through each of the XRB systems
for(i in 1:length(test_systems)){
  # create a dummy variable to read in the systems
  dummy <- read.table(paste(file_loc,"/",test_systems[i], ".asc",sep=""), skip = 1)
  print(paste(print(test_systems[i]),": ",length(dummy[,1]), sep=""))
  
  # Store the original length of each system (before sampling)
  orig_system_length <- rbind(orig_system_length, length(dummy[,1]))
  
  # Store the data
  fulldata <- rbind(fulldata,dummy)
}

#Add a column for star type in numbers (1 = BH,2 = Non-pulsing NS,3 = Pulsar) for each type of source 
compact <- rep(1,dim(fulldata)[1])
hmbh <- which(fulldata[,2] == 'HMBH')
lmbh <- which(fulldata[,2] == 'LMBH')
bh <- c(hmbh,lmbh)
atoll <- which(fulldata[,2] == 'Atoll')
zsource <- which(fulldata[,2] == 'Zsource')
bursters_as_np <- which(fulldata[,2] == 'Burster') 
pulsars <- which(fulldata[,2] == 'Pulsar')
nonpulsar <- c(atoll,zsource,bursters_as_np)
compact[nonpulsar] <- rep(2,length(nonpulsar))
compact[pulsars] <- rep(3,length(pulsars))

# Add a column for star type written out ("BH","NS","Pulsar")
compact_s <- rep("BH",dim(fulldata)[1])
hmbh <- which(fulldata[,2] == 'HMBH')
lmbh <- which(fulldata[,2] == 'LMBH')
bh <- c(hmbh,lmbh)
atoll <- which(fulldata[,2] == 'Atoll')
zsource <- which(fulldata[,2] == 'Zsource')
bursters_as_np <- which(fulldata[,2] == 'Burster') 
pulsars <- which(fulldata[,2] == 'Pulsar')
nonpulsar <- c(atoll,zsource,bursters_as_np)
compact_s[nonpulsar] <- rep("NS",length(nonpulsar))
compact_s[pulsars] <- rep("Pulsar",length(pulsars))
fulldata <- cbind(compact,compact_s,fulldata)
names(fulldata) <- c("Compact_Star","Star_Type","System","Type","Date","CC1","CC2","CC3")

#Set an upper bound for number of data points to include in a training data set. This is set to 20%, but can be changed
data_size <- .2*dim(fulldata)[1]

#Determine vector of probabilities to sample from, which are inversely proprtional to the system size
systems <- levels(fulldata$System)
probs <- sapply(systems, function(x) 1/length(which(fulldata$System == x)))

# probs
probs <- probs/sum(probs)
sampling_probs <- sapply(seq(dim(fulldata)[1]), function(x) probs[fulldata[x,3]])


# Sample each data point with the corresponding probability
samples <- sample(dim(fulldata)[1],size=data_size,replace=FALSE,prob=sampling_probs)
training <- fulldata[samples,]

# Create a data frame
emp.data <- data.frame(
  Classification = training["Compact_Star"],
  System = training["System"],
  Star_Type = training["Star_Type"],
  Type = training["Type"],
  Date = training["Date"],
  CC1 = training["CC1"],
  CC2 = training["CC2"],
  CC3 = training["CC3"],
  stringsAsFactors = FALSE
)

# Check the number of total observations in sampled data
length(emp.data[,3])

# create a list to store the number of observations you ended up with after sampling
simp_full_data_list = {}
num_of_pts = c()

# Loop through individual test systems to generate individual files for each XRB source
for (i in 1:length(test_systems)){
  # Create a subset of observations from only that specific system
  sub = subset(emp.data, System==test_systems[i])
  
  # Define the name of the output file
  output_filename_asc = paste(output_folder,
                              test_systems[i],'.asc', sep="")
  
  # Create the data frame 
  clean.data <- data.frame(
    source_name = test_systems[i],
    classification = sub["Type"],
    MJD = sub["Date"],
    soft_colors = sub["CC1"],
    hard_colors = sub["CC2"],
    relative_intensity = sub["CC3"],
    stringsAsFactors = FALSE
  )
  # Print the name of the system an the number of points after sampling
  print(paste((test_systems[i]),", number of data pts: ", length(sub["CC1"][,1]), sep=""))
  
  # count the number of points and append to the list "num_of_pts"
  num_of_pts = append(num_of_pts, c(length(sub["CC1"][,1])))
  
  #WRITE NEW FILES
  write.table(clean.data, file=output_filename_asc, append = FALSE, sep = "    ", dec = ".",
              row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  # Record how many observations remained for this XRB systems after sampling
  simp_full_data = c(test_systems[i], orig_system_length[i], length(sub["CC1"][,1]))
  names(simp_full_data) = c("System","Sign. Points","Sampled points")
  simp_full_data_list <- rbind(simp_full_data_list,simp_full_data)
}

# Print and write the number of sampled observations to a csv_file
simp_full_data_list
write.csv(simp_full_data_list,'number_of_sampled_pts20.csv')

mean(num_of_pts)
sd(num_of_pts)

# Run code below to create histograms of the number of observations before and after subsampling

# Counts number of points before sampling
num_of_pts_bf = c()
for (i in 1:length(test_systems)){
  sub_bf = subset(fulldata, System==test_systems[i])
  num_of_pts_bf = append(num_of_pts_bf, c(length(sub_bf["CC1"][,1]))) 
}


# Plot before sampling
par(mfrow=c(1,2))
barplot(num_of_pts_bf, xlab="Sources", ylab="Number of observations", main="Before Subsampling")

minimum= min(num_of_pts)
maximum= max(num_of_pts)
# Plot After sampling
barplot(num_of_pts, xlab="Sources", ylab="Number of observations", main=paste("After Subsampling"))#, min:",minimum, "max:" , maximum))

print("End of Sampling Script")