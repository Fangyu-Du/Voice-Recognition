# BDML
# Group Project

### 1. clean data

# clean the dataset so that it only contains data with
# file names and valid gender labels
unclean = read.csv("cv-valid-train.csv")
View(unclean)
gender_data = subset(unclean,select=c('filename','gender'))
clean_data = gender_data[!(gender_data$gender)=="",]
View(clean_data)

# randomly select 15000 data for use
indices = sample(1:nrow(clean_data), 15000)
data_for_use = clean_data[indices,]
View(data_for_use)

# change the filename into wav extension
data_for_use = read.csv("data_for_use.csv")

for(i in 1:nrow(data_for_use)){
  oldfilename = as.character(data_for_use$filename[i])
  name = substr(oldfilename,16,28)
  newfilename = paste(name,".wav",sep="")
  
  data_for_use$filename = as.character(data_for_use$filename)
  data_for_use$filename[data_for_use$filename == oldfilename] = newfilename
}

View(data_for_use)
colnames(data_for_use) <- c("filename", "label")
write.csv(data_for_use,"wav_data.csv", row.names = FALSE)

# add columns to wav dataset (for storing voice attributes)
wav_data = read.csv("wav_data.csv")
namevector = c("meanfreq","sd","median","Q25","Q75",
               "IQR","skew","kurt","sp.ent","sfm",
               "mode","centroid","meanfun","minfun","maxfun",
               "meandom","mindom","maxdom","dfrange","modindx")
wav_data[,namevector] <- NA
View(wav_data)
write.csv(wav_data,"wav_data.csv", row.names = FALSE)

### 2. convert mp3 to wav files

library(tuneR)

path = '~/Desktop/cv-valid-train/'
file.names = dir(path, pattern =".mp3")
print(file.names)

for(i in 1:length(file.names)){
  path_current_name = paste('~/Desktop/cv-valid-train/',file.names[i],sep="")
  mp3_file = readMP3(path_current_name)
  current_name = tools::file_path_sans_ext(file.names[i])
  new_name = paste(current_name, "wav", sep=".")
  path_new_name = paste('~/Desktop/cv-valid-train/',new_name,sep="")
  writeWave(mp3_file,path_new_name, extensible=FALSE)
}

### 3. extract features

packages <- c('tuneR', 'seewave', 'fftw', 'caTools', 'randomForest', 'warbleR', 'mice', 'e1071', 'rpart', 'rpart-plot', 'xgboost', 'e1071')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
library(tuneR)
library(seewave)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(warbleR)
library(mice)
library(xgboost)
library(e1071)

path = '~/Desktop/cv-valid-train/'
file.names = dir(path, pattern =".wav")
print(file.names)
wav_data = read.csv("wav_data.csv")

# set parameters
wl = 2048
threshold = 5
bp = c(0,22)
b=bp

for(i in 1:length(file.names)){
  current_file_name = paste(path,file.names[i],sep="")
  r = tuneR::readWave(current_file_name, from = 1, to = Inf, units = "seconds") 
  songspec <- seewave::spec(r, f = r@samp.rate, plot = FALSE)
  analysis <- seewave::specprop(songspec, f = r@samp.rate, flim = c(0, 280/1000), plot = FALSE)
  
  #save parameters
  meanfreq <- analysis$mean/1000
  sd <- analysis$sd/1000
  median <- analysis$median/1000
  Q25 <- analysis$Q25/1000
  Q75 <- analysis$Q75/1000
  IQR <- analysis$IQR/1000
  skew <- analysis$skewness
  kurt <- analysis$kurtosis
  sp.ent <- analysis$sh
  sfm <- analysis$sfm
  mode <- analysis$mode/1000
  centroid <- analysis$cent/1000
  ff <- seewave::fund(r, f = r@samp.rate, ovlp = 50, threshold = threshold, 
                      fmax = 280, ylim=c(0, 280/1000), plot = FALSE, wl = wl)[, 2]
  meanfun<-mean(ff, na.rm = T)
  minfun<-min(ff, na.rm = T)
  maxfun<-max(ff, na.rm = T)
  
  #Dominant frecuency parameters
  y <- seewave::dfreq(r, f = r@samp.rate, wl = wl, ylim=c(0, 280/1000), ovlp = 0, plot = F, threshold = threshold, bandpass = b * 1000, fftw = TRUE)[, 2]
  meandom <- mean(y, na.rm = TRUE)
  mindom <- min(y, na.rm = TRUE)
  maxdom <- max(y, na.rm = TRUE)
  dfrange <- (maxdom - mindom)
  
  #modulation index calculation
  changes <- vector()
  for(j in which(!is.na(y))){
    change <- abs(y[j] - y[j + 1])
    changes <- append(changes, change)
  }
  if(mindom==maxdom) modindx<-0 else modindx <- mean(changes, na.rm = T)/dfrange
  
  # replace values
  wav_data[i,3] = meanfreq
  wav_data[i,4] = sd
  wav_data[i,5] = median
  wav_data[i,6] = Q25
  wav_data[i,7] = Q75
  wav_data[i,8] = IQR
  wav_data[i,9] = skew
  wav_data[i,10] = kurt
  wav_data[i,11] = sp.ent
  wav_data[i,12] = sfm
  wav_data[i,13] = mode
  wav_data[i,14] = centroid
  wav_data[i,15] = meanfun
  wav_data[i,16] = minfun
  wav_data[i,17] = maxfun
  wav_data[i,18] = meandom
  wav_data[i,19] = mindom
  wav_data[i,20] = maxdom
  wav_data[i,21] = dfrange
  wav_data[i,22] = modindx
}

View(wav_data)
write.csv(wav_data,"wav_data.csv", row.names = FALSE)
clean_wav_data = na.omit(wav_data)
write.csv(wav_data,"wav_data.csv", row.names = FALSE)
View(clean_wav_data)
summary(clean_wav_data)
write.csv(clean_wav_data,"clean_wav_data.csv", row.names = FALSE)

### 4. merge data

# 3000+ data from kaggle
old_data = read.csv("voice.csv")
View(old_data)

# 15000 new data extracted by our own
new_data = read.csv("clean_wav_data.csv")

# modify the columns order to match with old dataset
new_data = new_data[,-1]
new_data1 = new_data[,c(2:21)]
new_data2 = new_data[,c(1)]
new_data = cbind(new_data1, new_data2)
colnames(new_data)[21] ="label"
View(new_data)

# merge data
complete_data <- rbind(old_data, new_data)
View(complete_data)
write.csv(complete_data,"complete_data.csv",row.names = FALSE)
