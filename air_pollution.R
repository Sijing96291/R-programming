pollutantmean<-function(directory,pollutant,id=1:332){
        #list full list of files in the directory
        files<-list.files(directory,full.names=TRUE)
        
        #create a empty data frame
        dat<-data.frame()
        
        #for files listed in id, we combine them by rows
        for(i in id){
                dat<-rbind(dat,read.csv(files[i]))  
        }
        
        #calculate the mean for the "dat" we just created
        mean(dat[,pollutant],na.rm=TRUE)
}