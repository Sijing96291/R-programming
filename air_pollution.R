###############part1###############
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


##################part2########################
complete <- function(directory,id=1:332){
        files<-list.files(directory,full.names=TRUE)
        dat<-data.frame()
        
        for(i in id){
                #number of obs in the selected file that are not missing
                nobs<-sum(complete.cases(read.csv(files[i])))
                
                #create a data frame that contains one obs for each file listed in id
                tmp<-data.frame(i,nobs)
                
                #for each iteration, we add one row to the empty data frame "dat"
                #from "tmp"
                dat<-rbind(dat,tmp)
        }
        
        #give the colname for the data frame "dat"
        colnames(dat)<-c("id","nobs")
        dat
}