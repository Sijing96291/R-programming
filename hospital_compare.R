#read the dataset in, specify all the columns are character.
outcome<-read.csv("outcome-of-care-measures.csv",colClasses = "character")

#transfer the column 11 to numeric
outcome[,11]<-as.numeric(outcome[,11])



#Returns a character vector with the name of the hospital that has
#the best(lowest) 30-day mortality for the specified outcome in that state

best<-function(state,outcome){
        data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
        st<-unique(data$State)
        
        if (state %in% st){
                data_sub<-subset(data,data$State==state)
                
                if(outcome=="heart attack"){
                        
                        data_sub[,11]<-as.numeric(data_sub[,11]) 
                        data_best<- subset(data_sub,data_sub[,11]==min(data_sub[,11],na.rm=TRUE))
                        data_best[,2] 
                        
                }else if (outcome=="heart failure"){
                        
                        data_sub[,17]<-as.numeric(data_sub[,17]) 
                        data_best<- subset(data_sub,data_sub[,17]==min(data_sub[,17],na.rm=TRUE))
                        data_best[,2]
                        
                }else if (outcome=="pneumonia"){
                        
                        data_sub[,23]<-as.numeric(data_sub[,23]) 
                        data_best<-subset(data_sub,data_sub[,23]==min(data_sub[,23],na.rm=TRUE))
                        data_best[,2]
                        
                }else{
                        stop("invalid outcome")
                }
        }else{
                stop("invalid state")
                
        }
}









