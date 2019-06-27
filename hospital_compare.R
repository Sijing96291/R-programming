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





#Write a function called rankhospital that returns a character vector with the name of the hospital
#that has the ranking specified by the num argument.

rankhospital<-function(state,outcome,num="best"){
        data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
        st<-unique(data$State)
        
        if (state %in% st){
                data_sub<-subset(data,data$State==state)
                
                if(outcome=="heart attack"){
                        
                        #create a new data frame "dat" contains only column 2 and 11
                        dat<-data_sub[,c(2,11)]
                        
                        #rank the data set by heart attack rate in ascending order
                        #removing all missing values
                        dat_rank<-dat[order(as.numeric(dat[,2]),dat[,1],na.last=NA),]
                        
                        #create a numeric vector "rank" in which contains ranking number
                        #for each hospital in a given State
                        rank<-seq(1,nrow(dat_rank),1)
                        
                        #create a new column named "Rank"
                        dat_rank$Rank<-rank
                        
                        #select the xth rank from the dataset
                        if(num>nrow(dat_rank)&&(num!="worst")&&(num!="best")){
                               print("NA") 
                        }else if(num=="best"){
                                print(dat_rank[1,1])
                        }else if(num=="worst"){
                                print(dat_rank[nrow(dat_rank),1])
                        }else{
                                print(dat_rank[num,1])
                                #ifelse(num=="best",print(dat_rank[1,1]),
                                #ifelse(num=="worst",print(dat_rank[nrow(dat_rank),1]),print(dat_rank[num,1])))
                        }
                        
                       
                        
                }else if (outcome=="heart failure"){
                        
                        dat<-data_sub[,c(2,17)]
                        dat_rank<-dat[order(as.numeric(dat[,2]),dat[,1],na.last=NA),]
                        rank<-seq(1,nrow(dat_rank),1)
                        dat_rank$Rank<-rank
                        
                        if(num>nrow(dat_rank)&&(num!="worst")&&(num!="best")){
                                print("NA") 
                        }else if(num=="best"){
                                print(dat_rank[1,1])
                        }else if(num=="worst"){
                                print(dat_rank[nrow(dat_rank),1])
                        }else{
                                print(dat_rank[num,1])
                                #ifelse(num=="best",print(dat_rank[1,1]),
                                #ifelse(num=="worst",print(dat_rank[nrow(dat_rank),1]),print(dat_rank[num,1])))
                        }
                        
                }else if (outcome=="pneumonia"){
                        
                        dat<-data_sub[,c(2,23)]
                        dat_rank<-dat[order(as.numeric(dat[,2]),dat[,1],na.last=NA),]
                        rank<-seq(1,nrow(dat_rank),1)
                        dat_rank$Rank<-rank
                        if(num>nrow(dat_rank)&&(num!="worst")&&(num!="best")){
                                print("NA") 
                        }else if(num=="best"){
                                print(dat_rank[1,1])
                        }else if(num=="worst"){
                                print(dat_rank[nrow(dat_rank),1])
                        }else{
                                print(dat_rank[num,1])
                                #ifelse(num=="best",print(dat_rank[1,1]),
                                #ifelse(num=="worst",print(dat_rank[nrow(dat_rank),1]),print(dat_rank[num,1])))
                        }
                        
                        
                }else{
                        stop("invalid outcome")
                }
        }else{
                stop("invalid state")
                
        }
}

        



#write a function that returns a 2-column data frame containing the hospital in each state
#that has the ranking specified in num

rankall<-function(outcome,num="best"){
        data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
        
        
        if(outcome=="heart attack"){
                a_data<-data[,c(2,7,11)]
                
                #split the dataset into groups by State
                # s is a list
                s<-split(a_data,a_data$State)
                
                #apply order() function to each group,and remove NA values
                #l is a list
                l<-lapply(s,function(x) x[order(as.numeric(x[,3]),x[,1],na.last=NA),])
                
                #generate a column "Rank" for hospitals grouped by State
                #r is a list
                r<-lapply(l,function(x) cbind(x,"Rank"=seq(1,nrow(x),1)))
                
                if (num=="best"){
                        
                        #select the first row from each State
                        f<-lapply(r,function(x) x[1,c(1,2)])
                        
                        #combine list of data frames into one data frame
                        dat<-do.call(rbind,f)
                        
                }else if(num=="worst"){
                        
                        #select the last row from each State
                         f<-lapply(r,function(x) x[nrow(x),c(1,2)])
                         dat<-do.call(rbind,f)
                        
                }else {
                        #select the the "num"th row from each State
                        f<-lapply(r,function(x) x[num,c(1,2)])
                        dat<-do.call(rbind,f)
                }
                
                
                
                
        }else if(outcome=="heart failure"){
                
                b_data<-data[,c(2,7,17)]
                s<-split(b_data,b_data$State)
                l<-lapply(s,function(x) x[order(as.numeric(x[,3]),x[,1],na.last=NA),])
                r<-lapply(l,function(x) cbind(x,"Rank"=seq(1,nrow(x),1)))
                
                if (num=="best"){
                        
                        #select the first row from each State
                        f<-lapply(r,function(x) x[1,c(1,2)])
                        
                        #combine list of data frames into one data frame
                        dat<-do.call(rbind,f)
                        
                }else if(num=="worst"){
                        
                        #select the last row from each State
                        f<-lapply(r,function(x) x[nrow(x),c(1,2)])
                        dat<-do.call(rbind,f)
                        
                }else {
                        #select the the "num"th row from each State
                        f<-lapply(r,function(x) x[num,c(1,2)])
                        dat<-do.call(rbind,f)
                }
                
        }else if(outcome=="pneumonia"){
                
                c_data<-data[,c(2,7,23)]
                s<-split(b_data,b_data$State)
                l<-lapply(s,function(x) x[order(as.numeric(x[,3]),x[,1],na.last=NA),])
                r<-lapply(l,function(x) cbind(x,"Rank"=seq(1,nrow(x),1)))
                
                if (num=="best"){
                        
                        #select the first row from each State
                        f<-lapply(r,function(x) x[1,c(1,2)])
                        
                        #combine list of data frames into one data frame
                        dat<-do.call(rbind,f)
                        
                }else if(num=="worst"){
                        
                        #select the last row from each State
                        f<-lapply(r,function(x) x[nrow(x),c(1,2)])
                        dat<-do.call(rbind,f)
                        
                }else {
                        #select the the "num"th row from each State
                        f<-lapply(r,function(x) x[num,c(1,2)])
                        dat<-do.call(rbind,f)
                }
                
        }else{
                stop("invalid outcome")
        }
}

