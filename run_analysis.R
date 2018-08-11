run_analysis <- function() {
        
        ##Loading library
        library(reshape2)
        
        ##Set working directory
        this_wd <- getwd()
        default_wd <- "E:/7-Data science/Coursera/Data science-Specialization/2-R Programming/coursera-kanin/data/getcleanasgm"
        if(this_wd != default_wd) {
                setwd(default_wd)
                print("Reset working directory")
        }
        
        ##Step 1: Obtain mean and std indices
        a_l <- read.table("./UCI HAR Dataset/activity_labels.txt")
        f <- read.table("./UCI HAR Dataset/features.txt")
        f_ind <- grep("std\\()|mean\\()", f$V2)
        f_nme <- sub("\\(\\)", "", as.vector(f$V2[f_ind]))
        
        ##Step 2:Read all the data
        tst_s <- read.table("./UCI HAR Dataset/test/subject_test.txt")
        tst_x <- read.table("./UCI HAR Dataset/test/X_test.txt")
        tst_y <- read.table("./UCI HAR Dataset/test/y_test.txt")
        
        trn_s <- read.table("./UCI HAR Dataset/train/subject_train.txt")
        trn_x <- read.table("./UCI HAR Dataset/train/X_train.txt")
        trn_y <- read.table("./UCI HAR Dataset/train/y_train.txt")
        
        ##Step 3: Extract only the mean and std measurements
        tst_dat <- tst_x[, f_ind]
        trn_dat <- trn_x[, f_ind]
        
        ##Step 4: Merges the training and the test dataset
        tst_all <- cbind(sub = tst_s$V1, act = tst_y$V1, tst_dat)
        trn_all <- cbind(sub = trn_s$V1, act = trn_y$V1, trn_dat)
        dat_all <- rbind(trn_all, tst_all)
        colnames(dat_all) <- c("subject", "activity", f_nme)
        
        ##Step 5: Names all the variables and activities
        dat_all$activity <- factor(dat_all$activity, levels = a_l$V1, labels = a_l$V2)
        dat_all$subject <- as.factor(dat_all$subject)
        
        ## To see the overall data: write.table(dat_all, "dat_all.txt", row.names = FALSE, quote = FALSE)
        
        ##Step 6: Creates a second dataset with the average of each variable and activity
        dat_all_melted <- melt(dat_all, id = c("subject", "activity"))
        dat_all_mean <- dcast(dat_all_melted, subject + activity ~ variable, mean)
        
        write.table(dat_all_mean, "tidy.txt", row.names = FALSE, quote = FALSE)

}