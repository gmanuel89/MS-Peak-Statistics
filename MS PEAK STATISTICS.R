# Clear the console
cat("\014")
# Empty the workspace
rm(list = ls())

ms_peak_statistics <- function() {
    
    #################### MS PEAK STATISTICS (TCL-TK GUI) ############
    
    
    ### Program version (Specified by the program writer!!!!)
    R_script_version <- "2017.07.12.0"
    ### Force update (in case something goes wrong after an update, when checking for updates and reading the variable force_update, the script can automatically download the latest working version, even if the rest of the script is corrupted, because it is the first thing that reads)
    force_update <- FALSE
    ### GitHub URL where the R file is
    github_R_url <- "https://raw.githubusercontent.com/gmanuel89/MS-Peak-Statistics/master/MS%20PEAK%20STATISTICS.R"
    ### GitHub URL of the program's WIKI
    github_wiki_url <- "https://github.com/gmanuel89/MS-Peak-Statistics/wiki"
    ### Name of the file when downloaded
    script_file_name <- "MS PEAK STATISTICS"
    # Change log
    change_log <- "1. New name!!!\n2. Parallel processing in correlation enabled!"
    
    
    
    
    
    
    ########## FUNCTIONS
    # Check internet connection
    check_internet_connection <<- function(method = "getURL", website_to_ping = "www.google.it") {
        ##### Start with getURL...
        there_is_internet <- FALSE
        ##### GET URL
        if (method == "getURL") {
            try({
                # Install the RCurl package if not installed
                if ("RCurl" %in% installed.packages()[,1]) {
                    library(RCurl)
                } else {
                    # Check for the personal local library presence before installing (~/R/packages/), then instlall in the local library
                    if (!dir.exists(Sys.getenv("R_LIBS_USER"))) {
                        dir.create(Sys.getenv("R_LIBS_USER"))
                    }
                    .libPaths(Sys.getenv("R_LIBS_USER"))
                    install.packages("RCurl", repos = "http://cran.mirror.garr.it/mirrors/CRAN/", quiet = TRUE, verbose = FALSE, lib = Sys.getenv("R_LIBS_USER"))
                    library(RCurl)
                }
            }, silent = TRUE)
            there_is_internet <- FALSE
            try({
                there_is_internet <- is.character(getURL(u = website_to_ping, followLocation = TRUE, .opts = list(timeout = 1, maxredirs = 2, verbose = FALSE)))
            }, silent = TRUE)
        }
        ##### If getURL failed... Go back to ping (which should never fail)
        ##### PING
        if (method == "ping" || there_is_internet == FALSE) {
            if (Sys.info()[1] == "Linux") {
                # -c: number of packets sent/received (attempts) ; -W timeout in seconds
                there_is_internet <- !as.logical(system(command = paste("ping -c 1 -W 2", website_to_ping), intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE))
            } else if (Sys.info()[1] == "Windows") {
                # -n: number of packets sent/received (attempts) ; -w timeout in milliseconds
                there_is_internet <- !as.logical(system(command = paste("ping -n 1 -w 2000", website_to_ping), intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE))
            } else {
                there_is_internet <- !as.logical(system(command = paste("ping", website_to_ping), intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE))
            }
        }
        return(there_is_internet)
    }
    
    # Install and load required packages
    install_and_load_required_packages <<- function(required_packages, repository = "http://cran.mirror.garr.it/mirrors/CRAN/", update_packages = FALSE, print_messages = FALSE) {
        ### Check internet connection
        there_is_internet <- check_internet_connection(method = "getURL", website_to_ping = "www.google.it")
        ########## Update all the packages (if there is internet connection)
        if (update_packages == TRUE) {
            if (there_is_internet == TRUE) {
                ##### If a repository is specified
                if (repository != "" || !is.null(repository)) {
                    # Check for the personal local library presence before installing (~/R/packages/), then install in the local library
                    if (!dir.exists(Sys.getenv("R_LIBS_USER"))) {
                        dir.create(Sys.getenv("R_LIBS_USER"))
                    }
                    .libPaths(Sys.getenv("R_LIBS_USER"))
                    update.packages(repos = repository, ask = FALSE, checkBuilt = TRUE, quiet = TRUE, verbose = FALSE, lib.loc = Sys.getenv("R_LIBS_USER"))
                } else {
                    # Check for the personal local library presence before installing (~/R/packages/), then instlall in the local library
                    if (!dir.exists(Sys.getenv("R_LIBS_USER"))) {
                        dir.create(Sys.getenv("R_LIBS_USER"))
                    }
                    .libPaths(Sys.getenv("R_LIBS_USER"))
                    update.packages(ask = FALSE, checkBuilt = TRUE, quiet = TRUE, verbose = FALSE, lib.loc = Sys.getenv("R_LIBS_USER"))
                }
                if (print_messages == TRUE) {
                    cat("\nPackages updated\n")
                }
            } else {
                if (print_messages == TRUE) {
                    cat("\nPackages cannot be updated due to internet connection problems\n")
                }
            }
        }
        ##### Retrieve the installed packages
        installed_packages <- installed.packages()[,1]
        ##### Determine the missing packages
        missing_packages <- required_packages[!(required_packages %in% installed_packages)]
        ##### If there are packages to install...
        if (length(missing_packages) > 0) {
            ### If there is internet...
            if (there_is_internet == TRUE) {
                ### If a repository is specified
                if (repository != "" || !is.null(repository)) {
                    # Check for the personal local library presence before installing (~/R/packages/), then instlall in the local library
                    if (!dir.exists(Sys.getenv("R_LIBS_USER"))) {
                        dir.create(Sys.getenv("R_LIBS_USER"))
                    }
                    .libPaths(Sys.getenv("R_LIBS_USER"))
                    install.packages(missing_packages, repos = repository, quiet = TRUE, verbose = FALSE, lib = Sys.getenv("R_LIBS_USER"))
                } else {
                    ### If NO repository is specified
                    # Check for the personal local library presence before installing (~/R/packages/), then instlall in the local library
                    if (!dir.exists(Sys.getenv("R_LIBS_USER"))) {
                        dir.create(Sys.getenv("R_LIBS_USER"))
                    }
                    .libPaths(Sys.getenv("R_LIBS_USER"))
                    install.packages(missing_packages, quiet = TRUE, verbose = FALSE, lib = Sys.getenv("R_LIBS_USER"))
                }
                if (print_messages == TRUE) {
                    cat("\nAll the required packages have been installed\n")
                }
                all_needed_packages_are_installed <<- TRUE
            } else {
                ### If there is NO internet...
                if (print_messages == TRUE) {
                    cat("\nSome packages cannot be installed due to internet connection problems\n")
                }
                all_needed_packages_are_installed <<- FALSE
            }
        } else {
            if (print_messages == TRUE) {
                cat("\nAll the required packages are installed\n")
            }
            all_needed_packages_are_installed <<- TRUE
        }
        ##### Load the packages (if there are all the packages)
        if ((length(missing_packages) > 0 && there_is_internet == TRUE) || length(missing_packages) == 0) {
            for (i in 1:length(required_packages)) {
                library(required_packages[i], character.only = TRUE, verbose = FALSE, quietly = TRUE, warn.conflicts = FALSE)
            }
            all_needed_packages_are_installed <<- TRUE
        } else {
            if (print_messages == TRUE) {
                cat("\nPackages cannot be installed/loaded... Expect issues...\n")
            }
            all_needed_packages_are_installed <<- FALSE
        }
    }
    
    ########## INSTALL AND LOAD THE REQUIRED PACKAGES
    install_and_load_required_packages(c("rattle", "phia", "MASS", "ggplot2", "lawstat", "coin", "multcomp", "agricolae", "tcltk", "Hmisc", "parallel"), update_packages = TRUE, print_messages = TRUE) # "Rcmdr", "RcmdrPlugin.coin"
    # The package lawstat is used for levene test for non parametric anova
    
    
    
    
    
    
    
    
    ###################################### Initialize the variables (default values)
    input_file <- ""
    output_folder <- getwd()
    output_format <- "Comma Separated Values (.csv)"
    file_format <- "csv"
    image_format <- ".png"
    pvalue_expression <- 0.05
    pvalue_tests <- 0.05
    TestPer_Base <- 0.17    #fair for class
    TestPer_Adv <- 0.19        #fair for class
    minimum_number_of_patients <- 3
    ### Data Management
    data_record <- TRUE
    ### Correlation Analysis
    correlation_analysis <- FALSE
    # Outlier estimation and removal
    remove_outliers_correlation_analysis <- FALSE
    ### Effect Analysis (2-Level Effects)
    # One class vs all the others (if more classes are present); one class vs the other class (if only two classes are present)
    two_level_effect_analysis  <- TRUE
    # The sampling = TRUE performs the sampling of around 20% of patients/ctrls to be used in R... The other 80% is exported for RapidMiner analysis (with the filtered signals)
    sampling <- FALSE
    # Outlier estimation in the 2-level effect analysis
    remove_outliers_two_level_effect_analysis <- FALSE
    # Cumulative class in two level effect
    cumulative_class_in_two_level_effect_analysis <- FALSE
    ### Effect Analysis (Multi-Level Effects) Stima pT (pT4 e pT5 sono uniti) e Grade
    multi_level_effect_analysis <- TRUE
    remove_outliers_multi_level_effect_analysis <- FALSE
    # Age binning
    age_binning <- FALSE
    age_bins <- 6
    # Plot correlation graphs
    plot_correlation_graphs <- FALSE
    transform_data <- FALSE
    correlation_analysis_method <- "spearman"
    allow_parallelization <- FALSE
    
    
    
    
    
    
    ################## Values of the variables (for displaying and dumping purposes)
    output_file_type_export_value <- "Comma Separated Values (.csv)"
    image_file_type_export_value <- "PNG (.png)"
    data_record_value <- "YES"
    correlation_analysis_value <- "NO"
    remove_outliers_correlation_analysis_value <- "NO"
    two_level_effect_analysis_value <- "YES"
    remove_outliers_two_level_effect_analysis_value <- "NO"
    multi_level_effect_analysis_value <- "YES"
    remove_outliers_multi_level_effect_analysis_value <- "NO"
    cumulative_class_in_two_level_effect_analysis_value <- "NO"
    plot_correlation_graphs_value <- "NO"
    transform_data_value <- "NO"
    correlation_analysis_method_value <- "Spearman"
    allow_parallelization_value <- "NO"
    check_for_updates_value <- R_script_version
    
    
    
    
    
    
    
    ##################################################### DEFINE WHAT THE BUTTONS DO
    
    ##### Check for updates (from my GitHub page) (it just updates the label telling the user if there are updates) (it updates the check for updates value that is called by the label). The function will read also if an update should be forced.
    check_for_updates_function <- function() {
        ### Initialize the version number
        online_version_number <- NULL
        ### Initialize the force update
        online_force_update <- FALSE
        ### Initialize the variable that says if there are updates
        update_available <- FALSE
        ### Initialize the change log
        online_change_log <- "Bug fixes"
        # Check if there is internet connection by pinging a website
        there_is_internet <- check_internet_connection(method = "getURL", website_to_ping = "www.google.it")
        # Check for updates only in case of working internet connection
        if (there_is_internet == TRUE) {
            try({
                ### Read the file from the web (first 10 lines)
                online_file <- readLines(con = github_R_url)
                ### Retrieve the version number
                for (l in online_file) {
                    if (length(grep("R_script_version <-", l, fixed = TRUE)) > 0) {
                        # Isolate the "variable" value
                        online_version_number <- unlist(strsplit(l, "R_script_version <- ", fixed = TRUE))[2]
                        # Remove the quotes
                        online_version_number <- unlist(strsplit(online_version_number, "\""))[2]
                        break
                    }
                }
                ### Retrieve the force update
                for (l in online_file) {
                    if (length(grep("force_update <-", l, fixed = TRUE)) > 0) {
                        # Isolate the "variable" value
                        online_force_update <- as.logical(unlist(strsplit(l, "force_update <- ", fixed = TRUE))[2])
                        break
                    }
                    if (is.null(online_force_update)) {
                        online_force_update <- FALSE
                    }
                }
                ### Retrieve the change log
                for (l in online_file) {
                    if (length(grep("change_log <-", l, fixed = TRUE)) > 0) {
                        # Isolate the "variable" value
                        online_change_log <- unlist(strsplit(l, "change_log <- ", fixed = TRUE))[2]
                        # Remove the quotes
                        online_change_log_split <- unlist(strsplit(online_change_log, "\""))[2]
                        # Split at the \n
                        online_change_log_split <- unlist(strsplit(online_change_log_split, "\\\\n"))
                        # Put it back to the character
                        online_change_log <- ""
                        for (o in online_change_log_split) {
                            online_change_log <- paste(online_change_log, o, sep = "\n")
                        }
                        break
                    }
                }
                ### Split the version number in YYYY.MM.DD
                online_version_YYYYMMDDVV <- unlist(strsplit(online_version_number, ".", fixed = TRUE))
                ### Compare with the local version
                local_version_YYYYMMDDVV = unlist(strsplit(R_script_version, ".", fixed = TRUE))
                ### Check the versions (from the Year to the Day)
                # Check the year
                if (as.numeric(local_version_YYYYMMDDVV[1]) < as.numeric(online_version_YYYYMMDDVV[1])) {
                    update_available <- TRUE
                }
                # If the year is the same (update is FALSE), check the month
                if (update_available == FALSE) {
                    if ((as.numeric(local_version_YYYYMMDDVV[1]) == as.numeric(online_version_YYYYMMDDVV[1])) && (as.numeric(local_version_YYYYMMDDVV[2]) < as.numeric(online_version_YYYYMMDDVV[2]))) {
                        update_available <- TRUE
                    }
                }
                # If the month and the year are the same (update is FALSE), check the day
                if (update_available == FALSE) {
                    if ((as.numeric(local_version_YYYYMMDDVV[1]) == as.numeric(online_version_YYYYMMDDVV[1])) && (as.numeric(local_version_YYYYMMDDVV[2]) == as.numeric(online_version_YYYYMMDDVV[2])) && (as.numeric(local_version_YYYYMMDDVV[3]) < as.numeric(online_version_YYYYMMDDVV[3]))) {
                        update_available <- TRUE
                    }
                }
                # If the day and the month and the year are the same (update is FALSE), check the daily version
                if (update_available == FALSE) {
                    if ((as.numeric(local_version_YYYYMMDDVV[1]) == as.numeric(online_version_YYYYMMDDVV[1])) && (as.numeric(local_version_YYYYMMDDVV[2]) == as.numeric(online_version_YYYYMMDDVV[2])) && (as.numeric(local_version_YYYYMMDDVV[3]) == as.numeric(online_version_YYYYMMDDVV[3])) && (as.numeric(local_version_YYYYMMDDVV[4]) < as.numeric(online_version_YYYYMMDDVV[4]))) {
                        update_available <- TRUE
                    }
                }
                ### Return messages
                if (is.null(online_version_number)) {
                    # The version number could not be ckecked due to internet problems
                    # Update the label
                    check_for_updates_value <- paste("Version: ", R_script_version, "\nUpdates not checked:\nconnection problems", sep = "")
                } else {
                    if (update_available == TRUE) {
                        # Update the label
                        check_for_updates_value <- paste("Version: ", R_script_version, "\nUpdate available:\n", online_version_number, sep = "")
                    } else {
                        # Update the label
                        check_for_updates_value <- paste("Version: ", R_script_version, "\nNo updates available", sep = "")
                    }
                }
            }, silent = TRUE)
        }
        ### Something went wrong: library not installed, retrieving failed, errors in parsing the version number
        if (is.null(online_version_number)) {
            # Update the label
            check_for_updates_value <- paste("Version: ", R_script_version, "\nUpdates not checked:\nconnection problems", sep = "")
        }
        # Escape the function
        update_available <<- update_available
        online_change_log <<- online_change_log
        check_for_updates_value <<- check_for_updates_value
        online_version_number <<- online_version_number
        online_force_update <<- online_force_update
    }
    
    ##### Download the updated file (from my GitHub page)
    download_updates_function <- function() {
        # Download updates only if there are updates available
        if (update_available == TRUE || online_force_update == TRUE) {
            # Changelog
            tkmessageBox(title = "Changelog", message = paste0("The updated script contains the following changes:\n", online_change_log), icon = "info")
            # Initialize the variable which says if the file has been downloaded successfully
            file_downloaded <- FALSE
            # Choose where to save the updated script
            tkmessageBox(title = "Download folder", message = "Select where to save the updated script file", icon = "info")
            download_folder <- tclvalue(tkchooseDirectory())
            # Download the file only if a download folder is specified, otherwise don't
            if (download_folder != "") {
                # Go to the working directory
                setwd(download_folder)
                tkmessageBox(message = paste0("The updated script file will be downloaded in:\n\n", download_folder))
                # Download the file
                try({
                    download.file(url = github_R_url, destfile = paste0(script_file_name, ".R"), method = "auto")
                    file_downloaded <- TRUE
                }, silent = TRUE)
                if (file_downloaded == TRUE) {
                    tkmessageBox(title = "Updated file downloaded!", message = paste0("The updated script, named:\n\n", paste0(script_file_name, ".R"), "\n\nhas been downloaded to:\n\n", download_folder, "\n\nThe current window will now close and the new updated script will be loaded!"), icon = "info")
                    # Destroy the window
                    try(tkdestroy(window), silent = TRUE)
                    # Relaunch the script
                    try(source(paste0(script_file_name, ".R")), silent = TRUE)
                } else {
                    tkmessageBox(title = "Connection problem", message = paste("The updated script file could not be downloaded due to internet connection problems!\n\nManually download the updated script file at:\n\n", github_R_url, sep = ""), icon = "warning")
                }
            } else {
                # No download folder specified!
                tkmessageBox(message = "The updated script file will not be downloaded!")
            }
        } else {
            tkmessageBox(title = "No update available", message = "NO UPDATES AVAILABLE!\n\nThe latest version is running!", icon = "info")
        }
        # Raise the focus on the main window (if there is)
        try(tkraise(window), silent = TRUE)
    }
    
    ### Downloading forced updates
    check_for_updates_function()
    if (online_force_update == TRUE) {
        download_updates_function()
    }
    
    ### Force check for updates
    force_check_for_updates_function <- function() {
        # Check for updates
        check_for_updates_function()
        # Display a message
        if (update_available == TRUE) {
            # Message
            tkmessageBox(title = "Update available", message = paste0("Update available!\n", online_version_number, "\n\nPress the 'DOWNLOAD UPDATE...' button to retrieve the updated script!"), icon = "info")
        } else {
            # Message
            tkmessageBox(title = "No update available", message = "No update available!", icon = "info")
        }
    }
    
    ##### Output file type (export)
    output_file_type_export_choice <- function() {
        # Catch the value from the menu
        output_format <- select.list(c("Comma Separated Values (.csv)", "Microsoft Excel (.xls)", "Microsoft Excel (.xlsx)"), title = "Output file format", preselect = "Comma Separated Values (.csv)")
        # Fix the file format
        if (output_format == "Comma Separated Values (.csv)" || output_format == "") {
            file_format <- "csv"
        } else if (output_format == "Microsoft Excel (.xlsx)") {
            file_format <- "xlsx"
            # Try to install the XLConnect (it will fail if Java is not installed)
            Java_is_installed <- FALSE
            try({
                install_and_load_required_packages("XLConnect")
                Java_is_installed <- TRUE
            }, silent = TRUE)
            # If it didn't install successfully, set to CSV
            if (Java_is_installed == FALSE) {
                tkmessageBox(title = "Java not installed", message = "Java is not installed, therefore the package XLConnect cannot be installed and loaded.\nThe output format is switched back to CSV", icon = "warning")
                file_format <- "csv"
            }
        } else if (output_format == "Microsoft Excel (.xls)") {
            file_format <- "xls"
            # Try to install the XLConnect (it will fail if Java is not installed)
            Java_is_installed <- FALSE
            try({
                install_and_load_required_packages("XLConnect")
                Java_is_installed <- TRUE
            }, silent = TRUE)
            # If it didn't install successfully, set to CSV
            if (Java_is_installed == FALSE) {
                tkmessageBox(title = "Java not installed", message = "Java is not installed, therefore the package XLConnect cannot be installed and loaded.\nThe output format is switched back to CSV", icon = "warning")
                file_format <- "csv"
            }
        }
        # Set the value of the displaying label
        output_file_type_export_value_label <- tklabel(window, text = output_format, font = label_font, bg = "white", width = 30)
        tkgrid(output_file_type_export_value_label, row = 7, column = 3, padx = c(10, 10), pady = c(10, 10))
        # Escape the function
        output_format <<- output_format
        file_format <<- file_format
    }
    
    ##### Image file type (export)
    image_file_type_export_choice <- function() {
        # Catch the value from the menu
        image_output_format <- select.list(c("JPG (.jpg)", "PNG (.png)", "TIFF (.tiff)"), title = "Image format", preselect = "PNG (.png)")
        # Fix the file format
        if (image_output_format == "JPG (.jpg)") {
            image_format <- ".jpg"
        } else if (image_output_format == "PNG (.png)" || image_output_format == "") {
            image_format <- ".png"
        } else if (image_output_format == "TIFF (.tiff)") {
            image_format <- ".tiff"
        }
        # Set the value of the displaying label
        image_file_type_export_value_label <- tklabel(window, text = image_output_format, font = label_font, bg = "white", width = 20)
        tkgrid(image_file_type_export_value_label, row = 7, column = 5, padx = c(10, 10), pady = c(10, 10))
        # Escape the function
        image_output_format <<- image_output_format
        image_format <<- image_format
    }
    
    ##### File import
    file_import_function <- function() {
        filepath_import_select <- tkmessageBox(title = "Input file", message = "Select the file containing all the mass spectrometric information for the statistics", icon = "info")
        input_file <- tclvalue(tkgetOpenFile(filetypes = "{{Comma Separated Value files} {.csv}} {{Microsoft Excel files} {.xls .xlsx}}"))
        if (!nchar(input_file)) {
            tkmessageBox(message = "No file selected")
        } else {
            tkmessageBox(message = paste("The following file will be read:", input_file))
        }
        if (input_file != "") {
            #################### IMPORT THE DATA FROM THE FILE
            #XLSInputDataName <- input_file
            #InputDataName <- sprintf("%s%s",output_folder,XLSInputDataName)
            #excel.InputFile <- file.path(InputDataName)
            ##### XLS or XLSX
            if (length(grep(".xls", input_file, fixed = TRUE)) > 0 || length(grep(".xlsx", input_file, fixed = TRUE)) > 0) {
                input_data <- readWorksheetFromFile(input_file, sheet = 1)
                input_data <- as.data.frame(input_data)
            } else if (length(grep(".csv", input_file, fixed = TRUE)) > 0) {
                ##### CSV
                input_data <- read.csv(input_file, header=TRUE, sep=",")
            }
            ## Data type
            try({input_data$Age <- as.numeric(input_data$Age)}, silent = TRUE)
            try({input_data$pT <- as.numeric(input_data$pT)}, silent = TRUE)
            try({input_data$pT_2009 <- as.factor(input_data$pT_2009)}, silent = TRUE)
            try({input_data$Dim <- as.numeric(input_data$Dim)}, silent = TRUE)
            try({input_data$Grade <- as.numeric(input_data$Grade)}, silent = TRUE)
            try({input_data$Class <- as.factor(input_data$Class)}, silent = TRUE)
            ## Age binning
            try({
                if (isTRUE(age_binning)) {
                    age_bins <- age_bins
                    Age_BINNED <- binning(input_data$Age, age_bins, method = "quantile", ordered = TRUE)
                    levels_age_binned <- levels(Age_BINNED) <- c(1:age_bins)
                    # Replace the Age with the Age_BINNED
                    input_data$Age <- cbind(Age_BINNED)
                }
            }, silent = TRUE)
            ##### Separate the mass spectrometric data from the demographic data
            # All features
            feature_vector <- colnames(input_data)
            # Non signals (columns)
            tkmessageBox(title = "Demographical data", message = "Select the demographic data (to be separated from the mass spectrometric data)", icon = "info")
            non_signals <- select.list(feature_vector, title = "Demographical features", multiple = TRUE)
            ##### Isolate signal from non-signal data
            non_signals_data <- input_data[,(names(input_data) %in% non_signals)]
            signals_data <- input_data[,!(names(input_data) %in% non_signals)]
            number_of_signals <- ncol(signals_data)
            ### Matrix of signal names
            signal_name <- data.matrix(colnames(signals_data))
            colnames(signal_name) <- "Signal Name"
            ##### Determine the features of interest
            # Discriminant column
            tkmessageBox(title = "Discriminant feature", message = "Select the discriminant feature", icon = "info")
            discriminant_feature <- select.list(c(non_signals, "NONE"), title = "Discriminant feature", preselect = "Class")
            # Discriminant column
            tkmessageBox(title = "Patient ID", message = "Select the attribute for the patient ID", icon = "info")
            patient_id_attribute <- select.list(c(non_signals, "NONE"), title = "Patient ID attribute", preselect = "No")
            # Features for correlation analysis
            if (correlation_analysis == TRUE) {
                tkmessageBox(title = "Correlation analysis data", message = "Select the demographic data for correlation analysis", icon = "info")
                non_signals_for_correlation_analysis <- select.list(non_signals[non_signals != discriminant_feature], title = "Features for correlation analysis", multiple = TRUE)
            } else {
                non_signals_for_correlation_analysis <- character()
            }
            # Features for two-level effect analysis
            if (two_level_effect_analysis == TRUE) {
                tkmessageBox(title = "Two-level effect analysis data", message = "Select the demographic data for two-level effect analysis", icon = "info")
                two_level_effect_analysis_non_features <- select.list(non_signals[non_signals != discriminant_feature], title = "Features for two-level effect analysis", multiple = TRUE)
            } else {
                two_level_effect_analysis_non_features <- character()
            }
            if (multi_level_effect_analysis == TRUE) {
                # Features for multi-level effect analysis
                tkmessageBox(title = "Multi-level effect analysis data", message = "Select the demographic data for multi-level effect analysis", icon = "info")
                multi_level_effect_analysis_non_features <- select.list(non_signals[non_signals != discriminant_feature], title = "Features for multi-level effect analysis", multiple = TRUE)
            } else {
                multi_level_effect_analysis_non_features <- character()
            }
            ## Class list
            if (discriminant_feature == "NONE" || discriminant_feature == "") {
                class_list <- discriminant_feature
            } else {
                class_list <- levels(as.factor(input_data[, discriminant_feature]))
            }
            ## Patient ID as rownames
            if (patient_id_attribute == "NONE" || patient_id_attribute == "") {
                rownames(input_data) <- seq(1, nrow(input_data), by = 1)
            } else {
                rownames(input_data) <- input_data[, patient_id_attribute]
            }
            ### Retrieve the input file name
            input_filename <- NULL
            try({
                if (Sys.info()[1] == "Linux" || Sys.info()[1] == "Darwin") {
                    input_filename <- unlist(strsplit(input_file, "/"))
                    input_filename <- input_filename[length(input_filename)]
                    input_filename <- unlist(strsplit(input_filename, ".", fixed = TRUE))[1]
                } else if (Sys.info()[1] == "Windows") {
                    input_filename <- unlist(strsplit(input_file, "\\\\"))
                    input_filename <- input_filename[length(input_filename)]
                    input_filename <- unlist(strsplit(input_filename, ".", fixed = TRUE))[1]
                }
            }, silent = TRUE)
            # Escape the function
            input_file <<- input_file
            input_filename <<- input_filename
            input_data <<- input_data
            feature_vector <<- feature_vector
            non_signals <<- non_signals
            non_signals_data <<- non_signals_data
            number_of_signals <<- number_of_signals
            signals_data <<- signals_data
            signal_name <<- signal_name
            discriminant_feature <<- discriminant_feature
            patient_id_attribute <<- patient_id_attribute
            non_signals_for_correlation_analysis <<- non_signals_for_correlation_analysis
            two_level_effect_analysis_non_features <<- two_level_effect_analysis_non_features
            multi_level_effect_analysis_non_features <<- multi_level_effect_analysis_non_features
            class_list <<- class_list
            tkmessageBox(title = "File imported", message = "The data has been successfully imported from the file!", icon = "info")
        } else {
            # Escape the function
            input_file <<- input_file
            tkmessageBox(title = "No input file selected", message = "No input file has been selected!!!\nPlease, select a file to be imported", icon = "warning")
        }
    }
    
    ##### Output
    browse_output_function <- function() {
        output_folder <- tclvalue(tkchooseDirectory())
        if (!nchar(output_folder)) {
            # Get the output folder from the default working directory
            output_folder <- getwd()
        }
        # Go to the working directory
        setwd(output_folder)
        tkmessageBox(message = paste("Every file will be saved in", output_folder))
        tkmessageBox(message = "A sub-directory named 'STATISTICS X' will be created for each run!")
        # Escape the function
        output_folder <<- output_folder
    }
    
    ##### Exit
    end_session_function <- function() {
        q(save = "no")
    }
    
    ##### Multicore processing
    allow_parallelization_choice <- function() {
        ##### Messagebox
        tkmessageBox(title = "Parallel processing is resource hungry", message = "Parallel processing is resource hungry.\nBy activating it, the computation becomes faster, but the program will eat a lot of RAM, possibly causing your computer to freeze. If you want to play safe, do not enable it.", icon = "warning")
        # Catch the value from the menu
        allow_parallelization <- select.list(c("YES","NO"), title = "Choose", multiple = FALSE, preselect = "NO")
        # Default
        if (allow_parallelization == "YES") {
            allow_parallelization <- TRUE
        }
        if (allow_parallelization == "NO" || allow_parallelization == "") {
            allow_parallelization <- FALSE
        }
        # Set the value of the displaying label
        if (allow_parallelization == TRUE) {
            allow_parallelization_value <- "YES"
        } else {
            allow_parallelization_value <- "NO"
        }
        allow_parallelization_value_label <- tklabel(window, text = allow_parallelization_value, font = label_font, bg = "white", width = 20)
        tkgrid(allow_parallelization_value_label, row = 9, column = 5)
        # Escape the function
        allow_parallelization <<- allow_parallelization
        allow_parallelization_value <<- allow_parallelization_value
    }
    
    ##### Data Record
    data_record_choice <- function() {
        # Catch the value from the menu
        data_record <- select.list(c("YES","NO"), title = "Data record", preselect = "YES")
        # Default
        if (data_record == "YES" || data_record == "") {
            data_record <- TRUE
        }
        if (data_record == "NO") {
            data_record <- FALSE
        }
        # Set the value of the displaying label
        if (data_record == TRUE) {
            data_record_value <- "YES"
        } else {
            data_record_value <- "NO"
        }
        data_record_value_label <- tklabel(window, text = data_record_value, font = label_font, bg = "white", width = 20)
        tkgrid(data_record_value_label, row = 5, column = 5, padx = c(10, 10), pady = c(10, 10))
        # Escape the function
        data_record <<- data_record
        data_record_value <<- data_record_value
    }
    
    ##### Correlation analysis
    correlation_analysis_choice <- function() {
        # Catch the value from the menu
        correlation_analysis <- select.list(c("YES","NO"), title = "Correlation analysis", preselect = "NO")
        # Default
        if (correlation_analysis == "YES") {
            correlation_analysis <- TRUE
            # Select the method
            correlation_analysis_method <- select.list(c("Pearson", "Spearman"), title = "Correlation analysis method", preselect = "Spearman")
            # Fix the values
            if (correlation_analysis_method == "") {
                correlation_analysis_method <- "spearman"
                correlation_analysis_method_value <- "Spearman"
            } else if (correlation_analysis_method == "Pearson") {
                correlation_analysis_method <- "pearson"
                correlation_analysis_method_value <- "Pearson"
            } else if (correlation_analysis_method == "Spearman") {
                correlation_analysis_method <- "spearman"
                correlation_analysis_method_value <- "Spearman"
            }
        }
        if (correlation_analysis == "NO" || correlation_analysis == "") {
            correlation_analysis <- FALSE
            correlation_analysis_method_value <- ""
        }
        # Set the value of the displaying label
        if (correlation_analysis == TRUE) {
            correlation_analysis_value <- paste("YES\n( ", correlation_analysis_method_value, " )", sep = "")
        } else {
            correlation_analysis_value <- "NO"
        }
        correlation_analysis_value_label <- tklabel(window, text = correlation_analysis_value, font = label_font, bg = "white", width = 20, height = 2)
        tkgrid(correlation_analysis_value_label, row = 2, column = 2, padx = c(10, 10), pady = c(10, 10))
        # Escape the function
        correlation_analysis <<- correlation_analysis
        correlation_analysis_method <<- correlation_analysis_method
        correlation_analysis_value <<- correlation_analysis_value
        correlation_analysis_method_value <<- correlation_analysis_method_value
    }
    
    ##### Remove outliers correlation analysis
    remove_outliers_correlation_analysis_choice <- function() {
        # Catch the value from the menu
        remove_outliers_correlation_analysis <- select.list(c("YES","NO"), title = "Outlier removal: correlation analysis", preselect = "NO")
        # Default
        if (remove_outliers_correlation_analysis == "YES") {
            remove_outliers_correlation_analysis <- TRUE
        }
        if (remove_outliers_correlation_analysis == "NO" || remove_outliers_correlation_analysis == "") {
            remove_outliers_correlation_analysis <- FALSE
        }
        # Set the value of the displaying label
        if (remove_outliers_correlation_analysis == TRUE) {
            remove_outliers_correlation_analysis_value <- "YES"
        } else {
            remove_outliers_correlation_analysis_value <- "NO"
        }
        remove_outliers_correlation_analysis_value_label <- tklabel(window, text = remove_outliers_correlation_analysis_value, font = label_font, bg = "white", width = 20)
        tkgrid(remove_outliers_correlation_analysis_value_label, row = 2, column = 4, padx = c(10, 10), pady = c(10, 10))
        # Escape the function
        remove_outliers_correlation_analysis <<- remove_outliers_correlation_analysis
        remove_outliers_correlation_analysis_value <<- remove_outliers_correlation_analysis_value
    }
    
    ##### Plot Correlation graphs
    plot_correlation_graphs_choice <- function() {
        #### Messagebox
        tkmessageBox(title = "Plot correlation graphs is resource hungry", message = "The activity of computing, generating and saving correlation plots is resource hungry. By activating it, the overall computation speed becomes slower.", icon = "warning")
        # Catch the value from the menu
        plot_correlation_graphs <- select.list(c("YES","NO"), title = "Plot correlation graphs", preselect = "NO")
        # Default
        if (plot_correlation_graphs == "YES") {
            plot_correlation_graphs <- TRUE
        }
        if (plot_correlation_graphs == "NO" || plot_correlation_graphs == "") {
            plot_correlation_graphs <- FALSE
        }
        # Set the value of the displaying label
        if (plot_correlation_graphs == TRUE) {
            plot_correlation_graphs_value <- "YES"
        } else {
            plot_correlation_graphs_value <- "NO"
        }
        plot_correlation_graphs_value_label <- tklabel(window, text = plot_correlation_graphs_value, font = label_font, bg = "white", width = 20)
        tkgrid(plot_correlation_graphs_value_label, row = 2, column = 6, padx = c(10, 10), pady = c(10, 10))
        # Escape the function
        plot_correlation_graphs <<- plot_correlation_graphs
        plot_correlation_graphs_value <<- plot_correlation_graphs_value
    }
    
    ##### Two-level effect analysis
    two_level_effect_analysis_choice <- function() {
        # Catch the value from the menu
        two_level_effect_analysis <- select.list(c("YES","NO"), title = "Two-level effect analysis", preselect = "YES")
        # Default
        if (two_level_effect_analysis == "YES" || two_level_effect_analysis == "") {
            two_level_effect_analysis <- TRUE
        }
        if (two_level_effect_analysis == "NO") {
            two_level_effect_analysis <- FALSE
        }
        # Set the value of the displaying label
        if (two_level_effect_analysis == TRUE) {
            two_level_effect_analysis_value <- "YES"
        } else {
            two_level_effect_analysis_value <- "NO"
        }
        two_level_effect_analysis_value_label <- tklabel(window, text = two_level_effect_analysis_value, font = label_font, bg = "white", width = 20)
        tkgrid(two_level_effect_analysis_value_label, row = 3, column = 2, padx = c(10, 10), pady = c(10, 10))
        # Escape the function
        two_level_effect_analysis <<- two_level_effect_analysis
        two_level_effect_analysis_value <<- two_level_effect_analysis_value
    }
    
    ##### Cumulative class in Two-level effect analysis
    cumulative_class_in_two_level_effect_analysis_choice <- function() {
        # Catch the value from the menu
        cumulative_class_in_two_level_effect_analysis <- select.list(c("YES","NO"), title = "Cumulative class", preselect = "YES")
        # Default
        if (cumulative_class_in_two_level_effect_analysis == "YES" || cumulative_class_in_two_level_effect_analysis == "") {
            cumulative_class_in_two_level_effect_analysis <- TRUE
        }
        if (cumulative_class_in_two_level_effect_analysis == "NO") {
            cumulative_class_in_two_level_effect_analysis <- FALSE
        }
        # Set the value of the displaying label
        if (cumulative_class_in_two_level_effect_analysis == TRUE) {
            cumulative_class_in_two_level_effect_analysis_value <- "YES"
        } else {
            cumulative_class_in_two_level_effect_analysis_value <- "NO"
        }
        cumulative_class_in_two_level_effect_analysis_value_label <- tklabel(window, text = cumulative_class_in_two_level_effect_analysis_value, font = label_font, bg = "white", width = 20)
        tkgrid(cumulative_class_in_two_level_effect_analysis_value_label, row = 3, column = 6, padx = c(10, 10), pady = c(10, 10))
        # Escape the function
        cumulative_class_in_two_level_effect_analysis <<- cumulative_class_in_two_level_effect_analysis
        cumulative_class_in_two_level_effect_analysis_value <<- cumulative_class_in_two_level_effect_analysis_value
    }
    
    ##### Multi-level effect analysis
    multi_level_effect_analysis_choice <- function() {
        # Catch the value from the menu
        multi_level_effect_analysis <- select.list(c("YES","NO"), title = "Multi-level effect analysis", preselect = "YES")
        # Default
        if (multi_level_effect_analysis == "YES" || multi_level_effect_analysis == "") {
            multi_level_effect_analysis <- TRUE
        }
        if (multi_level_effect_analysis == "NO") {
            multi_level_effect_analysis <- FALSE
        }
        # Set the value of the displaying label
        if (multi_level_effect_analysis == TRUE) {
            multi_level_effect_analysis_value <- "YES"
        } else {
            multi_level_effect_analysis_value <- "NO"
        }
        multi_level_effect_analysis_value_label <- tklabel(window, text = multi_level_effect_analysis_value, font = label_font, bg = "white", width = 20)
        tkgrid(multi_level_effect_analysis_value_label, row = 4, column = 3, padx = c(10, 10), pady = c(10, 10))
        # Escape the function
        multi_level_effect_analysis <<- multi_level_effect_analysis
        multi_level_effect_analysis_value <<- multi_level_effect_analysis_value
    }
    
    ##### Remove outliers two-level effect analysis
    remove_outliers_two_level_effect_analysis_choice <- function() {
        # Catch the value from the menu
        remove_outliers_two_level_effect_analysis <- select.list(c("YES","NO"), title = "Outlier removal: two-level effect analysis", preselect = "NO")
        # Default
        if (remove_outliers_two_level_effect_analysis == "YES") {
            remove_outliers_two_level_effect_analysis <- TRUE
        }
        if (remove_outliers_two_level_effect_analysis == "NO" || remove_outliers_two_level_effect_analysis == "") {
            remove_outliers_two_level_effect_analysis <- FALSE
        }
        # Set the value of the displaying label
        if (remove_outliers_two_level_effect_analysis == TRUE) {
            remove_outliers_two_level_effect_analysis_value <- "YES"
        } else {
            remove_outliers_two_level_effect_analysis_value <- "NO"
        }
        remove_outliers_two_level_effect_analysis_value_label <- tklabel(window, text = remove_outliers_two_level_effect_analysis_value, font = label_font, bg = "white", width = 20)
        tkgrid(remove_outliers_two_level_effect_analysis_value_label, row = 3, column = 4, padx = c(10, 10), pady = c(10, 10))
        # Escape the function
        remove_outliers_two_level_effect_analysis <<- remove_outliers_two_level_effect_analysis
        remove_outliers_two_level_effect_analysis_value <<- remove_outliers_two_level_effect_analysis_value
    }
    
    ##### Remove outliers multi-level effect analysis
    remove_outliers_multi_level_effect_analysis_choice <- function() {
        # Catch the value from the menu
        remove_outliers_multi_level_effect_analysis <- select.list(c("YES","NO"), title = "Outlier removal: multi-level effect analysis", preselect = "NO")
        # Default
        if (remove_outliers_multi_level_effect_analysis == "YES") {
            remove_outliers_multi_level_effect_analysis <- TRUE
        }
        if (remove_outliers_multi_level_effect_analysis == "NO" || remove_outliers_multi_level_effect_analysis == "") {
            remove_outliers_multi_level_effect_analysis <- FALSE
        }
        # Set the value of the displaying label
        if (remove_outliers_multi_level_effect_analysis == TRUE) {
            remove_outliers_multi_level_effect_analysis_value <- "YES"
        } else {
            remove_outliers_multi_level_effect_analysis_value <- "NO"
        }
        remove_outliers_multi_level_effect_analysis_value_label <- tklabel(window, text = remove_outliers_multi_level_effect_analysis_value, font = label_font, bg = "white", width = 20)
        tkgrid(remove_outliers_multi_level_effect_analysis_value_label, row = 4, column = 5, padx = c(10, 10), pady = c(10, 10))
        # Escape the function
        remove_outliers_multi_level_effect_analysis <<- remove_outliers_multi_level_effect_analysis
        remove_outliers_multi_level_effect_analysis_value <<- remove_outliers_multi_level_effect_analysis_value
    }
    
    ##### Data transformation
    transform_data_choice <- function() {
        # Catch the value from the menu
        transform_data <- select.list(c("YES", "NO"), title = "Data transformation", preselect = "NO")
        # Default
        if (transform_data == "YES") {
            transform_data <- TRUE
            # Select the algorithm
            transform_data_algorithm <- select.list(c("Square root", "Natural logarithm", "Decimal logarithm", "Binary logarithm", "Absolute value", "Sine", "Cosine", "Exponential"), title = "Data transformation method", preselect = "Square root")
            # Fix the values
            if (transform_data_algorithm == "") {
                transform_data_algorithm <- "Square root"
            }
        } else if (transform_data == "NO" || transform_data == "") {
            transform_data <- FALSE
        }
        # Set the value of the displaying label
        if (transform_data == TRUE) {
            transform_data_value <- paste("YES\n( ", transform_data_algorithm, " )", sep = "")
        } else {
            transform_data_value <- "NO"
        }
        transform_data_value_label <- tklabel(window, text = transform_data_value, font = label_font, bg = "white", width = 20, height = 2)
        tkgrid(transform_data_value_label, row = 5, column = 3, padx = c(10, 10), pady = c(10, 10))
        # Escape the function
        transform_data <<- transform_data
        transform_data_algorithm <<- transform_data_algorithm
        transform_data_value <<- transform_data_value
    }
    
    ##### Run the statistics
    run_statistics_function <- function() {
        ### Progress bar
        program_progress_bar <- tkProgressBar(title = "Calculating...", label = "", min = 0, max = 1, initial = 0, width = 300)
        setTkProgressBar(program_progress_bar, value = 0, title = NULL, label = "0 %")
        # Go to the working directory
        setwd(output_folder)
        if (input_file != "") {
            ##### Automatically create a subfolder with all the results
            # Add the date and time to the filename
            current_date <- unlist(strsplit(as.character(Sys.time()), " "))[1]
            current_date_split <- unlist(strsplit(current_date, "-"))
            current_time <- unlist(strsplit(as.character(Sys.time()), " "))[2]
            current_time_split <- unlist(strsplit(current_time, ":"))
            final_date <- ""
            for (x in 1:length(current_date_split)) {
                final_date <- paste0(final_date, current_date_split[x])
            }
            final_time <- ""
            for (x in 1:length(current_time_split)) {
                final_time <- paste0(final_time, current_time_split[x])
            }
            final_date_time <- paste(final_date, final_time, sep = "_")
            STATISTICS_subfolder <- paste0("STATISTICS", " (", final_date_time, ")")
            # Generate the new subfolder
            output_folder <- file.path(output_folder, STATISTICS_subfolder)
            # Create the subfolder
            dir.create(output_folder)
            # Go to the new working directory
            setwd(output_folder)
            
            # Progress bar
            setTkProgressBar(program_progress_bar, value = 0.05, title = NULL, label = "5 %")
            
            ########################################################################
            
            #################### Retrieve the values from the entries
            pvalue_expression <- tclvalue(pvalue_expression)
            pvalue_expression <- as.numeric(pvalue_expression)
            pvalue_expression_value <- as.character(pvalue_expression)
            pvalue_tests <- tclvalue(pvalue_tests)
            pvalue_tests <- as.numeric(pvalue_tests)
            pvalue_tests_value <- as.character(pvalue_tests)
            minimum_number_of_patients <- tclvalue(minimum_number_of_patients)
            minimum_number_of_patients <- as.integer(minimum_number_of_patients)
            minimum_number_of_patients_value <- as.character(minimum_number_of_patients)
            
            ########################################################################
            
            #################### FUNCTIONS
            ## P-value extractor functions
            assumption_p <- function(objt) {
                ifelse(is.object(objt), return(objt$p.value), return(NA))
            }
            assumption_anova <- function(objt) {
                ifelse(is.object(objt), return(objt$Pr[1]), return(NA))
            }
            assumption_permutation <- function(objt) {
                ifelse(is.object(objt), return(pvalue(objt)[1]), return(NA))
            }
            
            ## Post hoc tests
            write_posthoc_file <- function(file_name, data, file_format = "xlsx"){
                # Store the original filename
                original_file_name <- file_name
                # The sheet name must not contain more than 31 characters
                length_sheet_name <- length(unlist(strsplit(original_file_name, "")))
                # Fix the sheet name if it is too long...
                if (length_sheet_name > 31) {
                    sheet_name <- unlist(strsplit(original_file_name, ""))[1:31]
                    final_sheet_name <- ""
                    for (ch in 1:length(sheet_name)) {
                        final_sheet_name <- paste(final_sheet_name, sheet_name[ch], sep = "")
                    }
                } else {
                    final_sheet_name <- original_file_name
                }
                ### Excel
                if (file_format == "xls" || file_format == "xlsx" || file_format == "csv") {
                    # Fix the extension
                    file_name <- paste(file_name, ".xlsx", sep = "")
                    install_and_load_required_packages("XLConnect")
                    wb = loadWorkbook(file = file_name, create = TRUE)
                    # Create a new sheet
                    createSheet(wb, name = final_sheet_name)
                    # cumulative length (rows) of matrices
                    # +2 = 1 for list names, 1 for header row
                    cumlen = cumsum(c(1, head(sapply(data, nrow), n = - 1) + 2))
                    # Write data rows (implicitly vectorized!)
                    writeWorksheet(wb, data = data, sheet = final_sheet_name, startRow = cumlen + 1, header = TRUE)
                    # Write list names
                    writeWorksheet(wb, data = as.list(names(data)), sheet = final_sheet_name, startRow = cumlen, header = FALSE)
                    saveWorkbook(wb)
                } else if (file_format == "csv") {
                    # Fix the extension
                    file_name <- paste(file_name, ".", file_format, sep = "")
                    output_matrix <- matrix(nrow = 1, ncol = length(data))
                    # Fill the matrix
                    for (l in 1:length(data)) {
                        output_matrix[1, l] <- data[[l]]
                    }
                    colnames(output_matrix) <- names(sapply(data, nrow))
                    write.csv(output_matrix, file = file_name)
                }
            }
            
            ##### Split the dependent variable according to the factor variable
            group_dependent_variable <- function(dependent_variable, factor_variable) {
                # Extract the levels of the factor variable
                factor_levels <- levels(factor_variable)
                # Determine the number of levels
                number_of_levels <- length(factor_levels)
                # Generate a list in which each element is relative to the observations of the dependent variable with a certain value of ther factor variable
                dependent_variable_split <- list()
                # For each level of the factor variable...
                for (i in 1:number_of_levels){
                    # Extract the dependent variable observations with that factor variable value
                    dependent_variable_split[[i]] <- dependent_variable[factor_variable == factor_levels[i]]
                }
                # Return
                return(dependent_variable_split)
            }
            
            ### Function to export the data files
            write_file <- function(file_name, data, file_format = "xlsx"){
                # Store the original filename
                original_file_name <- file_name
                # The sheet name must not contain more than 31 characters
                length_sheet_name <- length(unlist(strsplit(original_file_name, "")))
                # Fix the sheet name if it is too long...
                if (length_sheet_name > 31) {
                    sheet_name <- unlist(strsplit(original_file_name, ""))[1:31]
                    final_sheet_name <- ""
                    for (ch in 1:length(sheet_name)) {
                        final_sheet_name <- paste(final_sheet_name, sheet_name[ch], sep = "")
                    }
                } else {
                    final_sheet_name <- original_file_name
                }
                # Fix the extension
                file_name <- paste(file_name, ".", file_format, sep = "")
                if (file_format == "xls" || file_format == "xlsx") {
                    wb <- loadWorkbook(file_name, create = TRUE)
                    # Create a new sheet
                    createSheet(wb, name = final_sheet_name)
                    # Write data rows (implicitly vectorized!)
                    writeWorksheet(wb, data = data, sheet = final_sheet_name, header = TRUE)
                    saveWorkbook(wb)
                } else if (file_format == "csv") {
                    write.csv(data, file = file_name)
                }
            }
            
            ## Function for correlation analysis: x corresponds to each element of the list (a data frame with the signal column and the non-signal column, with patient IDs as rownames)
            correlation_analysis_subfunction <- function(x, correlation_method, plot_correlation, remove_outliers_correlation, plot_format, correlation_plots_subfolder, correlation_plots_subfolder_no_outliers) {
                # Initialize outputs
                outlier_list <- list()
                correlation_list <- list()
                # Isolate the column corresponding to the mass (as a vector)
                mass_x <- x[, 1]
                non_signal_column_original <- x[, 2]
                patient_id <- rownames(x)
                # Remove NAs from the non signal and from the signal
                mass_x <- mass_x[!is.na(non_signal_column_original)]
                non_signal_column <- non_signal_column_original[!is.na(non_signal_column_original)]
                patient_id <- patient_id[!is.na(non_signal_column_original)]
                ### Generate a graph before the removal of outliers
                if (plot_correlation == TRUE) {
                    plot_name <- sprintf("%s%s%s%s", "Correlation ", names(x)[1], " vs ", names(x)[2])
                    file_name <- sprintf("%s%s", plot_name, plot_format)
                    scatter_plot <- qplot(mass_x, non_signal_column, geom = "auto", main = plot_name, ylab = names(x)[2], xlab = names(x)[1])
                    setwd(correlation_plots_subfolder)
                    ggsave(scatter_plot, file = file_name , width = 4, height = 4)
                }
                ### Outlier detection
                if (isTRUE(remove_outliers_correlation)) {
                    # Detect the outliers (fence)
                    outliers <- boxplot(mass_x, plot = FALSE)$out
                    # If there are some...
                    if (length(outliers) > 0) {
                        # Build the dataframe to be dumped
                        outliers_dataframe <- as.data.frame(cbind(patient_id[mass_x %in% outliers]))
                        # Signal name
                        sign_name <- rep(names(x)[1], nrow(outliers_dataframe))
                        # Append the two columns...
                        outliers_dataframe <- cbind(outliers_dataframe, sign_name)
                        # Fix the column names
                        colnames(outliers_dataframe) <- c("Patient", "Mass")
                        # Store this in the final list of outlier dataframes
                        outlier_list[[names(x)[1]]] <- outliers_dataframe
                        # Replace the intensity in the original dataframe with NA (so that they are excluded)
                        mass_x[mass_x %in% outliers] <- NA
                    }
                    # Extract the values without the outliers
                    mass_x_no_outliers <- mass_x[!is.na(mass_x)]
                    non_signal_column_no_outliers <- non_signal_column[!is.na(mass_x)]
                    ### Generate a graph after the removal of outliers
                    if (plot_correlation == TRUE) {
                        plot_name <- sprintf("%s%s%s%s", "Correlation ", names(x)[1], " vs ", names(x)[2], " (without outliers)")
                        file_name <- sprintf("%s%s", plot_name, plot_format)
                        scatter_plot <- qplot(mass_x, non_signal_column, geom = "auto", main = sprintf("%s%s%s%s", "Correlation ", names(x)[1], " vs ", names(x)[2], "\n(without outliers)"), ylab = names(x)[2], xlab = names(x)[1])
                        setwd(correlation_plots_subfolder_no_outliers)
                        ggsave(scatter_plot, file = file_name , width = 4, height = 4)
                    }
                }
                ### Compute the results
                correlation_result_vector <- c(cor.test(mass_x, as.numeric(non_signal_column), method = correlation_method)$estimate, cor.test(mass_x, as.numeric(non_signal_column), method = correlation_method)$p.value)
                ### Correlation (signal m with the non-signal ns)
                # Append to the final correlation list
                correlation_list[[names(x)[2]]][[names(x)[1]]] <- correlation_result_vector
                ### Return
                return(list(correlation_sublist = correlation_list, outlier_sublist = outlier_list))
            }
            
            ########################################################################
            
            
            
            
            
            #################### RUN THE STATISTICS
            
            ################ Transform the data (transform directly the input_data and not the signals_data because it is the input_data variable that it is carried throughout the analysis, but transform only the signals data)
            # Store the original before transforming
            original_input_data <- input_data
            if (isTRUE(transform_data)) {
                # Progress bar
                setTkProgressBar(program_progress_bar, value = 0.05, title = NULL, label = "Data transformation")
                if (transform_data_algorithm == "Square root") {
                    input_data[,!(names(input_data) %in% non_signals)] <- sqrt(input_data[,!(names(input_data) %in% non_signals)])
                } else if (transform_data_algorithm == "Natural logarithm") {
                    input_data[,!(names(input_data) %in% non_signals)] <- log(input_data[,!(names(input_data) %in% non_signals)])
                } else if (transform_data_algorithm == "Decimal logarithm") {
                    input_data[,!(names(input_data) %in% non_signals)] <- log10(input_data[,!(names(input_data) %in% non_signals)])
                } else if (transform_data_algorithm == "Binary logarithm") {
                    input_data[,!(names(input_data) %in% non_signals)] <- log2(input_data[,!(names(input_data) %in% non_signals)])
                } else if (transform_data_algorithm == "Absolute value") {
                    input_data[,!(names(input_data) %in% non_signals)] <- abs(input_data[,!(names(input_data) %in% non_signals)])
                } else if (transform_data_algorithm == "Sine") {
                    input_data[,!(names(input_data) %in% non_signals)] <- sin(input_data[,!(names(input_data) %in% non_signals)])
                } else if (transform_data_algorithm == "Cosine") {
                    input_data[,!(names(input_data) %
