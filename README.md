# README #

### Setup Instructions ###

1. Download and install R (http://www.r-project.org/)
2. (Recommended) Download and install RStudio (an IDE for R) (http://www.rstudio.com/products/rstudio/download/)
3. Download and install Git for version control (http://git-scm.com/downloads)3.
4. Launch RStudio
5. Install the following packages using the `install.packages("packge_name")` command in R. Alternatively, if you are using RStudio you can use the menu option **Tools > Install Packages...** and then enter the names of the packages you wish to install.
  1. zoo
  2. sqldf
  3. lhs
  4. beepr
  5. ggplot2
  6. scales
  7. hexbin
6. From the menu, select **File > New Project**
7. In the dialogue menu that pops up, select **Version Control**
8. In the next menu select **Git**
9. Paste the repository URL from GitHub into the **Repository URL** box.
10. A folder will be created containing all of the scripts and functions in the GitHub repository.
  1. The name of that folder will be whatever you enter in the **Project directory name** dialogue box. By default this will be the same as the name of the respository (oilshale).
  2. The folder will be located as a subdirectory of whatever folder you point to in the **Create project as subdirectory of** dialogue box. By default it's a subdirectory of ~/R, which is the R folder generated in your home directory when you first install a package in R.
13. After entering all the information, click the **Create Project** button.
14. Download the CFD simulation data (~100 MB) and place it in the **Prepared Data** folder: https://www.dropbox.com/s/hy32gv3aqeb8uvp/dataImport.rda?dl=0
15. Open the script **UO_main.R** , located in the directory you created in Step 10. This is the main script for model.
16. Under the section of the scripts labeled **# 1.1 Paths**, change the paths for each of the directories listed to the location on your computer the repository was downloaded.

You can now run the script. If you are using RStudio, this can be done using the menu option **Code > Source** or any of its keyboard shortcuts and variations. All of the model input options are contained in the script **UO_options.R**.

### Questions? ###

* Contact Jon Wilkey (jon.wilkey@gmail.com)