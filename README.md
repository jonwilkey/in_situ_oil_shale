# README #

The purpose of this repository is to:

* Track changes in scripts (primarily for myself).
* Provide a method for sharing the latest version of model code with the group (for everyone).

### How do I get set up? ###

There are a couple of things to do to get setup model:

1. Download and install R (http://www.r-project.org/)
2. (Recommended) Download and install RStudio (a really good IDE for R) (http://www.rstudio.com/products/rstudio/download/)
3. Download and install Git for version control (http://git-scm.com/downloads) - for sharing scripts
3. You'll also need setup an account on bitbucket.org (I think there is a link and instructions in the version control invite message I sent out on June 3, 2014). Of course you probably aren't reading this guide here unless you've already worked that requirement out...
4. Download this shared Dropbox folder (https://www.dropbox.com/sh/u0qreiitqarhjqz/AAB0FEr15rP_NqL_oms0FWZra?dl=0) - for sharing data files
5. Launch RStudio
6. From the menu, select **File > New Project**
7. In the dialogue menu that pops up, select **Version Control**
8. In the next menu select **Git**
9. Paste the repository URL from bitbucket.org into the **Repository URL** box. You can connect using either SSH or HTTPS (select the method you'd prefer to use from the dropdown menu).
10. A folder will be created containing all of the scripts saved on the bitbucket server.
11. The name of that folder will be whatever you enter in the **Project directory name** dialogue box. By default I believe it's the same name as the bitbucket.org project name (oilshale).
12. The folder will be located as a subdirectory of whatever folder you point to in the **Create project as subdirectory of** dialogue box. By default it's a subdirectory of ~/R, which is the R folder generated in your home directory when you first run R (or install any packages in R).
13. After entering all the information, click the **Create Project** button.
14. Open the script **UO_main.R** , located in the directory you created in Step 14. This is the main script for model.
15. Under the section of the scripts labeled **# Paths**, change the paths for each of the directories listed to the location on your computer where you downloaded the Dropbox shared folder contents.

Finally, make sure that you have the libraries necessary to run the script in question. There should be a list of the libraries used under the **# Libraries** section header. You can install libraries using the following command in the R console (window in lower left of RStudio):

install.packages("package_name")

Alternatively, you can use the menu option Tools > Install Packages... and then enter the names of the packages you wish to install.

You can now finally run the script. Do this using the menu option Code > Source or any of its keyboard shortcuts and variations. The script editor (top left window) also contains a drop-down source menu button in the upper-right corner. By default, source runs without printing to the console, however you can also run source with echo and it will print every line in the script as it executes them.

### Questions? ###

* Documentation for Git [here](http://git-scm.com/doc)
* Documentation for Bitbucket [here](https://confluence.atlassian.com/x/bgozDQ)