# eia-oeconomica
The home of all code for the Oeconomica Entrepreneurship, Innovation & Antitrust cohort for 2020–2021.

# Data managment and setting up your R project
All data used in this project will be kept in a shared Google Drive folder (`EIA Cohort (Shared Folder)`). Please make sure you sync this folder to your computer.

If you cannot sync the Google Drive folder, or if you want to explore this project and were not part of the cohort, the data is available for download here: https://drive.google.com/drive/folders/1kZHpzNzSht9ov1M0jJPrRLT_KGFKH7Es?usp=sharing. Adjust the following steps regarding setting up the `data.R` file to reflect the file path to that of the downloaded, unzipped data folder.

First, make sure you click the "Fork" button in the upper right corner of the GitHub page for this repository.

We are now going to do a couple things to set up GitHub with R:
- Open RStudio, and create a new project
  - Select "Version Control" and "Git"
  - Click the green "Code" button in your forked version of this repository on GitHub (it should be called `[your-username]/eia-oeconomica`), and copy and paste the HTTPS URL of the repository into the "Repository URL" space in RStudio 
  - The "Project directory name" should be `eia-oeconomica`
  - Choose to make it a subdirectory of a location you will easily find on your computer (perhaps your desktop or in your Google Drive folder — you can pick anywhere other than our cohort's shared Google Drive folder!)
  - Click "Create Project"
- In your new R project, locate the file called: `data.R`
  - In this file, change the following line of code to reflect the path to the `data` folder on your computer (if you downloaded the data via the link above, then this folder is just the unzipped folder from the download): `ddir <- "/Users/gelkouh/Google Drive (UChicago)/EIA Cohort (Shared Folder)/data"` 
  - (note: you may need to begin the file path in your code with `C:` depending on the operating system your computer uses)
  - Make sure you save the `data.R` file and keep it where it is

The `data.R` file will allow us to write code that is system agnostic: in other words, our code can call a standard file (`data.R`) that we will have on all our machines, and then we will have an object called `ddir` in our R environment which can be used to tell our computer where to look for data. 

(Thank you to https://github.com/ekarsten for the framework for some of this data management section.)

# Updating your fork and other odds and ends
When the code in this repository is changed, you will need to update your fork to reflect these changes. For instructions on how to do this, in addition to other short tutorials on relevant GitHub and coding related tasks, see the "Wiki" tab of this repository (https://github.com/gelkouh/eia-oeconomica/wiki).

The timeline and tasks to do for our project are under the "Projects" tab of this repository (https://github.com/gelkouh/eia-oeconomica/projects).

You can review and explore the fall economic history and theory lectures here: https://drive.google.com/drive/u/1/folders/1tVogZxYUKvoSj0J0IjfZJArn-1SFn_5r
