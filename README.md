# CPDP-HIEL
This is a GitHub link to access the replication package of the under review work titled **"How Far Does the Predictive Decision Impact the Software Project? The Cost, Service Time, and Failure Analysis from a Cross-Project Defect Prediction Model"** 


Softwares Required
-----------------------------------

1. RStudio-4.1.3 or
2. https://rstudio.cloud/ 
   It is recommended to use the online platform for easy interaction.


Instructions to Replicate the Work
-----------------------------------
Please follow this sequence to replicate the work.
**Loading the Packages:**
1. Open the program /Programs/Libraries.R
2. Select all the code and then click on _run_ to include the mentioned libraries.
3. If the libraries are not present in the system, please use install.libraries("PackageName") to install the required packages. Please select all the dependencies while installing the packages.

**Load the Projects**
1. Open the /Programs/LoadProjects.R
2. Specify the data path to the working directory using setwd('Datapath to individial repositories'). Example: setwd('Specify the local path/Data/PROMISE/')
3. Select all the projects of the respective repository and then click on _run_ to load all the projects information into the data frames.

**Define the functions**
1. Open the /Programs/Functions.R
2. Select all the code and click on _run_ to define the required functions for the experimentation

**CPDP using HIEL**

The following procedure is similar for all the projects in the three repositories
1. Chage the directory to /Programs/PROMISE/ and open any program
2. For each target project, add details to the _targetData_ and then run the entire code to train and classification using probabilistic weighted majority voting approach.
3. For example: for the target project ant-1.3, add the deails of the dataframe _ant1_ to the dataframe _testData_. Select all the code that cover till PWMV approach.
4. Each program contain the code for proposed measures also. Select targeted code for the proposed measure and then _run_.
5. Repeat the procedure for each program.

**Significance Tests**
1. For the significance tests, open /Programs/SignificanceTests.R program
2. Copy the performances observed after running on the target project to the dataframe _HIEL_.
3. Copy the performances of the other models to the dataframe _OtherModel_
4. Clink on _run_ to perform the required significance tests.

**Miscellaneous Results**
Open the CPDP-Review-Results.xlsx.xlsx to access all the results of our work along with the Miscellaneous results.
