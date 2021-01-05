# Trajectories R-package

To detect an visualize statistically significant event sequencies in OMOP data.

## Prerequisites

In order to run the package, you need:

1. A database that has data in OMOP CDM v5 format. The database should also contain OMOP vocabulary, but this can be in a separate schema.
2. A database user + passwords that has:
 a. Read (SELECT) permission from OMOP CDM tables and vocabulary
 b. CREATE, DROP, SELECT, INSERT, UPDATE, DELETE permission in some schema in the same database. This is used for creating and population temporary data tables. For this, you can create a separate schema in the same database.
3. Rstudio

## Installation

1. Clone this repository
2. Open the project (file ***Trajectories.Rproj***) in RStudio
3. Install the package via top-right menu: ***Build*** -> ***Install and Restart***

## Setup & running

1. Rename ***Renviron.template*** to ***.Renviron***
2. Add your database username and password to ***.Renviron*** (these are the database credentials for accessing OMOP CDM and writing temporary analysis tables/data). After you restart your RStudio, it automatically reads the database credentials from that file so that you do not have to enter them each time. Also, .Renviron is not under version control, therefore it is kept unchanged even if you pull the updates of the R-package.
3. Restart RStudio to automatically read in database credentials.
4. Open ***extras/CodeToRun.R***
5. Check that parameters up to *"# The following parameters are used in the calculations."* are correct and run the code.
6. It should produce quite a lot of output to the R console and ultimately create bunch of graphs to the folder that is set by variable "mainOutputFolder"

## Contributors

This package is developed by Kadri Künnapuu, Sulev Reisberg, Raivo Kolde (University of Tartu and STACC) with the help of other people. Its development started in IMI EHDEN project. the contributors are not limited to EHDEN consortium members (University of Tartu, STACC).
