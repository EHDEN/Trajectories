# Trajectories R-package

It is a package for detecting and visualizing statistically significant event sequencies in OMOP CDM data.

## Prerequisites

In order to run the package, you need:

1. A database that has data in OMOP CDM v5 format. The database should also contain OMOP vocabulary, but this can be in a separate schema.
2. A database user + passwords that has:
 a. Read (SELECT) permission from OMOP CDM tables and vocabulary
 b. CREATE, DROP, SELECT, INSERT, UPDATE, DELETE permission in some schema of the same database. This is used for creating and temporary analysis tables. For this, you can create a separate schema in the same database.

## Installation

1. Clone this repository
2. Open the project (file ***Trajectories.Rproj***) in RStudio
3. Install the package via top-right menu: ***Build*** -> ***Install and Restart***

## Setup & running

1. Rename ***Renviron.template*** to ***.Renviron***
2. Add your database username and password to ***.Renviron*** (these are the database credentials for accessing OMOP CDM and writing temporary analysis tables/data). After you restart your RStudio, it automatically reads the database credentials from that file so that you do not have to enter them each time. Also, .Renviron is not under version control, therefore it is kept unchanged even if you pull the updates of the R-package.
3. Restart RStudio to automatically read in database credentials.
4. Open ***extras/CodeToRunValidate.R***
5. Check that parameters in the *"# SETTING UP THE PARAMETER VALUES, CONNECT TO DATABASE"* section are correct and run the code.
6. It will produce quite a lot of output to the R console and ultimately create bunch of tables, figures, logs to the folder that is set by variable "mainOutputFolder"

See vignette for more information.

## Contributors

This package is developed by Kadri Künnapuu, Kadri Ligi, Raivo Kolde, Sven Laur, Solomon Ioannou, Peter Rijnbeek, Jaak Vilo and Sulev Reisberg. See details from the corresponding scientific paper. This work was supported by the Estonian Research Council grants (PRG1095, RITA1/02-96-11); the European Union through the European Regional Development Fund grant EU48684; and the European Social Fund via IT Academy programme. The European Health Data & Evidence Network has received funding from the Innovative Medicines Initiative 2 Joint Undertaking (JU) under grant agreement No 806968. The JU receives support from the European Union’s Horizon 2020 research and innovation programme and EFPIA.
