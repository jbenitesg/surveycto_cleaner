# Apply the SurveyCTO's forms defintion to your data
Works with the data definitions from [SurveyCTO API in R](https://github.com/agency-fund/rsurveycto)
```r
library('haven')
library('dplyr')
library('rsurveycto')
library('labelled')

# Read the function
source("https://raw.githubusercontent.com/jbenitesg/surveycto_cleaner/main/surveyCTO_cleaner.R")
## Set your key authorization
auth = scto_auth('PATH_TO_AUTH_FILE')
## Get your form definitions (labels)
definitions = scto_get_form_definitions(auth, form_ids = 'YOUR_FORM')
## Read the data
scto_data = scto_read(auth, 'YOUR_FORM')

## Save the choices
choices = definitions[["choices"]]
## Save the choices
fields = definitions[["fields"]]

# Applied the forms definition (type of variable, label of the variable and their values)
newdata = surveyCTO_cleaner(data = data,  fields = fields, choices = choices)

## Check the data with the definitions
labelled::generate_dictionary(newdata)
```
