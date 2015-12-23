#!/bin/bash

#
# Running the scraper.
#
Rscript app/collector.R

#
# Registering the datasets on HDX.
#
source venv/bin/activate
python app/hdx_register/
