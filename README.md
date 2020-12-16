![img](https://img.shields.io/badge/Lifecycle-Stable-97ca00)

British Columbia Snow Duration Analysis
=======================================

### Overview

Series of code with the purpose of utilizing the Moderate Resolution Imaging Spectro-Radiometer (MODIS) M*D10A10 datasets for deriving snow duration metrics over BC during the period 2002 - 2018. Additional analysis is carried out to explore the influence of major climate modes including the El Nino Southern Oscillation and the Pacific Decadal Oscillation on inter-annual snow cover variability.  

### Project Status

This project is completed and has been published in The Cryosphere [https://www.the-cryosphere-discuss.net/tc-2019-61/](https://www.the-cryosphere-discuss.net/tc-2019-61/).

### Workflow

We use Google Earth Engine to download the daily MODIS snow cover product, then Python to fit the LOWESS time series interpolation, and then R to analyze the data start, end and duration of snow cover.

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/snow-dur-analysis/issues/) or contact alexandre.bevington@gov.bc.ca. 

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### License

    Copyright 2018 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

------------------------------------------------------------------------

*This project was created using the [bcgovr](https://github.com/bcgov/bcgovr) package.*
