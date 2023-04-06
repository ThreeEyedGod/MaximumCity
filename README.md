# MaximumCity 
[![GitHub CI](https://github.com/ThreeEyedGod/MaximumCity/workflows/CI/badge.svg)](https://github.com/ThreeEyedGod/MaximumCity/actions) [![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/threeeyedgod/MaximumCity/blob/main/LICENSE)
[![Stable Version](https://img.shields.io/github/v/tag/ThreeEyedGod/MaximumCity)](https://img.shields.io/github/v/tag/ThreeEyedGod/MaximumCity)
[![Latest Release](https://img.shields.io/github/v/release/ThreeEyedGod/MaximumCity?color=%233D9970)](https://img.shields.io/github/v/release/ThreeEyedGod/MaximumCity?color=%233D9970)


The idea behind this project is that of an experimental ["serverless" computing service](https://www.protocol.com/newsletters/protocol-enterprise/serverless-container-aws?rebelltitem=1#rebelltitem1), that allows developers to write applications without having to know anything about the cloud hardware on which their applications will run -  often and also  referred to sometimes as "Function-as-a-Service" or Lambda. 


Synopsis
---------

MaximumCity is code built on Haskell. It comprises of a program deployed to AWS lambda, whose workflows are accessible using AWS API Gateway. An effect library is used for Separation of Concerns. The development environment includes a CI pipeline using Github Actions. QuickTest and Hspec for unit testing.  Libraries used include LiquidHaskell (refinement types), Polysemy (effect library), Servant, AWS Lambda runtime, Attoparsec and pdf-toolbox. 

### Functionality  
Provides a telegram interface for a Weather sourcing task which uses caller's IP or location to get weather from a implementation of [DarkSky](https://twitter.com/alexanderrey007/status/1370733643279269889).

## The Program
Takes a place name entered in MaximumCity Bot. Gets latlong for that from positionstack and also grabs your IP address (to the extent telegram provides some details) and determines the weather for that location from public weather sources. 
